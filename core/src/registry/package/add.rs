use std::path::PathBuf;
use std::sync::Arc;

use clap::Parser;
use imbl_value::InternedString;
use itertools::Itertools;
use rpc_toolkit::HandlerArgs;
use serde::{Deserialize, Serialize};
use ts_rs::TS;
use url::Url;

use crate::PackageId;
use crate::context::CliContext;
use crate::prelude::*;
use crate::progress::FullProgressTracker;
use crate::registry::asset::BufferedHttpSource;
use crate::registry::context::RegistryContext;
use crate::registry::package::index::PackageVersionInfo;
use crate::s9pk::S9pk;
use crate::s9pk::merkle_archive::source::http::HttpSource;
use crate::s9pk::v2::SIG_CONTEXT;
use crate::sign::commitment::merkle_archive::MerkleArchiveCommitment;
use crate::sign::ed25519::Ed25519;
use crate::sign::{AnySignature, AnyVerifyingKey, SignatureScheme};
use crate::util::VersionString;
use crate::util::io::TrackingIO;

#[derive(Debug, Deserialize, Serialize, TS)]
#[serde(rename_all = "camelCase")]
#[ts(export)]
pub struct AddPackageParams {
    #[ts(type = "string")]
    pub url: Url,
    #[ts(skip)]
    #[serde(rename = "__Auth_signer")]
    pub uploader: AnyVerifyingKey,
    pub commitment: MerkleArchiveCommitment,
    pub signature: AnySignature,
}

pub async fn add_package(
    ctx: RegistryContext,
    AddPackageParams {
        url,
        uploader,
        commitment,
        signature,
    }: AddPackageParams,
) -> Result<(), Error> {
    uploader
        .scheme()
        .verify_commitment(&uploader, &commitment, SIG_CONTEXT, &signature)?;
    let peek = ctx.db.peek().await;
    let uploader_guid = peek.as_index().as_signers().get_signer(&uploader)?;
    let s9pk = S9pk::deserialize(
        &Arc::new(HttpSource::new(ctx.client.clone(), url.clone()).await?),
        Some(&commitment),
    )
    .await?;

    let manifest = s9pk.as_manifest();

    let mut info = PackageVersionInfo::from_s9pk(&s9pk, url).await?;
    for (_, s9pk) in &mut info.s9pks {
        if !s9pk.signatures.contains_key(&uploader) && s9pk.commitment == commitment {
            s9pk.signatures.insert(uploader.clone(), signature.clone());
        }
    }

    ctx.db
        .mutate(|db| {
            if db.as_admins().de()?.contains(&uploader_guid)
                || db
                    .as_index()
                    .as_package()
                    .as_packages()
                    .as_idx(&manifest.id)
                    .or_not_found(&manifest.id)?
                    .as_authorized()
                    .de()?
                    .get(&uploader_guid)
                    .map_or(false, |v| manifest.version.satisfies(v))
            {
                let package = db
                    .as_index_mut()
                    .as_package_mut()
                    .as_packages_mut()
                    .upsert(&manifest.id, || Ok(Default::default()))?;
                let v = package.as_versions_mut();
                if let Some(prev) = v.as_idx_mut(&manifest.version) {
                    prev.mutate(|p| p.merge_with(info))?;
                } else {
                    v.insert(&manifest.version, &info)?;
                }

                Ok(())
            } else {
                Err(Error::new(eyre!("UNAUTHORIZED"), ErrorKind::Authorization))
            }
        })
        .await
        .result
}

#[derive(Debug, Deserialize, Serialize, Parser)]
#[command(rename_all = "kebab-case")]
#[serde(rename_all = "camelCase")]
pub struct CliAddPackageParams {
    pub file: PathBuf,
    pub url: Url,
}

pub async fn cli_add_package(
    HandlerArgs {
        context: ctx,
        parent_method,
        method,
        params: CliAddPackageParams { file, url },
        ..
    }: HandlerArgs<CliContext, CliAddPackageParams>,
) -> Result<(), Error> {
    let s9pk = S9pk::open(&file, None).await?;

    let progress = FullProgressTracker::new();
    let mut sign_phase = progress.add_phase(InternedString::intern("Signing File"), Some(1));
    let mut verify_phase = progress.add_phase(InternedString::intern("Verifying URL"), Some(100));
    let mut index_phase = progress.add_phase(
        InternedString::intern("Adding File to Registry Index"),
        Some(1),
    );

    let progress_task =
        progress.progress_bar_task(&format!("Adding {} to registry...", file.display()));

    sign_phase.start();
    let commitment = s9pk.as_archive().commitment().await?;
    let signature = Ed25519.sign_commitment(ctx.developer_key()?, &commitment, SIG_CONTEXT)?;
    sign_phase.complete();

    verify_phase.start();
    let source = BufferedHttpSource::new(ctx.client.clone(), url.clone(), verify_phase).await?;
    let mut src = S9pk::deserialize(&Arc::new(source), Some(&commitment)).await?;
    src.serialize(&mut TrackingIO::new(0, &mut tokio::io::sink()), true)
        .await?;

    index_phase.start();
    ctx.call_remote::<RegistryContext>(
        &parent_method.into_iter().chain(method).join("."),
        imbl_value::json!({
            "url": &url,
            "signature": AnySignature::Ed25519(signature),
            "commitment": commitment,
        }),
    )
    .await?;
    index_phase.complete();

    progress.complete();

    progress_task.await.with_kind(ErrorKind::Unknown)?;

    Ok(())
}

#[derive(Debug, Deserialize, Serialize, Parser, TS)]
#[serde(rename_all = "camelCase")]
#[ts(export)]
pub struct RemovePackageParams {
    pub id: PackageId,
    pub version: VersionString,
    #[ts(skip)]
    #[arg(skip)]
    #[serde(rename = "__Auth_signer")]
    pub signer: Option<AnyVerifyingKey>,
}

pub async fn remove_package(
    ctx: RegistryContext,
    RemovePackageParams {
        id,
        version,
        signer,
    }: RemovePackageParams,
) -> Result<(), Error> {
    let peek = ctx.db.peek().await;
    let signer =
        signer.ok_or_else(|| Error::new(eyre!("missing signer"), ErrorKind::InvalidRequest))?;
    let signer_guid = peek.as_index().as_signers().get_signer(&signer)?;

    ctx.db
        .mutate(|db| {
            if db.as_admins().de()?.contains(&signer_guid)
                || db
                    .as_index()
                    .as_package()
                    .as_packages()
                    .as_idx(&id)
                    .or_not_found(&id)?
                    .as_authorized()
                    .de()?
                    .get(&signer_guid)
                    .map_or(false, |v| version.satisfies(v))
            {
                if let Some(package) = db
                    .as_index_mut()
                    .as_package_mut()
                    .as_packages_mut()
                    .as_idx_mut(&id)
                {
                    package.as_versions_mut().remove(&version)?;
                }
                Ok(())
            } else {
                Err(Error::new(eyre!("UNAUTHORIZED"), ErrorKind::Authorization))
            }
        })
        .await
        .result
}
