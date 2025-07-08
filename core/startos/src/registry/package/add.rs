use std::path::PathBuf;
use std::sync::Arc;

use clap::Parser;
use imbl_value::InternedString;
use itertools::Itertools;
use models::{PackageId, VersionString};
use rpc_toolkit::HandlerArgs;
use serde::{Deserialize, Serialize};
use ts_rs::TS;
use url::Url;

use crate::context::CliContext;
use crate::prelude::*;
use crate::progress::{FullProgressTracker, ProgressTrackerWriter, ProgressUnits};
use crate::registry::context::RegistryContext;
use crate::registry::package::index::PackageVersionInfo;
use crate::registry::signer::commitment::merkle_archive::MerkleArchiveCommitment;
use crate::registry::signer::sign::ed25519::Ed25519;
use crate::registry::signer::sign::{AnySignature, AnyVerifyingKey, SignatureScheme};
use crate::s9pk::merkle_archive::source::http::HttpSource;
use crate::s9pk::merkle_archive::source::ArchiveSource;
use crate::s9pk::v2::SIG_CONTEXT;
use crate::s9pk::S9pk;
use crate::util::io::TrackingIO;

#[derive(Debug, Deserialize, Serialize, TS)]
#[serde(rename_all = "camelCase")]
#[ts(export)]
pub struct AddPackageParams {
    #[ts(type = "string")]
    pub url: Url,
    #[ts(skip)]
    #[serde(rename = "__auth_signer")]
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
    if !info.s9pk.signatures.contains_key(&uploader) {
        info.s9pk.signatures.insert(uploader.clone(), signature);
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
                    .contains(&uploader_guid)
            {
                let package = db
                    .as_index_mut()
                    .as_package_mut()
                    .as_packages_mut()
                    .upsert(&manifest.id, || Ok(Default::default()))?;
                package.as_versions_mut().insert(&manifest.version, &info)?;

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
    let source = HttpSource::new(ctx.client.clone(), url.clone()).await?;
    let len = source.size().await;
    let mut src = S9pk::deserialize(&Arc::new(source), Some(&commitment)).await?;
    if let Some(len) = len {
        verify_phase.set_total(len);
    }
    verify_phase.set_units(Some(ProgressUnits::Bytes));
    let mut verify_writer = ProgressTrackerWriter::new(tokio::io::sink(), verify_phase);
    src.serialize(&mut TrackingIO::new(0, &mut verify_writer), true)
        .await?;
    let (_, mut verify_phase) = verify_writer.into_inner();
    verify_phase.complete();

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
    #[serde(rename = "__auth_signer")]
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
                    .contains(&signer_guid)
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
