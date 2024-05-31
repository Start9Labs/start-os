use std::path::PathBuf;
use std::sync::Arc;

use clap::Parser;
use helpers::NonDetachingJoinHandle;
use imbl_value::InternedString;
use itertools::Itertools;
use rpc_toolkit::HandlerArgs;
use serde::{Deserialize, Serialize};
use ts_rs::TS;
use url::Url;

use crate::context::CliContext;
use crate::prelude::*;
use crate::progress::{FullProgressTracker, PhasedProgressBar};
use crate::registry::context::RegistryContext;
use crate::registry::package::index::PackageVersionInfo;
use crate::registry::signer::commitment::merkle_archive::MerkleArchiveCommitment;
use crate::registry::signer::sign::ed25519::Ed25519;
use crate::registry::signer::sign::{AnySignature, AnyVerifyingKey, SignatureScheme};
use crate::s9pk::merkle_archive::source::http::HttpSource;
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
        false,
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
    let s9pk = S9pk::open(&file, None, false).await?;

    let mut progress = FullProgressTracker::new();
    let progress_handle = progress.handle();
    let mut sign_phase = progress_handle.add_phase(InternedString::intern("Signing File"), Some(1));
    let mut verify_phase =
        progress_handle.add_phase(InternedString::intern("Verifying URL"), Some(100));
    let mut index_phase = progress_handle.add_phase(
        InternedString::intern("Adding File to Registry Index"),
        Some(1),
    );

    let progress_task: NonDetachingJoinHandle<()> = tokio::spawn(async move {
        let mut bar = PhasedProgressBar::new(&format!("Adding {} to registry...", file.display()));
        loop {
            let snap = progress.snapshot();
            bar.update(&snap);
            if snap.overall.is_complete() {
                break;
            }
            progress.changed().await
        }
    })
    .into();

    sign_phase.start();
    let commitment = s9pk.as_archive().commitment().await?;
    let signature = Ed25519.sign_commitment(ctx.developer_key()?, &commitment, SIG_CONTEXT)?;
    sign_phase.complete();

    verify_phase.start();
    let mut src = S9pk::deserialize(
        &Arc::new(HttpSource::new(ctx.client.clone(), url.clone()).await?),
        Some(&commitment),
        false,
    )
    .await?;
    src.serialize(&mut TrackingIO::new(0, tokio::io::sink()), true)
        .await?;
    verify_phase.complete();

    index_phase.start();
    ctx.call_remote::<RegistryContext>(
        &parent_method.into_iter().chain(method).join("."),
        imbl_value::json!({
            "url": &url,
            "signature": signature,
            "commitment": commitment,
        }),
    )
    .await?;
    index_phase.complete();

    progress_handle.complete();

    progress_task.await.with_kind(ErrorKind::Unknown)?;

    Ok(())
}
