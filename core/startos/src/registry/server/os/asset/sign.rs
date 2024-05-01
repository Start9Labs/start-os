use std::collections::BTreeMap;
use std::panic::UnwindSafe;
use std::path::PathBuf;

use clap::Parser;
use helpers::NonDetachingJoinHandle;
use imbl_value::InternedString;
use rpc_toolkit::{from_fn_async, CallRemote, HandlerExt, ParentHandler};
use serde::{Deserialize, Serialize};
use ts_rs::TS;

use crate::context::CliContext;
use crate::prelude::*;
use crate::progress::{FullProgressTracker, PhasedProgressBar};
use crate::registry::asset::RegistryAsset;
use crate::registry::server::context::RegistryContext;
use crate::registry::server::os::index::OsVersionInfo;
use crate::registry::server::os::SIG_CONTEXT;
use crate::registry::signer::{Blake3Ed25519Signature, Signature};
use crate::s9pk::merkle_archive::source::ArchiveSource;
use crate::util::Version;

pub fn sign_api() -> ParentHandler {
    ParentHandler::new()
        .root_handler(from_fn_async(cli_sign_asset).no_display())
        .subcommand("iso", from_fn_async(sign_iso).no_cli())
        .subcommand("img", from_fn_async(sign_img).no_cli())
        .subcommand("squashfs", from_fn_async(sign_squashfs).no_cli())
}

#[derive(Debug, Deserialize, Serialize, TS)]
#[serde(rename_all = "camelCase")]
#[ts(export)]
pub struct SignAssetParams {
    #[ts(type = "string")]
    version: Version,
    #[ts(type = "string")]
    platform: InternedString,
    signature: Signature,
}

async fn sign_asset(
    ctx: RegistryContext,
    SignAssetParams {
        version,
        platform,
        signature,
    }: SignAssetParams,
    accessor: impl FnOnce(&mut Model<OsVersionInfo>) -> &mut Model<BTreeMap<InternedString, RegistryAsset>>
        + UnwindSafe
        + Send,
) -> Result<(), Error> {
    ctx.db
        .mutate(|db| {
            let guid = db.as_index().as_signers().get_signer(&signature.signer())?;
            if !db
                .as_index()
                .as_os()
                .as_versions()
                .as_idx(&version)
                .or_not_found(&version)?
                .as_signers()
                .de()?
                .contains(&guid)
            {
                return Err(Error::new(
                    eyre!("signer {guid} is not authorized"),
                    ErrorKind::Authorization,
                ));
            }

            accessor(
                db.as_index_mut()
                    .as_os_mut()
                    .as_versions_mut()
                    .as_idx_mut(&version)
                    .or_not_found(&version)?,
            )
            .as_idx_mut(&platform)
            .or_not_found(&platform)?
            .as_signature_info_mut()
            .mutate(|s| s.add_sig(&signature, SIG_CONTEXT))?;

            Ok(())
        })
        .await
}

pub async fn sign_iso(ctx: RegistryContext, params: SignAssetParams) -> Result<(), Error> {
    sign_asset(ctx, params, |m| m.as_iso_mut()).await
}

pub async fn sign_img(ctx: RegistryContext, params: SignAssetParams) -> Result<(), Error> {
    sign_asset(ctx, params, |m| m.as_img_mut()).await
}

pub async fn sign_squashfs(ctx: RegistryContext, params: SignAssetParams) -> Result<(), Error> {
    sign_asset(ctx, params, |m| m.as_squashfs_mut()).await
}

#[derive(Debug, Deserialize, Serialize, Parser)]
#[command(rename_all = "kebab-case")]
#[serde(rename_all = "camelCase")]
pub struct CliSignAssetParams {
    #[arg(short = 'p', long = "platform")]
    pub platform: InternedString,
    #[arg(short = 'v', long = "version")]
    pub version: Version,
    pub file: PathBuf,
}

pub async fn cli_sign_asset(
    ctx: CliContext,
    CliSignAssetParams {
        platform,
        version,
        file: path,
    }: CliSignAssetParams,
) -> Result<(), Error> {
    let ext = match path.extension().and_then(|e| e.to_str()) {
        Some("iso") => "iso",
        Some("img") => "img",
        Some("squashfs") => "squashfs",
        _ => {
            return Err(Error::new(
                eyre!("Unknown extension"),
                ErrorKind::InvalidRequest,
            ))
        }
    };

    let file = tokio::fs::File::open(&path).await?.into();

    let mut progress = FullProgressTracker::new();
    let progress_handle = progress.handle();
    let mut sign_phase =
        progress_handle.add_phase(InternedString::intern("Signing File"), Some(10));
    let mut index_phase = progress_handle.add_phase(
        InternedString::intern("Adding Signature to Registry Index"),
        Some(1),
    );

    let progress_task: NonDetachingJoinHandle<()> = tokio::spawn(async move {
        let mut bar = PhasedProgressBar::new(&format!("Adding {} to registry...", path.display()));
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

    let blake3_sig =
        Blake3Ed25519Signature::sign_file(ctx.developer_key()?, &file, SIG_CONTEXT).await?;
    let size = blake3_sig.size;
    let signature = Signature::Blake3Ed25519(blake3_sig);
    sign_phase.complete();

    <CliContext as CallRemote<RegistryContext>>::call_remote(
        &ctx,
        &format!("os.asset.sign.{ext}"),
        imbl_value::json!({
            "platform": platform,
            "version": version,
            "signature": signature,
        }),
    )
    .await?;
    index_phase.complete();

    progress_handle.complete();

    progress_task.await.with_kind(ErrorKind::Unknown)?;

    Ok(())
}
