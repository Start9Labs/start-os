use std::collections::BTreeMap;
use std::panic::UnwindSafe;
use std::path::{Path, PathBuf};

use clap::Parser;
use helpers::{AtomicFile, NonDetachingJoinHandle};
use imbl_value::{json, InternedString};
use itertools::Itertools;
use rpc_toolkit::{from_fn_async, Context, HandlerArgs, HandlerExt, ParentHandler};
use serde::{Deserialize, Serialize};
use ts_rs::TS;

use crate::context::CliContext;
use crate::prelude::*;
use crate::progress::{FullProgressTracker, PhasedProgressBar};
use crate::registry::asset::RegistryAsset;
use crate::registry::context::RegistryContext;
use crate::registry::os::index::OsVersionInfo;
use crate::util::Version;

pub fn get_api<C: Context>() -> ParentHandler<C> {
    ParentHandler::new()
        .subcommand("iso", from_fn_async(get_iso).no_cli())
        .subcommand("iso", from_fn_async(cli_get_os_asset).no_display())
        .subcommand("img", from_fn_async(get_img).no_cli())
        .subcommand("img", from_fn_async(cli_get_os_asset).no_display())
        .subcommand("squashfs", from_fn_async(get_squashfs).no_cli())
        .subcommand("squashfs", from_fn_async(cli_get_os_asset).no_display())
}

#[derive(Debug, Deserialize, Serialize, TS)]
#[serde(rename_all = "camelCase")]
#[ts(export)]
pub struct GetOsAssetParams {
    #[ts(type = "string")]
    pub version: Version,
    #[ts(type = "string")]
    pub platform: InternedString,
}

async fn get_os_asset(
    ctx: RegistryContext,
    GetOsAssetParams { version, platform }: GetOsAssetParams,
    accessor: impl FnOnce(&Model<OsVersionInfo>) -> &Model<BTreeMap<InternedString, RegistryAsset>>
        + UnwindSafe
        + Send,
) -> Result<RegistryAsset, Error> {
    accessor(
        ctx.db
            .peek()
            .await
            .as_index()
            .as_os()
            .as_versions()
            .as_idx(&version)
            .or_not_found(&version)?,
    )
    .as_idx(&platform)
    .or_not_found(&platform)?
    .de()
}

pub async fn get_iso(
    ctx: RegistryContext,
    params: GetOsAssetParams,
) -> Result<RegistryAsset, Error> {
    get_os_asset(ctx, params, |info| info.as_iso()).await
}

pub async fn get_img(
    ctx: RegistryContext,
    params: GetOsAssetParams,
) -> Result<RegistryAsset, Error> {
    get_os_asset(ctx, params, |info| info.as_img()).await
}

pub async fn get_squashfs(
    ctx: RegistryContext,
    params: GetOsAssetParams,
) -> Result<RegistryAsset, Error> {
    get_os_asset(ctx, params, |info| info.as_squashfs()).await
}

#[derive(Debug, Deserialize, Serialize, Parser)]
#[command(rename_all = "kebab-case")]
#[serde(rename_all = "camelCase")]
pub struct CliGetOsAssetParams {
    pub version: Version,
    pub platform: InternedString,
    #[arg(long = "download", short = 'd')]
    pub download: Option<PathBuf>,
    #[arg(
        long = "reverify",
        short = 'r',
        help = "verify the hash of the file a second time after download"
    )]
    pub reverify: bool,
}

async fn cli_get_os_asset(
    HandlerArgs {
        context: ctx,
        parent_method,
        method,
        params:
            CliGetOsAssetParams {
                version,
                platform,
                download,
                reverify,
            },
        ..
    }: HandlerArgs<CliContext, CliGetOsAssetParams>,
) -> Result<RegistryAsset, Error> {
    let res = from_value::<RegistryAsset>(
        ctx.call_remote::<RegistryContext>(
            &parent_method.into_iter().chain(method).join("."),
            json!({
                "version": version,
                "platform": platform,
            }),
        )
        .await?,
    )?;

    let validator = res.validate(res.signature_info.all_signers())?;

    if let Some(download) = download {
        let mut file = AtomicFile::new(&download, None::<&Path>)
            .await
            .with_kind(ErrorKind::Filesystem)?;

        let mut progress = FullProgressTracker::new();
        let progress_handle = progress.handle();
        let mut download_phase =
            progress_handle.add_phase(InternedString::intern("Downloading File"), Some(100));
        download_phase.set_total(validator.size()?);
        let reverify_phase = if reverify {
            Some(progress_handle.add_phase(InternedString::intern("Reverifying File"), Some(10)))
        } else {
            None
        };

        let progress_task: NonDetachingJoinHandle<()> = tokio::spawn(async move {
            let mut bar = PhasedProgressBar::new("Downloading...");
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

        download_phase.start();
        let mut download_writer = download_phase.writer(&mut *file);
        res.download(ctx.client.clone(), &mut download_writer, &validator)
            .await?;
        let (_, mut download_phase) = download_writer.into_inner();
        file.save().await.with_kind(ErrorKind::Filesystem)?;
        download_phase.complete();

        if let Some(mut reverify_phase) = reverify_phase {
            reverify_phase.start();
            validator.validate_file(&download).await?;
            reverify_phase.complete();
        }

        progress_handle.complete();

        progress_task.await.with_kind(ErrorKind::Unknown)?;
    }

    Ok(res)
}
