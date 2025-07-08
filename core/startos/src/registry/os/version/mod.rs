use std::collections::BTreeMap;

use chrono::Utc;
use clap::Parser;
use exver::{Version, VersionRange};
use imbl_value::InternedString;
use itertools::Itertools;
use rpc_toolkit::{from_fn_async, Context, HandlerExt, ParentHandler};
use serde::{Deserialize, Serialize};
use sqlx::query;
use ts_rs::TS;

use crate::context::CliContext;
use crate::prelude::*;
use crate::registry::context::RegistryContext;
use crate::registry::device_info::DeviceInfo;
use crate::registry::os::index::OsVersionInfo;
use crate::registry::signer::sign::AnyVerifyingKey;
use crate::util::serde::{display_serializable, HandlerExtSerde, WithIoFormat};

pub mod signer;

pub fn version_api<C: Context>() -> ParentHandler<C> {
    ParentHandler::new()
        .subcommand(
            "add",
            from_fn_async(add_version)
                .with_metadata("admin", Value::Bool(true))
                .with_metadata("get_signer", Value::Bool(true))
                .no_display()
                .with_about("Add OS version")
                .with_call_remote::<CliContext>(),
        )
        .subcommand(
            "remove",
            from_fn_async(remove_version)
                .with_metadata("admin", Value::Bool(true))
                .no_display()
                .with_about("Remove OS version")
                .with_call_remote::<CliContext>(),
        )
        .subcommand(
            "signer",
            signer::signer_api::<C>().with_about("Add, remove, and list version signers"),
        )
        .subcommand(
            "get",
            from_fn_async(get_version)
                .with_metadata("get_device_info", Value::Bool(true))
                .with_display_serializable()
                .with_custom_display_fn(|handle, result| {
                    display_version_info(handle.params, result)
                })
                .with_about("Get OS versions and related version info")
                .with_call_remote::<CliContext>(),
        )
}

#[derive(Debug, Deserialize, Serialize, Parser, TS)]
#[command(rename_all = "kebab-case")]
#[serde(rename_all = "camelCase")]
#[ts(export)]
pub struct AddVersionParams {
    #[ts(type = "string")]
    pub version: Version,
    pub headline: String,
    pub release_notes: String,
    #[ts(type = "string")]
    pub source_version: VersionRange,
    #[arg(skip)]
    #[ts(skip)]
    #[serde(rename = "__auth_signer")]
    pub signer: Option<AnyVerifyingKey>,
}

pub async fn add_version(
    ctx: RegistryContext,
    AddVersionParams {
        version,
        headline,
        release_notes,
        source_version,
        signer,
    }: AddVersionParams,
) -> Result<(), Error> {
    ctx.db
        .mutate(|db| {
            let signer = signer
                .map(|s| db.as_index().as_signers().get_signer(&s))
                .transpose()?;
            db.as_index_mut()
                .as_os_mut()
                .as_versions_mut()
                .upsert(&version, || Ok(OsVersionInfo::default()))?
                .mutate(|i| {
                    i.headline = headline;
                    i.release_notes = release_notes;
                    i.source_version = source_version;
                    i.authorized.extend(signer);
                    Ok(())
                })
        })
        .await
        .result
}

#[derive(Debug, Deserialize, Serialize, Parser, TS)]
#[command(rename_all = "kebab-case")]
#[serde(rename_all = "camelCase")]
#[ts(export)]
pub struct RemoveVersionParams {
    #[ts(type = "string")]
    pub version: Version,
}

pub async fn remove_version(
    ctx: RegistryContext,
    RemoveVersionParams { version }: RemoveVersionParams,
) -> Result<(), Error> {
    ctx.db
        .mutate(|db| {
            db.as_index_mut()
                .as_os_mut()
                .as_versions_mut()
                .remove(&version)?;
            Ok(())
        })
        .await
        .result
}

#[derive(Debug, Deserialize, Serialize, Parser, TS)]
#[command(rename_all = "kebab-case")]
#[serde(rename_all = "camelCase")]
#[ts(export)]
pub struct GetOsVersionParams {
    #[ts(type = "string | null")]
    #[arg(long = "src")]
    pub source_version: Option<Version>,
    #[ts(type = "string | null")]
    #[arg(long)]
    pub target_version: Option<VersionRange>,
    #[arg(long = "id")]
    server_id: Option<String>,
    #[ts(type = "string | null")]
    #[arg(long)]
    platform: Option<InternedString>,
    #[ts(skip)]
    #[arg(skip)]
    #[serde(rename = "__device_info")]
    pub device_info: Option<DeviceInfo>,
}

pub async fn get_version(
    ctx: RegistryContext,
    GetOsVersionParams {
        source_version: source,
        target_version: target,
        server_id,
        platform,
        device_info,
    }: GetOsVersionParams,
) -> Result<BTreeMap<Version, OsVersionInfo>, Error> {
    let source = source.or_else(|| device_info.as_ref().map(|d| d.os.version.clone()));
    let platform = platform.or_else(|| device_info.as_ref().map(|d| d.os.platform.clone()));
    if let (Some(pool), Some(server_id), Some(arch)) = (&ctx.pool, server_id, &platform) {
        let created_at = Utc::now();

        query!(
            "INSERT INTO user_activity (created_at, server_id, arch) VALUES ($1, $2, $3)",
            created_at,
            server_id,
            &**arch
        )
        .execute(pool)
        .await?;
    }
    let target = target.unwrap_or(VersionRange::Any);
    ctx.db
        .peek()
        .await
        .into_index()
        .into_os()
        .into_versions()
        .into_entries()?
        .into_iter()
        .map(|(v, i)| i.de().map(|i| (v, i)))
        .filter_ok(|(version, info)| {
            platform
                .as_ref()
                .map_or(true, |p| info.squashfs.contains_key(p))
                && version.satisfies(&target)
                && source
                    .as_ref()
                    .map_or(true, |s| s.satisfies(&info.source_version))
        })
        .collect()
}

pub fn display_version_info<T>(
    params: WithIoFormat<T>,
    info: BTreeMap<Version, OsVersionInfo>,
) -> Result<(), Error> {
    use prettytable::*;

    if let Some(format) = params.format {
        return display_serializable(format, info);
    }

    let mut table = Table::new();
    table.add_row(row![bc =>
        "VERSION",
        "HEADLINE",
        "RELEASE NOTES",
        "ISO PLATFORMS",
        "IMG PLATFORMS",
        "SQUASHFS PLATFORMS",
    ]);
    for (version, info) in &info {
        table.add_row(row![
            &version.to_string(),
            &info.headline,
            &info.release_notes,
            &info.iso.keys().into_iter().join(", "),
            &info.img.keys().into_iter().join(", "),
            &info.squashfs.keys().into_iter().join(", "),
        ]);
    }
    table.print_tty(false)?;
    Ok(())
}
