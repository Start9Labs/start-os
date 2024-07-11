use std::collections::BTreeMap;

use chrono::Utc;
use clap::Parser;
use exver::VersionRange;
use itertools::Itertools;
use rpc_toolkit::{from_fn_async, Context, HandlerExt, ParentHandler};
use serde::{Deserialize, Serialize};
use sqlx::query;
use ts_rs::TS;

use crate::context::CliContext;
use crate::prelude::*;
use crate::registry::context::RegistryContext;
use crate::registry::os::index::OsVersionInfo;
use crate::registry::signer::sign::AnyVerifyingKey;
use crate::util::serde::{display_serializable, HandlerExtSerde, WithIoFormat};
use crate::util::VersionString;

pub mod signer;

pub fn version_api<C: Context>() -> ParentHandler<C> {
    ParentHandler::new()
        .subcommand(
            "add",
            from_fn_async(add_version)
                .with_metadata("admin", Value::Bool(true))
                .with_metadata("getSigner", Value::Bool(true))
                .no_display()
                .with_call_remote::<CliContext>(),
        )
        .subcommand(
            "remove",
            from_fn_async(remove_version)
                .with_metadata("admin", Value::Bool(true))
                .no_display()
                .with_call_remote::<CliContext>(),
        )
        .subcommand("signer", signer::signer_api::<C>())
        .subcommand(
            "get",
            from_fn_async(get_version)
                .with_display_serializable()
                .with_custom_display_fn(|handle, result| {
                    Ok(display_version_info(handle.params, result))
                })
                .with_call_remote::<CliContext>(),
        )
}

#[derive(Debug, Deserialize, Serialize, Parser, TS)]
#[command(rename_all = "kebab-case")]
#[serde(rename_all = "camelCase")]
#[ts(export)]
pub struct AddVersionParams {
    pub version: VersionString,
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
}

#[derive(Debug, Deserialize, Serialize, Parser, TS)]
#[command(rename_all = "kebab-case")]
#[serde(rename_all = "camelCase")]
#[ts(export)]
pub struct RemoveVersionParams {
    pub version: VersionString,
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
}

#[derive(Debug, Deserialize, Serialize, Parser, TS)]
#[command(rename_all = "kebab-case")]
#[serde(rename_all = "camelCase")]
#[ts(export)]
pub struct GetOsVersionParams {
    #[ts(type = "string | null")]
    #[arg(long = "src")]
    pub source: Option<VersionString>,
    #[ts(type = "string | null")]
    #[arg(long = "target")]
    pub target: Option<VersionRange>,
    #[ts(type = "string | null")]
    #[arg(long = "id")]
    server_id: Option<String>,
    #[ts(type = "string | null")]
    #[arg(long = "arch")]
    arch: Option<String>,
}

pub async fn get_version(
    ctx: RegistryContext,
    GetOsVersionParams {
        source,
        target,
        server_id,
        arch,
    }: GetOsVersionParams,
) -> Result<BTreeMap<VersionString, OsVersionInfo>, Error> {
    if let (Some(pool), Some(server_id), Some(arch)) = (&ctx.pool, server_id, arch) {
        let created_at = Utc::now();

        query!(
            "INSERT INTO user_activity (created_at, server_id, arch) VALUES ($1, $2, $3)",
            created_at,
            server_id,
            arch
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
            version.satisfies(&target)
                && source
                    .as_ref()
                    .map_or(true, |s| s.satisfies(&info.source_version))
        })
        .collect()
}

pub fn display_version_info<T>(
    params: WithIoFormat<T>,
    info: BTreeMap<VersionString, OsVersionInfo>,
) {
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
            version.as_str(),
            &info.headline,
            &info.release_notes,
            &info.iso.keys().into_iter().join(", "),
            &info.img.keys().into_iter().join(", "),
            &info.squashfs.keys().into_iter().join(", "),
        ]);
    }
    table.print_tty(false).unwrap();
}
