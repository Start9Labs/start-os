use std::path::PathBuf;

use clap::builder::ValueParserFactory;
use clap::Parser;
use color_eyre::eyre::eyre;
use emver::VersionRange;
use futures::StreamExt;
use reqwest::Url;
use rpc_toolkit::yajrc::RpcError;
use rpc_toolkit::CallRemote;
use serde::{Deserialize, Serialize};
use serde_json::{json, Value};
use tracing::instrument;

use crate::context::{CliContext, RpcContext};
use crate::core::rpc_continuations::RequestGuid;
use crate::db::model::{
    PackageDataEntry, PackageDataEntryInstalled, PackageDataEntryMatchModelRef,
    PackageDataEntryRemoving,
};
use crate::notifications::NotificationLevel;
use crate::prelude::*;
use crate::s9pk::manifest::{Manifest, PackageId};
use crate::s9pk::merkle_archive::source::http::HttpSource;
use crate::s9pk::S9pk;
use crate::upload::upload;
use crate::util::clap::FromStrParser;
use crate::util::Never;

pub mod progress;

pub const PKG_ARCHIVE_DIR: &str = "package-data/archive";
pub const PKG_PUBLIC_DIR: &str = "package-data/public";
pub const PKG_WASM_DIR: &str = "package-data/wasm";

// #[command(display(display_serializable))]
pub async fn list(ctx: RpcContext) -> Result<Value, Error> {
    Ok(ctx.db.peek().await.as_package_data().as_entries()?
        .iter()
        .filter_map(|(id, pde)| {
            let status = match pde.as_match() {
                PackageDataEntryMatchModelRef::Installed(_) => {
                    "installed"
                }
                PackageDataEntryMatchModelRef::Installing(_) => {
                    "installing"
                }
                PackageDataEntryMatchModelRef::Updating(_) => {
                    "updating"
                }
                PackageDataEntryMatchModelRef::Restoring(_) => {
                    "restoring"
                }
                PackageDataEntryMatchModelRef::Removing(_) => {
                    "removing"
                }
                PackageDataEntryMatchModelRef::Error(_) => {
                    "error"
                }
            };
            serde_json::to_value(json!({ "status":status, "id": id.clone(), "version": pde.as_manifest().as_version().de().ok()?}))
            .ok()
        })
        .collect())
}

#[derive(Debug, Clone, Copy, serde::Deserialize, serde::Serialize)]
#[serde(rename_all = "kebab-case")]
pub enum MinMax {
    Min,
    Max,
}
impl Default for MinMax {
    fn default() -> Self {
        MinMax::Max
    }
}
impl std::str::FromStr for MinMax {
    type Err = Error;
    fn from_str(s: &str) -> Result<Self, Self::Err> {
        match s {
            "min" => Ok(MinMax::Min),
            "max" => Ok(MinMax::Max),
            _ => Err(Error::new(
                eyre!("Must be one of \"min\", \"max\"."),
                crate::ErrorKind::ParseVersion,
            )),
        }
    }
}
impl ValueParserFactory for MinMax {
    type Parser = FromStrParser<Self>;
    fn value_parser() -> Self::Parser {
        FromStrParser::new()
    }
}
impl std::fmt::Display for MinMax {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            MinMax::Min => write!(f, "min"),
            MinMax::Max => write!(f, "max"),
        }
    }
}

#[derive(Deserialize, Serialize, Parser)]
#[serde(rename_all = "kebab-case")]
#[command(rename_all = "kebab-case")]
pub struct InstallParams {
    id: String,
    #[arg(short = 'm', long = "marketplace-url")]
    marketplace_url: Option<Url>,
    #[arg(short = 'v', long = "version-spec")]
    version_spec: Option<String>,
    #[arg(long = "version-priority")]
    version_priority: Option<MinMax>,
}

// #[command(
//     custom_cli(cli_install(async, context(CliContext))),
// )]
#[instrument(skip_all)]
pub async fn install(
    ctx: RpcContext,
    InstallParams {
        id,
        marketplace_url,
        version_spec,
        version_priority,
    }: InstallParams,
) -> Result<(), Error> {
    let version_str = match &version_spec {
        None => "*",
        Some(v) => &*v,
    };
    let version: VersionRange = version_str.parse()?;
    let marketplace_url =
        marketplace_url.unwrap_or_else(|| crate::DEFAULT_MARKETPLACE.parse().unwrap());
    let version_priority = version_priority.unwrap_or_default();
    let s9pk = S9pk::deserialize(
        &HttpSource::new(
            ctx.client.clone(),
            format!(
                "{}/package/v0/{}.s9pk?spec={}&version-priority={}",
                marketplace_url, id, version, version_priority,
            )
            .parse()?,
        )
        .await?,
    )
    .await?;

    let download = ctx
        .services
        .install(ctx.clone(), s9pk, None::<Never>)
        .await?;
    tokio::spawn(async move { download.await?.await });

    Ok(())
}

#[derive(Deserialize, Serialize)]
#[serde(rename_all = "kebab-case")]
pub struct SideloadParams {
    manifest: Manifest,
    icon: Option<String>,
}

#[instrument(skip_all)]
pub async fn sideload(ctx: RpcContext) -> Result<RequestGuid, Error> {
    let (guid, file) = upload(&ctx).await?;
    let download = ctx
        .services
        .install(ctx.clone(), S9pk::deserialize(&file).await?, None::<Never>)
        .await?;
    tokio::spawn(async { download.await?.await });
    Ok(guid)
}

#[instrument(skip_all)]
async fn cli_install(
    ctx: CliContext,
    target: String,
    marketplace_url: Option<Url>,
    version_spec: Option<String>,
    version_priority: Option<MinMax>,
) -> Result<(), RpcError> {
    if target.ends_with(".s9pk") {
        let path = PathBuf::from(target);

        // TODO: validate s9pk

        // rpc call remote sideload
        tracing::debug!("calling package.sideload");
        let guid = from_value::<RequestGuid>(
            ctx.call_remote("package.sideload", imbl_value::json!({}))
                .await?,
        )?;
        tracing::debug!("package.sideload succeeded {:?}", guid);

        // hit continuation api with guid that comes back
        let file = tokio::fs::File::open(path).await?;
        let content_length = file.metadata().await?.len();
        let body = reqwest::Body::wrap_stream(tokio_util::io::ReaderStream::new(file));
        let res = ctx
            .client
            .post(format!("{}rest/rpc/{}", ctx.base_url, guid,))
            .header(reqwest::header::CONTENT_LENGTH, content_length)
            .body(body)
            .send()
            .await?;
        if res.status().as_u16() == 200 {
            tracing::info!("Package Uploaded")
        } else {
            tracing::info!("Package Upload failed: {}", res.text().await?)
        }
    } else {
        let params = match (target.split_once("@"), version_spec) {
            (Some((pkg, v)), None) => {
                imbl_value::json!({ "id": pkg, "marketplace-url": marketplace_url, "version-spec": v, "version-priority": version_priority })
            }
            (Some(_), Some(_)) => {
                return Err(crate::Error::new(
                    eyre!("Invalid package id {}", target),
                    ErrorKind::InvalidRequest,
                )
                .into())
            }
            (None, Some(v)) => {
                imbl_value::json!({ "id": target, "marketplace-url": marketplace_url, "version-spec": v, "version-priority": version_priority })
            }
            (None, None) => {
                imbl_value::json!({ "id": target, "marketplace-url": marketplace_url, "version-priority": version_priority })
            }
        };
        tracing::debug!("calling package.install");
        ctx.call_remote("package.install", params).await?;
        tracing::debug!("package.install succeeded");
    }
    Ok(())
}

#[derive(Deserialize, Serialize, Parser)]
#[serde(rename_all = "kebab-case")]
#[command(rename_all = "kebab-case")]
pub struct UninstallParams {
    id: PackageId,
}

pub async fn uninstall(
    ctx: RpcContext,
    UninstallParams { id }: UninstallParams,
) -> Result<PackageId, Error> {
    ctx.db
        .mutate(|db| {
            let (manifest, static_files, installed) =
                match db.as_package_data().as_idx(&id).or_not_found(&id)?.de()? {
                    PackageDataEntry::Installed(PackageDataEntryInstalled {
                        manifest,
                        static_files,
                        installed,
                    }) => (manifest, static_files, installed),
                    _ => {
                        return Err(Error::new(
                            eyre!("Package is not installed."),
                            crate::ErrorKind::NotFound,
                        ));
                    }
                };
            let pde = PackageDataEntry::Removing(PackageDataEntryRemoving {
                manifest,
                static_files,
                removing: installed,
            });
            db.as_package_data_mut().insert(&id, &pde)
        })
        .await?;

    let return_id = id.clone();

    tokio::spawn(async move {
        if let Err(e) = ctx.services.uninstall(&id).await {
            let err_str = format!("Uninstall of {} Failed: {}", id, e);
            tracing::error!("{}", err_str);
            tracing::debug!("{:?}", e);
            if let Err(e) = ctx
                .notification_manager
                .notify(
                    ctx.db.clone(), // allocating separate handle here because the lifetime of the previous one is the expression
                    Some(id),
                    NotificationLevel::Error,
                    String::from("Uninstall Failed"),
                    err_str,
                    (),
                    None,
                )
                .await
            {
                tracing::error!("Failed to issue Notification: {}", e);
                tracing::debug!("{:?}", e);
            }
        }
    });

    Ok(return_id)
}
