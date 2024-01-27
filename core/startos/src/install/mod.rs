use std::io::SeekFrom;
use std::path::PathBuf;

use clap::builder::ValueParserFactory;
use clap::{value_parser, CommandFactory, FromArgMatches, Parser};
use color_eyre::eyre::eyre;
use emver::VersionRange;
use reqwest::header::{HeaderMap, CONTENT_LENGTH};
use reqwest::Url;
use rpc_toolkit::yajrc::RpcError;
use rpc_toolkit::CallRemote;
use serde::{Deserialize, Serialize};
use serde_json::{json, Value};
use tokio::io::{AsyncReadExt, AsyncSeekExt};
use tracing::instrument;

use crate::context::{CliContext, RpcContext};
use crate::core::rpc_continuations::RequestGuid;
use crate::db::model::{
    PackageDataEntry, PackageDataEntryInstalled, PackageDataEntryMatchModelRef,
    PackageDataEntryRemoving,
};
use crate::prelude::*;
use crate::s9pk::manifest::PackageId;
use crate::s9pk::merkle_archive::source::http::HttpSource;
use crate::s9pk::v1::reader::S9pkReader;
use crate::s9pk::v2::compat::{self, MAGIC_AND_VERSION};
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
    id: PackageId,
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

    ensure_code!(
        &s9pk.as_manifest().id == &id,
        ErrorKind::ValidateS9pk,
        "manifest.id does not match expected"
    );

    let download = ctx
        .services
        .install(ctx.clone(), s9pk, None::<Never>)
        .await?;
    tokio::spawn(async move { download.await?.await });

    Ok(())
}

#[instrument(skip_all)]
pub async fn sideload(ctx: RpcContext) -> Result<RequestGuid, Error> {
    let (guid, file) = upload(&ctx).await?;
    tokio::spawn(async move {
        if let Err(e) = async {
            ctx.services
                .install(ctx.clone(), S9pk::deserialize(&file).await?, None::<Never>)
                .await?
                .await?
                .await?;
            file.delete().await
        }
        .await
        {
            tracing::error!("Error sideloading package: {e}");
            tracing::debug!("{e:?}");
        }
    });
    Ok(guid)
}

#[derive(Deserialize, Serialize)]
#[serde(rename_all = "kebab-case")]
pub enum CliInstallParams {
    Marketplace(InstallParams),
    Sideload(PathBuf),
}
impl CommandFactory for CliInstallParams {
    fn command() -> clap::Command {
        use clap::{Arg, Command};
        Command::new("install")
            .arg(
                Arg::new("sideload")
                    .long("sideload")
                    .short('s')
                    .required_unless_present("id")
                    .value_parser(value_parser!(PathBuf)),
            )
            .args(InstallParams::command().get_arguments().cloned().map(|a| {
                if a.get_id() == "id" {
                    a.required(false).required_unless_present("sideload")
                } else {
                    a
                }
                .conflicts_with("sideload")
            }))
    }
    fn command_for_update() -> clap::Command {
        Self::command()
    }
}
impl FromArgMatches for CliInstallParams {
    fn from_arg_matches(matches: &clap::ArgMatches) -> Result<Self, clap::Error> {
        if let Some(sideload) = matches.get_one::<PathBuf>("sideload") {
            Ok(Self::Sideload(sideload.clone()))
        } else {
            Ok(Self::Marketplace(InstallParams::from_arg_matches(matches)?))
        }
    }
    fn update_from_arg_matches(&mut self, matches: &clap::ArgMatches) -> Result<(), clap::Error> {
        *self = Self::from_arg_matches(matches)?;
        Ok(())
    }
}

#[instrument(skip_all)]
pub async fn cli_install(ctx: CliContext, params: CliInstallParams) -> Result<(), RpcError> {
    match params {
        CliInstallParams::Sideload(path) => {
            const MAGIC_LEN: usize = MAGIC_AND_VERSION.len();
            let mut magic = [0_u8; MAGIC_LEN];
            let mut file = tokio::fs::File::open(&path).await?;
            file.read_exact(&mut magic).await?;
            file.seek(SeekFrom::Start(0)).await?;
            if magic == compat::MAGIC_AND_VERSION {
                tracing::info!("Converting package to v2 s9pk");
                let new_path = path.with_extension("compat.s9pk");
                S9pk::from_v1(
                    S9pkReader::from_reader(file, true).await?,
                    &new_path,
                    ctx.developer_key()?,
                )
                .await?;
                tokio::fs::rename(&new_path, &path).await?;
                file = tokio::fs::File::open(&path).await?;
            }

            // rpc call remote sideload
            tracing::debug!("calling package.sideload");
            let guid = from_value::<RequestGuid>(
                ctx.call_remote("package.sideload", imbl_value::json!({}))
                    .await?,
            )?;
            tracing::debug!("package.sideload succeeded {:?}", guid);

            // hit continuation api with guid that comes back
            let content_length = file.metadata().await?.len();
            let res = ctx
                .rest_continuation(
                    guid,
                    reqwest::Body::wrap_stream(tokio_util::io::ReaderStream::new(file)),
                    {
                        let mut map = HeaderMap::new();
                        map.insert(CONTENT_LENGTH, content_length.into());
                        map
                    },
                )
                .await?;
            if res.status().is_success() {
                tracing::info!("Package Uploaded")
            } else {
                tracing::error!(
                    "Package Upload failed: {} {}",
                    res.status().as_str(),
                    res.text().await?
                )
            }
        }
        CliInstallParams::Marketplace(params) => {
            tracing::debug!("calling package.install");
            ctx.call_remote("package.install", to_value(&params)?)
                .await?;
            tracing::debug!("package.install succeeded");
        }
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

    tokio::spawn(async move { ctx.services.uninstall(&ctx, &id).await });

    Ok(return_id)
}
