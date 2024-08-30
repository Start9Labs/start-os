use std::ops::Deref;
use std::path::PathBuf;
use std::time::Duration;

use clap::builder::ValueParserFactory;
use clap::{value_parser, CommandFactory, FromArgMatches, Parser};
use color_eyre::eyre::eyre;
use exver::VersionRange;
use futures::{AsyncWriteExt, StreamExt};
use imbl_value::{json, InternedString};
use itertools::Itertools;
use models::VersionString;
use reqwest::header::{HeaderMap, CONTENT_LENGTH};
use reqwest::Url;
use rpc_toolkit::yajrc::{GenericRpcMethod, RpcError};
use rpc_toolkit::HandlerArgs;
use rustyline_async::ReadlineEvent;
use serde::{Deserialize, Serialize};
use tokio::sync::oneshot;
use tracing::instrument;
use ts_rs::TS;

use crate::context::{CliContext, RpcContext};
use crate::db::model::package::{ManifestPreference, PackageState, PackageStateMatchModelRef};
use crate::prelude::*;
use crate::progress::{FullProgress, FullProgressTracker, PhasedProgressBar};
use crate::registry::context::{RegistryContext, RegistryUrlParams};
use crate::registry::package::get::GetPackageResponse;
use crate::rpc_continuations::{Guid, RpcContinuation};
use crate::s9pk::manifest::PackageId;
use crate::upload::upload;
use crate::util::clap::FromStrParser;
use crate::util::io::open_file;
use crate::util::net::WebSocketExt;
use crate::util::Never;

pub const PKG_ARCHIVE_DIR: &str = "package-data/archive";
pub const PKG_PUBLIC_DIR: &str = "package-data/public";
pub const PKG_WASM_DIR: &str = "package-data/wasm";

// #[command(display(display_serializable))]
pub async fn list(ctx: RpcContext) -> Result<Vec<Value>, Error> {
    Ok(ctx
        .db
        .peek()
        .await
        .as_public()
        .as_package_data()
        .as_entries()?
        .iter()
        .filter_map(|(id, pde)| {
            let status = match pde.as_state_info().as_match() {
                PackageStateMatchModelRef::Installed(_) => "installed",
                PackageStateMatchModelRef::Installing(_) => "installing",
                PackageStateMatchModelRef::Updating(_) => "updating",
                PackageStateMatchModelRef::Restoring(_) => "restoring",
                PackageStateMatchModelRef::Removing(_) => "removing",
                PackageStateMatchModelRef::Error(_) => "error",
            };
            Some(json!({
                "status": status,
                "id": id.clone(),
                "version": pde.as_state_info()
                    .as_manifest(ManifestPreference::Old)
                    .as_version()
                    .de()
                    .ok()?
            }))
        })
        .collect())
}

#[derive(Debug, Clone, Copy, serde::Deserialize, serde::Serialize, TS)]
#[serde(rename_all = "camelCase")]
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

#[derive(Deserialize, Serialize, TS)]
#[serde(rename_all = "camelCase")]
#[ts(export)]
pub struct InstallParams {
    #[ts(type = "string")]
    registry: Url,
    id: PackageId,
    version: VersionString,
}

#[instrument(skip_all)]
pub async fn install(
    ctx: RpcContext,
    InstallParams {
        registry,
        id,
        version,
    }: InstallParams,
) -> Result<(), Error> {
    let package: GetPackageResponse = from_value(
        ctx.call_remote_with::<RegistryContext, _>(
            "package.get",
            json!({
                "id": id,
                "version": VersionRange::exactly(version.deref().clone()),
            }),
            RegistryUrlParams {
                registry: registry.clone(),
            },
        )
        .await?,
    )?;

    let asset = &package
        .best
        .get(&version)
        .ok_or_else(|| {
            Error::new(
                eyre!("{id}@{version} not found on {registry}"),
                ErrorKind::NotFound,
            )
        })?
        .s9pk;

    let download = ctx
        .services
        .install(
            ctx.clone(),
            || asset.deserialize_s9pk_buffered(ctx.client.clone()),
            None::<Never>,
            None,
        )
        .await?;
    tokio::spawn(async move { download.await?.await });

    Ok(())
}

#[derive(Deserialize, Serialize, TS)]
#[serde(rename_all = "camelCase")]
pub struct SideloadParams {
    #[ts(skip)]
    #[serde(rename = "__auth_session")]
    session: Option<InternedString>,
}

#[derive(Deserialize, Serialize, TS)]
#[serde(rename_all = "camelCase")]
pub struct SideloadResponse {
    pub upload: Guid,
    pub progress: Guid,
}

#[instrument(skip_all)]
pub async fn sideload(
    ctx: RpcContext,
    SideloadParams { session }: SideloadParams,
) -> Result<SideloadResponse, Error> {
    let (upload, file) = upload(&ctx, session.clone()).await?;
    let (err_send, err_recv) = oneshot::channel();
    let progress = Guid::new();
    let progress_tracker = FullProgressTracker::new();
    let mut progress_listener = progress_tracker.stream(Some(Duration::from_millis(200)));
    ctx.rpc_continuations
        .add(
            progress.clone(),
            RpcContinuation::ws_authed(
                &ctx,
                session,
                |mut ws| {
                    use axum::extract::ws::Message;
                    async move {
                        if let Err(e) = async {
                            type RpcResponse = rpc_toolkit::yajrc::RpcResponse::<GenericRpcMethod<&'static str, (), FullProgress>>;
                            tokio::select! {
                                res = async {
                                    while let Some(progress) = progress_listener.next().await {
                                        ws.send(Message::Text(
                                            serde_json::to_string(&RpcResponse::from_result::<RpcError>(Ok(progress)))
                                                .with_kind(ErrorKind::Serialization)?,
                                        ))
                                        .await
                                        .with_kind(ErrorKind::Network)?;
                                    }
                                    Ok::<_, Error>(())
                                } => res?,
                                err = err_recv => {
                                    if let Ok(e) = err {
                                        ws.send(Message::Text(
                                            serde_json::to_string(&RpcResponse::from_result::<RpcError>(Err(e)))
                                            .with_kind(ErrorKind::Serialization)?,
                                        ))
                                        .await
                                        .with_kind(ErrorKind::Network)?;
                                    }
                                }
                            }

                            ws.normal_close("complete").await?;

                            Ok::<_, Error>(())
                        }
                        .await
                        {
                            tracing::error!("Error tracking sideload progress: {e}");
                            tracing::debug!("{e:?}");
                        }
                    }
                },
                Duration::from_secs(600),
            ),
        )
        .await;
    tokio::spawn(async move {
        if let Err(e) = async {
            let key = ctx.db.peek().await.into_private().into_compat_s9pk_key();

            ctx.services
                .install(
                    ctx.clone(),
                    || crate::s9pk::load(file.clone(), || Ok(key.de()?.0), Some(&progress_tracker)),
                    None::<Never>,
                    Some(progress_tracker.clone()),
                )
                .await?
                .await?
                .await?;
            file.delete().await
        }
        .await
        {
            let _ = err_send.send(RpcError::from(e.clone_output()));
            tracing::error!("Error sideloading package: {e}");
            tracing::debug!("{e:?}");
        }
    });
    Ok(SideloadResponse { upload, progress })
}

#[derive(Deserialize, Serialize, Parser)]
pub struct QueryPackageParams {
    id: PackageId,
    version: Option<VersionRange>,
}

#[derive(Deserialize, Serialize)]
#[serde(rename_all = "camelCase")]
pub enum CliInstallParams {
    Marketplace(QueryPackageParams),
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
            .args(
                QueryPackageParams::command()
                    .get_arguments()
                    .cloned()
                    .map(|a| {
                        if a.get_id() == "id" {
                            a.required(false).required_unless_present("sideload")
                        } else {
                            a
                        }
                        .conflicts_with("sideload")
                    }),
            )
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
            Ok(Self::Marketplace(QueryPackageParams::from_arg_matches(
                matches,
            )?))
        }
    }
    fn update_from_arg_matches(&mut self, matches: &clap::ArgMatches) -> Result<(), clap::Error> {
        *self = Self::from_arg_matches(matches)?;
        Ok(())
    }
}

#[derive(Deserialize, Serialize, Parser, TS)]
#[ts(export)]
pub struct InstalledVersionParams {
    id: PackageId,
}

pub async fn installed_version(
    ctx: RpcContext,
    InstalledVersionParams { id }: InstalledVersionParams,
) -> Result<Option<VersionString>, Error> {
    if let Some(pde) = ctx
        .db
        .peek()
        .await
        .into_public()
        .into_package_data()
        .into_idx(&id)
    {
        Ok(Some(
            pde.into_state_info()
                .as_manifest(ManifestPreference::Old)
                .as_version()
                .de()?,
        ))
    } else {
        Ok(None)
    }
}

#[instrument(skip_all)]
pub async fn cli_install(
    HandlerArgs {
        context: ctx,
        parent_method,
        method,
        params,
        ..
    }: HandlerArgs<CliContext, CliInstallParams>,
) -> Result<(), RpcError> {
    let method = parent_method.into_iter().chain(method).collect_vec();
    match params {
        CliInstallParams::Sideload(path) => {
            let file = open_file(path).await?;

            // rpc call remote sideload
            let SideloadResponse { upload, progress } = from_value::<SideloadResponse>(
                ctx.call_remote::<RpcContext>(
                    &method[..method.len() - 1]
                        .into_iter()
                        .chain(std::iter::once(&"sideload"))
                        .join("."),
                    imbl_value::json!({}),
                )
                .await?,
            )?;

            let upload = async {
                let content_length = file.metadata().await?.len();
                ctx.rest_continuation(
                    upload,
                    reqwest::Body::wrap_stream(tokio_util::io::ReaderStream::new(file)),
                    {
                        let mut map = HeaderMap::new();
                        map.insert(CONTENT_LENGTH, content_length.into());
                        map
                    },
                )
                .await?
                .error_for_status()
                .with_kind(ErrorKind::Network)?;
                Ok::<_, Error>(())
            };

            let progress = async {
                use tokio_tungstenite::tungstenite::Message;

                let mut bar = PhasedProgressBar::new("Sideloading");

                let mut ws = ctx.ws_continuation(progress).await?;

                let mut progress = FullProgress::new();

                type RpcResponse = rpc_toolkit::yajrc::RpcResponse<
                    GenericRpcMethod<&'static str, (), FullProgress>,
                >;

                loop {
                    tokio::select! {
                        msg = ws.next() => {
                            if let Some(msg) = msg {
                                if let Message::Text(t) = msg.with_kind(ErrorKind::Network)? {
                                    progress =
                                        serde_json::from_str::<RpcResponse>(&t)
                                            .with_kind(ErrorKind::Deserialization)?.result?;
                                    bar.update(&progress);
                                }
                            } else {
                                break;
                            }
                        }
                        _ = tokio::time::sleep(Duration::from_millis(100)) => {
                            bar.update(&progress);
                        },
                    }
                }

                Ok::<_, Error>(())
            };

            let (upload, progress) = tokio::join!(upload, progress);
            progress?;
            upload?;
        }
        CliInstallParams::Marketplace(QueryPackageParams { id, version }) => {
            let source_version: Option<VersionString> = from_value(
                ctx.call_remote::<RpcContext>("package.installed-version", json!({ "id": &id }))
                    .await?,
            )?;
            let mut packages: GetPackageResponse = from_value(
                ctx.call_remote::<RegistryContext>(
                    "package.get",
                    json!({ "id": &id, "version": version, "sourceVersion": source_version }),
                )
                .await?,
            )?;
            let version = if packages.best.len() == 1 {
                packages.best.pop_first().map(|(k, _)| k).unwrap()
            } else {
                println!("Multiple flavors of {id} found. Please select one of the following versions to install:");
                let version;
                loop {
                    let (mut read, mut output) = rustyline_async::Readline::new("> ".into())
                        .with_kind(ErrorKind::Filesystem)?;
                    for (idx, version) in packages.best.keys().enumerate() {
                        output
                            .write_all(format!("  {}) {}\n", idx + 1, version).as_bytes())
                            .await?;
                        read.add_history_entry(version.to_string());
                    }
                    if let ReadlineEvent::Line(line) = read.readline().await? {
                        let trimmed = line.trim();
                        match trimmed.parse() {
                            Ok(v) => {
                                if let Some((k, _)) = packages.best.remove_entry(&v) {
                                    version = k;
                                    break;
                                }
                            }
                            Err(_) => match trimmed.parse::<usize>() {
                                Ok(i) if (1..=packages.best.len()).contains(&i) => {
                                    version = packages.best.keys().nth(i - 1).unwrap().clone();
                                    break;
                                }
                                _ => (),
                            },
                        }
                        eprintln!("invalid selection: {trimmed}");
                        println!("Please select one of the following versions to install:");
                    } else {
                        return Err(Error::new(
                            eyre!("Could not determine precise version to install"),
                            ErrorKind::InvalidRequest,
                        )
                        .into());
                    }
                }
                version
            };
            ctx.call_remote::<RpcContext>(
                &method.join("."),
                to_value(&InstallParams {
                    id,
                    registry: ctx.registry_url.clone().or_not_found("--registry")?,
                    version,
                })?,
            )
            .await?;
        }
    }
    Ok(())
}

#[derive(Deserialize, Serialize, Parser, TS)]
#[serde(rename_all = "camelCase")]
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
            let entry = db
                .as_public_mut()
                .as_package_data_mut()
                .as_idx_mut(&id)
                .or_not_found(&id)?;
            entry.as_state_info_mut().map_mutate(|s| match s {
                PackageState::Installed(s) => Ok(PackageState::Removing(s)),
                _ => Err(Error::new(
                    eyre!("Package {id} is not installed."),
                    crate::ErrorKind::NotFound,
                )),
            })
        })
        .await?;

    let return_id = id.clone();

    tokio::spawn(async move {
        if let Err(e) = ctx.services.uninstall(&ctx, &id).await {
            tracing::error!("Error uninstalling service {id}: {e}");
            tracing::debug!("{e:?}");
        }
    });

    Ok(return_id)
}
