use std::path::PathBuf;
use std::time::Duration;

use clap::builder::ValueParserFactory;
use clap::{value_parser, CommandFactory, FromArgMatches, Parser};
use color_eyre::eyre::eyre;
use emver::VersionRange;
use futures::{FutureExt, StreamExt};
use itertools::Itertools;
use patch_db::json_ptr::JsonPointer;
use reqwest::header::{HeaderMap, CONTENT_LENGTH};
use reqwest::Url;
use rpc_toolkit::yajrc::RpcError;
use rpc_toolkit::HandlerArgs;
use serde::{Deserialize, Serialize};
use serde_json::{json, Value};
use tokio::sync::oneshot;
use tracing::instrument;
use ts_rs::TS;

use crate::context::{CliContext, RpcContext};
use crate::db::model::package::{ManifestPreference, PackageState, PackageStateMatchModelRef};
use crate::prelude::*;
use crate::progress::{FullProgress, PhasedProgressBar};
use crate::rpc_continuations::{RequestGuid, RpcContinuation};
use crate::s9pk::manifest::PackageId;
use crate::s9pk::merkle_archive::source::http::HttpSource;
use crate::s9pk::S9pk;
use crate::upload::upload;
use crate::util::clap::FromStrParser;
use crate::util::Never;

pub const PKG_ARCHIVE_DIR: &str = "package-data/archive";
pub const PKG_PUBLIC_DIR: &str = "package-data/public";
pub const PKG_WASM_DIR: &str = "package-data/wasm";

// #[command(display(display_serializable))]
pub async fn list(ctx: RpcContext) -> Result<Value, Error> {
    Ok(ctx.db.peek().await.as_public().as_package_data().as_entries()?
        .iter()
        .filter_map(|(id, pde)| {
            let status = match pde.as_state_info().as_match() {
                PackageStateMatchModelRef::Installed(_) => {
                    "installed"
                }
                PackageStateMatchModelRef::Installing(_) => {
                    "installing"
                }
                PackageStateMatchModelRef::Updating(_) => {
                    "updating"
                }
                PackageStateMatchModelRef::Restoring(_) => {
                    "restoring"
                }
                PackageStateMatchModelRef::Removing(_) => {
                    "removing"
                }
                PackageStateMatchModelRef::Error(_) => {
                    "error"
                }
            };
            serde_json::to_value(json!({ "status": status, "id": id.clone(), "version": pde.as_state_info().as_manifest(ManifestPreference::Old).as_version().de().ok()?}))
            .ok()
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

#[derive(Deserialize, Serialize, Parser, TS)]
#[serde(rename_all = "camelCase")]
#[command(rename_all = "kebab-case")]
pub struct InstallParams {
    id: PackageId,
    #[arg(short = 'm', long = "marketplace-url")]
    #[ts(type = "string | null")]
    registry: Option<Url>,
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
        registry,
        version_spec,
        version_priority,
    }: InstallParams,
) -> Result<(), Error> {
    let version_str = match &version_spec {
        None => "*",
        Some(v) => &*v,
    };
    let version: VersionRange = version_str.parse()?;
    let registry = registry.unwrap_or_else(|| crate::DEFAULT_MARKETPLACE.parse().unwrap());
    let version_priority = version_priority.unwrap_or_default();
    let s9pk = S9pk::deserialize(
        &HttpSource::new(
            ctx.client.clone(),
            format!(
                "{}/package/v0/{}.s9pk?spec={}&version-priority={}",
                registry, id, version, version_priority,
            )
            .parse()?,
        )
        .await?,
        true,
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

#[derive(Deserialize, Serialize)]
#[serde(rename_all = "camelCase")]
pub struct SideloadResponse {
    pub upload: RequestGuid,
    pub progress: RequestGuid,
}

#[instrument(skip_all)]
pub async fn sideload(ctx: RpcContext) -> Result<SideloadResponse, Error> {
    let (upload, file) = upload(&ctx).await?;
    let (id_send, id_recv) = oneshot::channel();
    let (err_send, err_recv) = oneshot::channel();
    let progress = RequestGuid::new();
    let db = ctx.db.clone();
    let mut sub = db
        .subscribe(
            "/package-data/{id}/install-progress"
                .parse::<JsonPointer>()
                .with_kind(ErrorKind::Database)?,
        )
        .await;
    ctx.rpc_continuations.add(
        progress.clone(),
        RpcContinuation::ws(
            Box::new(|mut ws| {
                use axum::extract::ws::Message;
                async move {
                    if let Err(e) = async {
                        let id = id_recv.await.map_err(|_| {
                            Error::new(
                                eyre!("Could not get id to watch progress"),
                                ErrorKind::Cancelled,
                            )
                        })?;
                        tokio::select! {
                            res = async {
                                while let Some(_) = sub.recv().await {
                                    ws.send(Message::Text(
                                        serde_json::to_string(&if let Some(p) = db
                                            .peek()
                                            .await
                                            .as_public()
                                            .as_package_data()
                                            .as_idx(&id)
                                            .and_then(|e| e.as_state_info().as_installing_info()).map(|i| i.as_progress())
                                        {
                                            Ok::<_, ()>(p.de()?)
                                        } else {
                                            let mut p = FullProgress::new();
                                            p.overall.complete();
                                            Ok(p)
                                        })
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
                                        serde_json::to_string(&Err::<(), _>(e))
                                        .with_kind(ErrorKind::Serialization)?,
                                    ))
                                    .await
                                    .with_kind(ErrorKind::Network)?;
                                }
                            }
                        }

                        ws.close().await.with_kind(ErrorKind::Network)?;

                        Ok::<_, Error>(())
                    }
                    .await
                    {
                        tracing::error!("Error tracking sideload progress: {e}");
                        tracing::debug!("{e:?}");
                    }
                }
                .boxed()
            }),
            Duration::from_secs(600),
        ),
    )
    .await;
    tokio::spawn(async move {
        if let Err(e) = async {
            let s9pk = S9pk::deserialize(&file, true).await?;
            let _ = id_send.send(s9pk.as_manifest().id.clone());
            ctx.services
                .install(ctx.clone(), s9pk, None::<Never>)
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

#[derive(Deserialize, Serialize)]
#[serde(rename_all = "camelCase")]
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
            let file = crate::s9pk::load(&ctx, path).await?;

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

                loop {
                    tokio::select! {
                        msg = ws.next() => {
                            if let Some(msg) = msg {
                                if let Message::Text(t) = msg.with_kind(ErrorKind::Network)? {
                                    progress =
                                        serde_json::from_str::<Result<_, RpcError>>(&t)
                                            .with_kind(ErrorKind::Deserialization)??;
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
        CliInstallParams::Marketplace(params) => {
            ctx.call_remote::<RpcContext>(&method.join("."), to_value(&params)?)
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
