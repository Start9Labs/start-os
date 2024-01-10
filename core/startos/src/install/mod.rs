use std::collections::BTreeMap;
use std::io::SeekFrom;
use std::path::{Path, PathBuf};
use std::sync::atomic::Ordering;
use std::sync::Arc;
use std::time::Duration;

use axum::body::Body;
use clap::builder::ValueParserFactory;
use clap::Parser;
use color_eyre::eyre::eyre;
use emver::VersionRange;
use futures::future::BoxFuture;
use futures::{FutureExt, StreamExt, TryStreamExt};
use http::header::CONTENT_LENGTH;
use http::{HeaderMap, Response, StatusCode};
use models::{mime, DataUrl};
use reqwest::Url;
use rpc_toolkit::yajrc::RpcError;
use rpc_toolkit::CallRemote;
use serde::{Deserialize, Serialize};
use serde_json::{json, Value};
use tokio::fs::{File, OpenOptions};
use tokio::io::{AsyncRead, AsyncSeek, AsyncSeekExt, AsyncWriteExt};
use tokio::process::Command;
use tokio::sync::oneshot;
use tokio_stream::wrappers::ReadDirStream;
use tracing::instrument;

use crate::config::ConfigureContext;
use crate::context::{CliContext, RpcContext};
use crate::core::rpc_continuations::{RequestGuid, RpcContinuation};
use crate::db::model::{
    CurrentDependencies, CurrentDependencyInfo, CurrentDependents, InstalledPackageInfo,
    PackageDataEntry, PackageDataEntryInstalled, PackageDataEntryInstalling,
    PackageDataEntryMatchModelRef, PackageDataEntryRemoving, PackageDataEntryRestoring,
    PackageDataEntryUpdating, StaticDependencyInfo, StaticFiles,
};
use crate::dependencies::{
    add_dependent_to_current_dependents_lists, compute_dependency_config_errs,
    set_dependents_with_live_pointers_to_needs_config,
};
use crate::install::progress::{InstallProgress, InstallProgressTracker};
use crate::notifications::NotificationLevel;
use crate::prelude::*;
use crate::registry::marketplace::with_query_params;
use crate::s9pk::manifest::{Manifest, PackageId};
use crate::s9pk::reader::S9pkReader;
use crate::status::{MainStatus, Status};
use crate::util::clap::FromStrParser;
use crate::util::docker::CONTAINER_TOOL;
use crate::util::io::response_to_reader;
use crate::util::serde::Port;
use crate::util::{AsyncFileExt, Invoke, Version};
use crate::volume::{asset_dir, script_dir};
use crate::{Error, ErrorKind, ResultExt};

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
    let man: Manifest = ctx
        .client
        .get(with_query_params(
            ctx.clone(),
            format!(
                "{}/package/v0/manifest/{}?spec={}&version-priority={}",
                marketplace_url, id, version, version_priority,
            )
            .parse()?,
        ))
        .send()
        .await
        .with_kind(crate::ErrorKind::Registry)?
        .error_for_status()
        .with_kind(crate::ErrorKind::Registry)?
        .json()
        .await
        .with_kind(crate::ErrorKind::Registry)?;
    let s9pk = ctx
        .client
        .get(with_query_params(
            ctx.clone(),
            format!(
                "{}/package/v0/{}.s9pk?spec=={}&version-priority={}",
                marketplace_url, id, man.version, version_priority,
            )
            .parse()?,
        ))
        .send()
        .await
        .with_kind(crate::ErrorKind::Registry)?
        .error_for_status()?;

    if *man.id != *id || !man.version.satisfies(&version) {
        return Err(Error::new(
            eyre!("Fetched package does not match requested id and version"),
            ErrorKind::Registry,
        ));
    }

    let public_dir_path = ctx
        .datadir
        .join(PKG_PUBLIC_DIR)
        .join(&man.id)
        .join(man.version.as_str());
    tokio::fs::create_dir_all(&public_dir_path).await?;

    let icon_type = todo!() as &str;
    let (license_res, instructions_res, icon_res) = tokio::join!(
        async {
            tokio::io::copy(
                &mut response_to_reader(
                    ctx.client
                        .get(with_query_params(
                            ctx.clone(),
                            format!(
                                "{}/package/v0/license/{}?spec=={}",
                                marketplace_url, id, man.version,
                            )
                            .parse()?,
                        ))
                        .send()
                        .await?
                        .error_for_status()?,
                ),
                &mut File::create(public_dir_path.join("LICENSE.md")).await?,
            )
            .await?;
            Ok::<_, color_eyre::eyre::Report>(())
        },
        async {
            tokio::io::copy(
                &mut response_to_reader(
                    ctx.client
                        .get(with_query_params(
                            ctx.clone(),
                            format!(
                                "{}/package/v0/instructions/{}?spec=={}",
                                marketplace_url, id, man.version,
                            )
                            .parse()?,
                        ))
                        .send()
                        .await?
                        .error_for_status()?,
                ),
                &mut File::create(public_dir_path.join("INSTRUCTIONS.md")).await?,
            )
            .await?;
            Ok::<_, color_eyre::eyre::Report>(())
        },
        async {
            tokio::io::copy(
                &mut response_to_reader(
                    ctx.client
                        .get(with_query_params(
                            ctx.clone(),
                            format!(
                                "{}/package/v0/icon/{}?spec=={}",
                                marketplace_url, id, man.version,
                            )
                            .parse()?,
                        ))
                        .send()
                        .await?
                        .error_for_status()?,
                ),
                &mut File::create(public_dir_path.join(format!("icon.{}", icon_type))).await?,
            )
            .await?;
            Ok::<_, color_eyre::eyre::Report>(())
        },
    );
    if let Err(e) = license_res {
        tracing::warn!("Failed to pre-download license: {}", e);
    }
    if let Err(e) = instructions_res {
        tracing::warn!("Failed to pre-download instructions: {}", e);
    }
    if let Err(e) = icon_res {
        tracing::warn!("Failed to pre-download icon: {}", e);
    }

    let progress = Arc::new(InstallProgress::new(s9pk.content_length()));
    let static_files = StaticFiles::local(&man.id, &man.version, icon_type);
    ctx.db
        .mutate(|db| {
            let pde = match db
                .as_package_data()
                .as_idx(&man.id)
                .map(|x| x.de())
                .transpose()?
            {
                Some(PackageDataEntry::Installed(PackageDataEntryInstalled {
                    installed,
                    static_files,
                    ..
                })) => PackageDataEntry::Updating(PackageDataEntryUpdating {
                    install_progress: progress.clone(),
                    static_files,
                    installed,
                    manifest: man.clone(),
                }),
                None => PackageDataEntry::Installing(PackageDataEntryInstalling {
                    install_progress: progress.clone(),
                    static_files,
                    manifest: man.clone(),
                }),
                _ => {
                    return Err(Error::new(
                        eyre!("Cannot install over a package in a transient state"),
                        crate::ErrorKind::InvalidRequest,
                    ))
                }
            };
            db.as_package_data_mut().insert(&man.id, &pde)
        })
        .await?;

    let downloading = download_install_s9pk(
        ctx.clone(),
        man.clone(),
        Some(marketplace_url),
        Arc::new(InstallProgress::new(s9pk.content_length())),
        response_to_reader(s9pk),
        None,
    );
    tokio::spawn(async move {
        if let Err(e) = downloading.await {
            let err_str = format!("Install of {}@{} Failed: {}", man.id, man.version, e);
            tracing::error!("{}", err_str);
            tracing::debug!("{:?}", e);
            if let Err(e) = ctx
                .notification_manager
                .notify(
                    ctx.db.clone(),
                    Some(man.id),
                    NotificationLevel::Error,
                    String::from("Install Failed"),
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
        Ok::<_, String>(())
    });

    Ok(())
}

#[derive(Deserialize, Serialize)]
#[serde(rename_all = "kebab-case")]
pub struct SideloadParams {
    manifest: Manifest,
    icon: Option<String>,
}

#[instrument(skip_all)]
pub async fn sideload(
    ctx: RpcContext,
    SideloadParams { manifest, icon }: SideloadParams,
) -> Result<RequestGuid, Error> {
    let new_ctx = ctx.clone();
    let guid = RequestGuid::new();
    if let Some(icon) = icon {
        use tokio::io::AsyncWriteExt;

        let public_dir_path = ctx
            .datadir
            .join(PKG_PUBLIC_DIR)
            .join(&manifest.id)
            .join(manifest.version.as_str());
        tokio::fs::create_dir_all(&public_dir_path).await?;

        let invalid_data_url =
            || Error::new(eyre!("Invalid Icon Data URL"), ErrorKind::InvalidRequest);
        let data = icon
            .strip_prefix(&format!("data:image/{};base64,", todo!(),))
            .ok_or_else(&invalid_data_url)?;
        let mut icon_file = File::create(public_dir_path.join(format!("icon.{}", todo!()))).await?;
        icon_file
            .write_all(&base64::decode(data).with_kind(ErrorKind::InvalidRequest)?)
            .await?;
        icon_file.sync_all().await?;
    }

    let handler = Box::new(|headers: HeaderMap| {
        async move {
            let content_length = match headers.get(CONTENT_LENGTH).map(|a| a.to_str()) {
                None => None,
                Some(Err(_)) => {
                    return Response::builder()
                        .status(StatusCode::BAD_REQUEST)
                        .body(Body::from("Invalid Content Length"))
                        .with_kind(ErrorKind::Network)
                }
                Some(Ok(a)) => match a.parse::<u64>() {
                    Err(_) => {
                        return Response::builder()
                            .status(StatusCode::BAD_REQUEST)
                            .body(Body::from("Invalid Content Length"))
                            .with_kind(ErrorKind::Network)
                    }
                    Ok(a) => Some(a),
                },
            };
            let progress = Arc::new(InstallProgress::new(content_length));
            let install_progress = progress.clone();

            new_ctx
                .db
                .mutate(|db| {
                    let pde = match db
                        .as_package_data()
                        .as_idx(&manifest.id)
                        .map(|x| x.de())
                        .transpose()?
                    {
                        Some(PackageDataEntry::Installed(PackageDataEntryInstalled {
                            installed,
                            static_files,
                            ..
                        })) => PackageDataEntry::Updating(PackageDataEntryUpdating {
                            install_progress,
                            installed,
                            manifest: manifest.clone(),
                            static_files,
                        }),
                        None => PackageDataEntry::Installing(PackageDataEntryInstalling {
                            install_progress,
                            static_files: StaticFiles::local(
                                &manifest.id,
                                &manifest.version,
                                todo!(),
                            ),
                            manifest: manifest.clone(),
                        }),
                        _ => {
                            return Err(Error::new(
                                eyre!("Cannot install over a package in a transient state"),
                                crate::ErrorKind::InvalidRequest,
                            ))
                        }
                    };
                    db.as_package_data_mut().insert(&manifest.id, &pde)
                })
                .await?;

            let (send, recv) = oneshot::channel();

            tokio::spawn(async move {
                if let Err(e) = download_install_s9pk(
                    new_ctx.clone(),
                    manifest.clone(),
                    None,
                    progress,
                    tokio_util::io::StreamReader::new(req.into_body().map_err(|e| {
                        std::io::Error::new(
                            match &e {
                                e if e.is_connect() => std::io::ErrorKind::ConnectionRefused,
                                e if e.is_timeout() => std::io::ErrorKind::TimedOut,
                                _ => std::io::ErrorKind::Other,
                            },
                            e,
                        )
                    })),
                    Some(send),
                )
                .await
                {
                    let err_str = format!(
                        "Install of {}@{} Failed: {}",
                        manifest.id, manifest.version, e
                    );
                    tracing::error!("{}", err_str);
                    tracing::debug!("{:?}", e);
                    if let Err(e) = new_ctx
                        .notification_manager
                        .notify(
                            new_ctx.db.clone(),
                            Some(manifest.id.clone()),
                            NotificationLevel::Error,
                            String::from("Install Failed"),
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

            if let Ok(_) = recv.await {
                Response::builder()
                    .status(StatusCode::OK)
                    .body(Body::empty())
                    .with_kind(ErrorKind::Network)
            } else {
                Response::builder()
                    .status(StatusCode::INTERNAL_SERVER_ERROR)
                    .body(Body::from("installation aborted before upload completed"))
                    .with_kind(ErrorKind::Network)
            }
        }
        .boxed()
    });
    ctx.add_continuation(
        guid.clone(),
        RpcContinuation::rest(handler, Duration::from_secs(30)),
    )
    .await;
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

        // inspect manifest no verify
        let mut reader = S9pkReader::open(&path, false).await?;
        let manifest = reader.manifest().await?;
        let icon = reader.icon().await?.to_vec().await?;
        let icon_str = format!("data:image/{};base64,{}", todo!(), base64::encode(&icon));

        // rpc call remote sideload
        tracing::debug!("calling package.sideload");
        let guid = from_value::<RequestGuid>(
            ctx.call_remote(
                "package.sideload",
                imbl_value::json!({ "manifest": manifest, "icon": icon_str }),
            )
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
        if let Err(e) = ctx.managers.uninstall(&id).await {
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

#[instrument(skip_all)]
pub async fn download_install_s9pk(
    ctx: RpcContext,
    temp_manifest: Manifest,
    marketplace_url: Option<Url>,
    progress: Arc<InstallProgress>,
    mut s9pk: impl AsyncRead + Unpin,
    download_complete: Option<oneshot::Sender<()>>,
) -> Result<(), Error> {
    let pkg_id = &temp_manifest.id;
    let version = &temp_manifest.version;
    let db = ctx.db.peek().await;

    if let Result::<(), Error>::Err(e) = {
        let ctx = ctx.clone();
        async move {
            let pkg_archive_dir = ctx
                .datadir
                .join(PKG_ARCHIVE_DIR)
                .join(pkg_id)
                .join(version.as_str());
            tokio::fs::create_dir_all(&pkg_archive_dir).await?;
            let pkg_archive =
                pkg_archive_dir.join(AsRef::<Path>::as_ref(pkg_id).with_extension("s9pk"));

            File::delete(&pkg_archive).await?;
            let mut dst = OpenOptions::new()
                .create(true)
                .write(true)
                .read(true)
                .open(&pkg_archive)
                .await?;

            progress
                .track_download_during(ctx.db.clone(), pkg_id, || async {
                    let mut progress_writer =
                        InstallProgressTracker::new(&mut dst, progress.clone());
                    tokio::io::copy(&mut s9pk, &mut progress_writer).await?;
                    progress.download_complete();
                    if let Some(complete) = download_complete {
                        complete.send(()).unwrap_or_default();
                    }
                    Ok(())
                })
                .await?;

            dst.seek(SeekFrom::Start(0)).await?;

            let progress_reader = InstallProgressTracker::new(dst, progress.clone());
            let mut s9pk_reader = progress
                .track_read_during(ctx.db.clone(), pkg_id, || {
                    S9pkReader::from_reader(progress_reader, true)
                })
                .await?;

            ctx.managers.install(&ctx, s9pk_reader).await?;

            Ok(())
        }
    }
    .await
    {
        if let Err(e) = cleanup_failed(&ctx, pkg_id).await {
            tracing::error!("Failed to clean up {}@{}: {}", pkg_id, version, e);
            tracing::debug!("{:?}", e);
        }

        Err(e)
    } else {
        Ok::<_, Error>(())
    }
}
