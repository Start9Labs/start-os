use std::collections::BTreeMap;
use std::io::SeekFrom;
use std::marker::PhantomData;
use std::path::{Path, PathBuf};
use std::process::Stdio;
use std::time::Duration;

use clap::ArgMatches;
use color_eyre::eyre::eyre;
use emver::VersionRange;
use futures::future::BoxFuture;
use futures::{FutureExt, StreamExt, TryStreamExt};
use http::header::CONTENT_LENGTH;
use http::{Request, Response, StatusCode};
use hyper::Body;
use models::DataUrl;
use reqwest::Url;
use rpc_toolkit::command;
use rpc_toolkit::yajrc::RpcError;
use tokio::fs::{File, OpenOptions};
use tokio::io::{AsyncRead, AsyncSeek, AsyncSeekExt};
use tokio::process::Command;
use tokio::sync::watch;
use tokio_stream::wrappers::ReadDirStream;
use tracing::instrument;

use crate::context::{CliContext, RpcContext};
use crate::core::rpc_continuations::{RequestGuid, RpcContinuation};
use crate::db::model::*;
use crate::dependencies::{
    get_current_dependents, BreakageRes, DependencyError, TaggedDependencyError,
};
use crate::install::cleanup::cleanup_failed;
use crate::install::progress::{InstallProgress, InstallProgressTracker};
use crate::notifications::NotificationLevel;
use crate::prelude::*;
use crate::s9pk::manifest::{Manifest, PackageId};
use crate::s9pk::reader::S9pkReader;
use crate::util::io::{copy_and_shutdown, response_to_reader};
use crate::util::serde::{display_serializable, IoFormat};
use crate::util::{display_none, Version};
use crate::version::{Current, VersionT};
use crate::volume::{asset_dir, script_dir};

pub mod cleanup;
pub mod progress;
pub mod update;

pub const PKG_ARCHIVE_DIR: &str = "package-data/archive";
pub const PKG_PUBLIC_DIR: &str = "package-data/public";
pub const PKG_WASM_DIR: &str = "package-data/wasm";

pub const PROGRESS_INTERVAL: Duration = Duration::from_millis(250);

pub fn default_icon() -> DataUrl<'static> {
    DataUrl::from_const(
        "image/png",
        include_bytes!("../../../frontend/projects/shared/assets/img/package-icon.png"),
    )
}

fn display_packages(arg: Vec<(PackageId, Version, String)>, matches: &ArgMatches) {
    use prettytable::*;

    if matches.is_present("format") {
        return display_serializable(arg, matches);
    }

    let mut table = Table::new();
    table.add_row(row![bc =>
        "ID",
        "VERSION",
        "STATE",
    ]);
    for (id, version, state) in arg {
        let mut row = row![&*id, version.as_str(), &state,];
        table.add_row(row);
    }
    table.print_tty(false).unwrap();
}

#[command(display(display_packages))]
pub async fn list(
    #[context] ctx: RpcContext,
    #[allow(unused_variables)]
    #[arg(long = "format")]
    format: Option<IoFormat>,
) -> Result<Vec<(PackageId, Version, String)>, Error> {
    ctx.db
        .peek()
        .await?
        .into_package_data()
        .into_entries()?
        .into_iter()
        .map(|(id, pde)| {
            use patch_db::ModelExt;
            Ok((
                id,
                pde.as_manifest()?.as_version().clone().de()?,
                pde.as_value()["state"]
                    .as_str()
                    .ok_or_else(|| {
                        Error::new(
                            eyre!("missing `state` on PackageDataEntry"),
                            ErrorKind::Database,
                        )
                    })?
                    .to_owned(),
            ))
        })
        .collect()
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
                ErrorKind::ParseVersion,
            )),
        }
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

fn set_installing(
    db: &mut DatabaseModel,
    man: &Manifest,
    static_files: &StaticFiles,
    size: Option<u64>,
) -> Result<(), Error> {
    let new = match db
        .as_package_data_mut()
        .remove(&man.id)?
        .map(|pde| pde.into_match())
    {
        Some(PackageDataEntryMatchModel::Installed(pde)) => {
            Model::<PackageDataEntry>::from_match(PackageDataEntryMatchModel::Updating(Model::<
                PackageDataEntryUpdating,
            >::from_parts(
                Model::new(static_files)?,
                pde.as_manifest().clone(),
                Model::new(man)?,
                Model::from_option(Some(pde.as_installed().clone())),
                Model::new(&InstallProgress::new(size))?,
            )))
        }
        Some(PackageDataEntryMatchModel::NeedsUpdate(pde)) => {
            Model::<PackageDataEntry>::from_match(PackageDataEntryMatchModel::Updating(Model::<
                PackageDataEntryUpdating,
            >::from_parts(
                Model::new(static_files)?,
                pde.as_manifest().clone(),
                Model::new(man)?,
                Model::from_option(None),
                Model::new(&InstallProgress::new(size))?,
            )))
        }
        None => Model::<PackageDataEntry>::from_match(PackageDataEntryMatchModel::Installing(
            Model::<PackageDataEntryInstalling>::from_parts(
                Model::new(static_files)?,
                Model::new(man)?,
                Model::new(&InstallProgress::new(size))?,
            ),
        )),
        _ => {
            return Err(Error::new(
                eyre!("cannot install over package in transient state"),
                ErrorKind::InvalidRequest,
            ))
        }
    };
    db.as_package_data_mut().insert_model(&man.id, new)?;
    Ok(())
}

fn set_removing(db: &mut DatabaseModel, id: &PackageId) -> Result<(), Error> {
    let new = match db
        .as_package_data_mut()
        .remove(id)?
        .or_not_found(id)?
        .into_match()
    {
        PackageDataEntryMatchModel::Installed(pde) => {
            Model::<PackageDataEntry>::from_match(PackageDataEntryMatchModel::Removing(Model::<
                PackageDataEntryRemoving,
            >::from_parts(
                pde.as_static_files().clone(),
                pde.as_manifest().clone(),
            )))
        }
        _ => {
            return Err(Error::new(
                eyre!("cannot install over package in transient state"),
                ErrorKind::InvalidRequest,
            ))
        }
    };
    Ok(())
}

#[command(
    custom_cli(cli_install(async, context(CliContext))),
    display(display_none),
    metadata(sync_db = true)
)]
#[instrument(skip_all)]
pub async fn install(
    #[context] ctx: RpcContext,
    #[arg] id: String,
    #[arg(short = 'm', long = "marketplace-url", rename = "marketplace-url")]
    marketplace_url: Option<Url>,
    #[arg(short = 'v', long = "version-spec", rename = "version-spec")] version_spec: Option<
        String,
    >,
    #[arg(long = "version-priority", rename = "version-priority")] version_priority: Option<MinMax>,
) -> Result<(), Error> {
    let version_str = match &version_spec {
        None => "*",
        Some(v) => &*v,
    };
    let version: VersionRange = version_str.parse()?;
    let marketplace_url =
        marketplace_url.unwrap_or_else(|| crate::DEFAULT_MARKETPLACE.parse().unwrap());
    let version_priority = version_priority.unwrap_or_default();
    let man: Manifest = reqwest::get(format!(
        "{}/package/v0/manifest/{}?spec={}&version-priority={}&eos-version-compat={}&arch={}",
        marketplace_url,
        id,
        version,
        version_priority,
        Current::new().compat(),
        &*crate::ARCH,
    ))
    .await
    .with_kind(ErrorKind::Registry)?
    .error_for_status()
    .with_kind(ErrorKind::Registry)?
    .json()
    .await
    .with_kind(ErrorKind::Registry)?;
    let s9pk = reqwest::get(format!(
        "{}/package/v0/{}.s9pk?spec=={}&version-priority={}&eos-version-compat={}&arch={}",
        marketplace_url,
        id,
        man.version,
        version_priority,
        Current::new().compat(),
        &*crate::ARCH,
    ))
    .await
    .with_kind(ErrorKind::Registry)?
    .error_for_status()
    .with_kind(ErrorKind::Registry)?;

    if &*man.id != &*id || !man.version.satisfies(&version) {
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

    let (license_res, instructions_res, icon_res) = tokio::join!(
        async {
            tokio::io::copy(
                &mut response_to_reader(
                    reqwest::get(format!(
                        "{}/package/v0/license/{}?spec=={}&eos-version-compat={}&arch={}",
                        marketplace_url,
                        id,
                        man.version,
                        Current::new().compat(),
                        &*crate::ARCH,
                    ))
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
                    reqwest::get(format!(
                        "{}/package/v0/instructions/{}?spec=={}&eos-version-compat={}&arch={}",
                        marketplace_url,
                        id,
                        man.version,
                        Current::new().compat(),
                        &*crate::ARCH,
                    ))
                    .await?
                    .error_for_status()?,
                ),
                &mut File::create(public_dir_path.join("INSTRUCTIONS.md")).await?,
            )
            .await?;
            Ok::<_, color_eyre::eyre::Report>(())
        },
        async {
            DataUrl::from_response(
                reqwest::get(format!(
                    "{}/package/v0/icon/{}?spec=={}&eos-version-compat={}&arch={}",
                    marketplace_url,
                    id,
                    man.version,
                    Current::new().compat(),
                    &*crate::ARCH,
                ))
                .await?
                .error_for_status()?,
            )
            .await
        },
    );
    if let Err(e) = license_res {
        tracing::warn!("Failed to pre-download license: {}", e);
    }
    if let Err(e) = instructions_res {
        tracing::warn!("Failed to pre-download instructions: {}", e);
    }
    let icon = match icon_res {
        Ok(a) => a,
        Err(e) => {
            tracing::warn!("Failed to pre-download icon: {}", e);
            default_icon()
        }
    };

    let static_files = StaticFiles::local(&man.id, &man.version, icon);
    let size = s9pk.content_length();
    ctx.db
        .mutate(|db| set_installing(db, &man, &static_files, size))
        .await?;

    tokio::spawn(async move {
        if let Err(e) = download_install_s9pk(
            &ctx,
            &man,
            Some(marketplace_url),
            s9pk.content_length(),
            response_to_reader(s9pk),
        )
        .await
        {
            let err_str = format!("Install of {}@{} Failed: {}", man.id, man.version, e);
            tracing::error!("{}", err_str);
            tracing::debug!("{:?}", e);
            if let Err(e) = ctx
                .notification_manager
                .notify(
                    &ctx.db,
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
    });

    Ok(())
}

#[command(rpc_only, display(display_none))]
#[instrument(skip_all)]
pub async fn sideload(
    #[context] ctx: RpcContext,
    #[arg] manifest: Manifest,
    #[arg] size: Option<u64>,
    #[arg] icon: Option<DataUrl>,
) -> Result<RequestGuid, Error> {
    let new_ctx = ctx.clone();
    let guid = RequestGuid::new();
    let static_files = StaticFiles::local(
        &manifest.id,
        &manifest.version,
        icon.unwrap_or_else(default_icon),
    );

    ctx.db
        .mutate(|db| set_installing(db, &manifest, &static_files, size))
        .await?;

    let handler = Box::new(move |req: Request<Body>| {
        async move {
            if let Err(e) = download_install_s9pk(
                &new_ctx,
                &manifest,
                None,
                size,
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
                        &new_ctx.db,
                        Some(manifest.id),
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

            Response::builder()
                .status(StatusCode::OK)
                .body(Body::empty())
                .with_kind(ErrorKind::Network)
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
        let len = reader.metadata().await?.len();
        let manifest = reader.manifest().await?;
        let icon = reader.icon(&manifest).await?;

        // rpc call remote sideload
        tracing::debug!("calling package.sideload");
        let guid = rpc_toolkit::command_helpers::call_remote(
            ctx.clone(),
            "package.sideload",
            serde_json::json!({ "manifest": manifest, "icon": icon, "size": len }),
            PhantomData::<RequestGuid>,
        )
        .await?
        .result?;
        tracing::debug!("package.sideload succeeded {:?}", guid);

        // hit continuation api with guid that comes back
        let file = tokio::fs::File::open(path).await?;
        let content_length = file.metadata().await?.len();
        let body = Body::wrap_stream(tokio_util::io::ReaderStream::new(file));
        let res = ctx
            .client
            .post(format!("{}rest/rpc/{}", ctx.base_url, guid,))
            .header(CONTENT_LENGTH, content_length)
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
                serde_json::json!({ "id": pkg, "marketplace-url": marketplace_url, "version-spec": v, "version-priority": version_priority })
            }
            (Some(_), Some(_)) => {
                return Err(Error::new(
                    eyre!("Invalid package id {}", target),
                    ErrorKind::InvalidRequest,
                )
                .into())
            }
            (None, Some(v)) => {
                serde_json::json!({ "id": target, "marketplace-url": marketplace_url, "version-spec": v, "version-priority": version_priority })
            }
            (None, None) => {
                serde_json::json!({ "id": target, "marketplace-url": marketplace_url, "version-priority": version_priority })
            }
        };
        tracing::debug!("calling package.install");
        rpc_toolkit::command_helpers::call_remote(
            ctx,
            "package.install",
            params,
            PhantomData::<()>,
        )
        .await?
        .result?;
        tracing::debug!("package.install succeeded");
    }
    Ok(())
}

#[command(
    subcommands(self(uninstall_impl(async)), uninstall_dry),
    display(display_none),
    metadata(sync_db = true)
)]
pub async fn uninstall(#[arg] id: PackageId) -> Result<PackageId, Error> {
    Ok(id)
}

#[command(rename = "dry", display(display_serializable))]
#[instrument(skip_all)]
pub async fn uninstall_dry(
    #[context] ctx: RpcContext,
    #[parent_data] id: PackageId,
) -> Result<BreakageRes, Error> {
    let mut res = BTreeMap::new();
    for (dep, _) in get_current_dependents(&ctx.db.peek().await?.into_package_data(), &id)?.0 {
        res.insert(
            dep,
            TaggedDependencyError {
                dependency: id.clone(),
                error: DependencyError::NotInstalled,
            },
        );
    }
    Ok(BreakageRes(res))
}

#[instrument(skip_all)]
pub async fn uninstall_impl(ctx: RpcContext, id: PackageId) -> Result<(), Error> {
    ctx.db.mutate(|db| set_removing(db, &id)).await?;

    tokio::spawn(async move {
        if let Err(e) = async { cleanup::uninstall(&ctx, &id).await }.await {
            let err_str = format!("Uninstall of {} Failed: {}", id, e);
            tracing::error!("{}", err_str);
            tracing::debug!("{:?}", e);
            if let Err(e) = ctx
                .notification_manager
                .notify(
                    &ctx.db,
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

    Ok(())
}

#[instrument(skip_all)]
pub async fn download_install_s9pk(
    ctx: &RpcContext,
    temp_manifest: &Manifest,
    marketplace_url: Option<Url>,
    size: Option<u64>,
    mut s9pk: impl AsyncRead + Unpin,
) -> Result<(), Error> {
    let pkg_id = &temp_manifest.id;
    let version = &temp_manifest.version;

    if let Err(e) = async {
        let pkg_archive_dir = ctx
            .datadir
            .join(PKG_ARCHIVE_DIR)
            .join(pkg_id)
            .join(version.as_str());
        tokio::fs::create_dir_all(&pkg_archive_dir).await?;
        let pkg_archive =
            pkg_archive_dir.join(AsRef::<Path>::as_ref(pkg_id).with_extension("s9pk"));

        let dst = OpenOptions::new()
            .create(true)
            .write(true)
            .read(true)
            .truncate(true)
            .open(&pkg_archive)
            .await?;

        let mut progress_tracker = InstallProgressTracker::new(dst, PROGRESS_INTERVAL, size);
        let mut progress_sub = progress_tracker.subscribe();
        progress::update_during(
            &ctx.db,
            &temp_manifest.id,
            &mut progress_sub,
            tokio::io::copy(&mut s9pk, &mut progress_tracker),
        )
        .await?;

        progress_tracker.downloaded();
        progress_tracker.seek(SeekFrom::Start(0)).await?;
        let progress = *progress_sub.borrow_and_update();
        ctx.db
            .mutate(|db| progress.update(db, &temp_manifest.id))
            .await?;

        let mut s9pk_reader = progress::update_during(
            &ctx.db,
            &temp_manifest.id,
            &mut progress_sub,
            S9pkReader::from_reader(progress_tracker, true),
        )
        .await?;

        s9pk_reader.validate(Some(&temp_manifest)).await?;
        s9pk_reader.validated();

        s9pk_reader.reset().await?;

        let progress = *progress_sub.borrow_and_update();
        ctx.db.mutate(|db| progress.update(db, &pkg_id)).await?;

        install_s9pk(
            &ctx,
            pkg_id,
            version,
            marketplace_url,
            &mut s9pk_reader,
            &mut progress_sub,
        )
        .await?;

        Ok(())
    }
    .await
    {
        if let Err(e) = cleanup_failed(&ctx, pkg_id).await {
            tracing::error!("Failed to clean up {}@{}: {}", pkg_id, version, e);
            tracing::debug!("{:?}", e);
        }
        Err(e)
    } else {
        Ok(())
    }
}

// #[instrument(skip_all)]
async fn install_s9pk<R: AsyncRead + AsyncSeek + Unpin + Send + Sync>(
    ctx: &RpcContext,
    pkg_id: &PackageId,
    version: &Version,
    marketplace_url: Option<Url>,
    rdr: &mut S9pkReader<InstallProgressTracker<R>>,
    progress_sub: &mut watch::Receiver<InstallProgress>,
) -> Result<(), Error> {
    let snapshot = ctx.db.peek().await?;
    let developer_key = rdr.developer_key().clone();

    tracing::info!("Install {}@{}: Unpacking Manifest", pkg_id, version);
    let manifest = progress::update_during(&ctx.db, &pkg_id, progress_sub, rdr.manifest()).await?;
    tracing::info!("Install {}@{}: Unpacked Manifest", pkg_id, version);

    tracing::info!("Install {}@{}: Unpacking icon", pkg_id, version);
    let icon = progress::update_during(&ctx.db, &pkg_id, progress_sub, rdr.icon(&manifest)).await?;
    tracing::info!("Install {}@{}: Unpacked icon", pkg_id, version);

    tracing::info!("Install {}@{}: Fetching Dependency Info", pkg_id, version);
    let mut dependency_info = BTreeMap::new();
    for (dep, info) in &manifest.dependencies.0 {
        let (manifest, icon): (Option<Manifest>, Option<DataUrl>) = if let Some((local_man, icon)) =
            snapshot
                .as_package_data()
                .as_idx(dep)
                .map(|pde| {
                    Ok::<_, Error>((
                        pde.as_manifest()?.clone().de()?,
                        pde.as_static_files()?.as_icon().clone().de()?,
                    ))
                })
                .transpose()?
        {
            (Some(local_man), Some(icon))
        } else if let Some(marketplace_url) = &marketplace_url {
            let man = match reqwest::get(format!(
                "{}/package/v0/manifest/{}?spec={}&eos-version-compat={}&arch={}",
                marketplace_url,
                dep,
                info.version,
                Current::new().compat(),
                &*crate::ARCH,
            ))
            .await
            .with_kind(ErrorKind::Registry)?
            .error_for_status()
            {
                Ok(a) => Ok(Some(a.json().await.with_kind(ErrorKind::Deserialization)?)),
                Err(e) if e.status() == Some(StatusCode::BAD_REQUEST) => Ok(None),
                Err(e) => Err(e),
            }
            .with_kind(ErrorKind::Registry)?;
            (man, None)
        } else {
            (None, None)
        };

        if let Some(marketplace_url) = &marketplace_url {
            if let Some(manifest) = &manifest {
                let dir = ctx
                    .datadir
                    .join(PKG_PUBLIC_DIR)
                    .join(&manifest.id)
                    .join(manifest.version.as_str());
                let icon_path = dir.join(format!("icon.{}", manifest.assets.icon_type()));
                if tokio::fs::metadata(&icon_path).await.is_err() {
                    tokio::fs::create_dir_all(&dir).await?;
                    let icon = reqwest::get(format!(
                        "{}/package/v0/icon/{}?spec={}&eos-version-compat={}&arch={}",
                        marketplace_url,
                        dep,
                        info.version,
                        Current::new().compat(),
                        &*crate::ARCH,
                    ))
                    .await
                    .with_kind(ErrorKind::Registry)?;
                    let mut dst = File::create(&icon_path).await?;
                    tokio::io::copy(&mut response_to_reader(icon), &mut dst).await?;
                    dst.sync_all().await?;
                }
            }
        }

        dependency_info.insert(
            dep.clone(),
            StaticDependencyInfo {
                icon: icon.unwrap_or_else(default_icon),
                title: manifest
                    .as_ref()
                    .map(|m| m.title.clone())
                    .unwrap_or_else(|| dep.to_string()),
            },
        );
    }
    tracing::info!("Install {}@{}: Fetched Dependency Info", pkg_id, version);

    let public_dir_path = ctx
        .datadir
        .join(PKG_PUBLIC_DIR)
        .join(pkg_id)
        .join(version.as_str());
    tokio::fs::create_dir_all(&public_dir_path).await?;

    tracing::info!("Install {}@{}: Unpacking LICENSE.md", pkg_id, version);
    progress::update_during(&ctx.db, &pkg_id, progress_sub, async {
        let license_path = public_dir_path.join("LICENSE.md");
        let mut dst = File::create(&license_path).await?;
        tokio::io::copy(&mut rdr.license().await?, &mut dst).await?;
        dst.sync_all().await?;
        Ok::<_, Error>(())
    })
    .await?;
    tracing::info!("Install {}@{}: Unpacked LICENSE.md", pkg_id, version);

    tracing::info!("Install {}@{}: Unpacking INSTRUCTIONS.md", pkg_id, version);
    progress::update_during(&ctx.db, &pkg_id, progress_sub, async {
        let instructions_path = public_dir_path.join("INSTRUCTIONS.md");
        let mut dst = File::create(&instructions_path).await?;
        tokio::io::copy(&mut rdr.instructions().await?, &mut dst).await?;
        dst.sync_all().await?;
        Ok::<_, Error>(())
    })
    .await?;
    tracing::info!("Install {}@{}: Unpacked INSTRUCTIONS.md", pkg_id, version);

    tracing::info!("Install {}@{}: Unpacking Docker Images", pkg_id, version);
    progress::update_during(&ctx.db, &pkg_id, progress_sub, async {
        let mut load = Command::new("docker")
            .arg("load")
            .stdin(Stdio::piped())
            .stderr(Stdio::piped())
            .spawn()?;
        let load_in = load.stdin.take().ok_or_else(|| {
            Error::new(
                eyre!("Could not write to stdin of docker load"),
                ErrorKind::Docker,
            )
        })?;
        let mut docker_rdr = rdr.docker_images().await?;
        copy_and_shutdown(&mut docker_rdr, load_in).await?;
        let res = load.wait_with_output().await?;
        if !res.status.success() {
            Err(Error::new(
                eyre!(
                    "{}",
                    String::from_utf8(res.stderr)
                        .unwrap_or_else(|e| format!("Could not parse stderr: {}", e))
                ),
                ErrorKind::Docker,
            ))
        } else {
            Ok(())
        }
    })
    .await?;
    tracing::info!("Install {}@{}: Unpacked Docker Images", pkg_id, version,);

    tracing::info!("Install {}@{}: Unpacking Assets", pkg_id, version);
    progress::update_during(&ctx.db, &pkg_id, progress_sub, async {
        let asset_dir = asset_dir(&ctx.datadir, pkg_id, version);
        if tokio::fs::metadata(&asset_dir).await.is_err() {
            tokio::fs::create_dir_all(&asset_dir).await?;
        }
        let mut tar = tokio_tar::Archive::new(rdr.assets().await?);
        tar.unpack(asset_dir).await?;

        let script_dir = script_dir(&ctx.datadir, pkg_id, version);
        if tokio::fs::metadata(&script_dir).await.is_err() {
            tokio::fs::create_dir_all(&script_dir).await?;
        }
        if let Some(mut hdl) = rdr.scripts().await? {
            tokio::io::copy(
                &mut hdl,
                &mut File::create(script_dir.join("embassy.js")).await?,
            )
            .await?;
        }

        Ok::<_, Error>(())
    })
    .await?;
    tracing::info!("Install {}@{}: Unpacked Assets", pkg_id, version);

    rdr.unpacked();
    let progress = *progress_sub.borrow_and_update();
    ctx.db.mutate(|db| progress.update(db, &pkg_id)).await?;

    // TODO: grab previous manager
    // TODO: deinit
    // TODO: exit previous manager

    tracing::info!("Install {}@{}: Creating manager", pkg_id, version);
    ctx.managers
        .add(ctx.clone(), manifest.clone(), marketplace_url)
        .await?;
    tracing::info!("Install {}@{}: Created manager", pkg_id, version);

    // TODO: init OR restore

    // TODO: update patchdb: service state + dependencies / dependency errors

    let static_files = StaticFiles::local(pkg_id, version, icon);
    let current_dependencies: CurrentDependencies = CurrentDependencies(
        manifest
            .dependencies
            .0
            .iter()
            .filter_map(|(id, info)| {
                if info.requirement.required() {
                    Some((id.clone(), CurrentDependencyInfo::default()))
                } else {
                    None
                }
            })
            .collect(),
    );

    tracing::info!("Install {}@{}: Complete", pkg_id, version);

    Ok(())
}

#[instrument(skip_all)]
pub fn load_images<'a, P: AsRef<Path> + 'a + Send + Sync>(
    datadir: P,
) -> BoxFuture<'a, Result<(), Error>> {
    async move {
        let docker_dir = datadir.as_ref();
        if tokio::fs::metadata(&docker_dir).await.is_ok() {
            ReadDirStream::new(tokio::fs::read_dir(&docker_dir).await?)
                .map(|r| r.with_ctx(|_| (ErrorKind::Filesystem, format!("{:?}", &docker_dir))))
                .try_for_each(|entry| async move {
                    let m = entry.metadata().await?;
                    if m.is_file() {
                        let path = entry.path();
                        let ext = path.extension().and_then(|ext| ext.to_str());
                        if ext == Some("tar") || ext == Some("s9pk") {
                            let mut load = Command::new("docker")
                                .arg("load")
                                .stdin(Stdio::piped())
                                .stderr(Stdio::piped())
                                .spawn()?;
                            let load_in = load.stdin.take().ok_or_else(|| {
                                Error::new(
                                    eyre!("Could not write to stdin of docker load"),
                                    ErrorKind::Docker,
                                )
                            })?;
                            match ext {
                                Some("tar") => {
                                    copy_and_shutdown(&mut File::open(&path).await?, load_in)
                                        .await?
                                }
                                Some("s9pk") => match async {
                                    let mut reader = S9pkReader::open(&path, true).await?;
                                    copy_and_shutdown(&mut reader.docker_images().await?, load_in)
                                        .await?;
                                    Ok::<_, Error>(())
                                }
                                .await
                                {
                                    Ok(()) => (),
                                    Err(e) => {
                                        tracing::error!(
                                            "Error loading docker images from s9pk: {e}"
                                        );
                                        tracing::debug!("{e:?}");
                                        return Ok(());
                                    }
                                },
                                _ => unreachable!(),
                            };

                            let res = load.wait_with_output().await?;
                            if !res.status.success() {
                                Err(Error::new(
                                    eyre!(
                                        "{}",
                                        String::from_utf8(res.stderr).unwrap_or_else(|e| format!(
                                            "Could not parse stderr: {}",
                                            e
                                        ))
                                    ),
                                    ErrorKind::Docker,
                                ))
                            } else {
                                Ok(())
                            }
                        } else {
                            Ok(())
                        }
                    } else if m.is_dir() {
                        load_images(entry.path()).await?;
                        Ok(())
                    } else {
                        Ok(())
                    }
                })
                .await
        } else {
            Ok(())
        }
    }
    .boxed()
}
