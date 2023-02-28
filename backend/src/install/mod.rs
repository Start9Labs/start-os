use std::collections::BTreeMap;
use std::io::SeekFrom;
use std::marker::PhantomData;
use std::path::{Path, PathBuf};
use std::process::Stdio;
use std::sync::atomic::Ordering;
use std::sync::Arc;
use std::time::Duration;

use color_eyre::eyre::eyre;
use emver::VersionRange;
use futures::future::BoxFuture;
use futures::{FutureExt, StreamExt, TryStreamExt};
use http::header::CONTENT_LENGTH;
use http::{Request, Response, StatusCode};
use hyper::Body;
use reqwest::Url;
use rpc_toolkit::command;
use rpc_toolkit::yajrc::RpcError;
use tokio::fs::{File, OpenOptions};
use tokio::io::{AsyncRead, AsyncSeek, AsyncSeekExt};
use tokio::process::Command;
use tokio_stream::wrappers::ReadDirStream;
use tracing::instrument;

use self::cleanup::{cleanup_failed, remove_from_current_dependents_lists};
use crate::config::ConfigureContext;
use crate::context::{CliContext, RpcContext};
use crate::core::rpc_continuations::{RequestGuid, RpcContinuation};
use crate::db::model::{
    CurrentDependencies, CurrentDependencyInfo, InstalledPackageInfo, PackageDataEntry,
    StaticDependencyInfo, StaticFiles,
};
use crate::dependencies::{
    break_all_dependents_transitive, BreakageRes, DependencyError, DependencyErrors,
};
use crate::install::cleanup::{cleanup, update_dependency_errors_of_dependents};
use crate::install::progress::{InstallProgress, InstallProgressTracker};
use crate::notifications::NotificationLevel;
use crate::prelude::*;
use crate::s9pk::manifest::{Manifest, PackageId};
use crate::s9pk::reader::S9pkReader;
use crate::status::{MainStatus, Status};
use crate::util::io::{copy_and_shutdown, response_to_reader};
use crate::util::serde::{display_serializable, Port};
use crate::util::{display_none, AsyncFileExt, Version};
use crate::version::{Current, VersionT};
use crate::volume::{asset_dir, script_dir};

pub mod cleanup;
pub mod progress;
pub mod update;

pub const PKG_ARCHIVE_DIR: &str = "package-data/archive";
pub const PKG_PUBLIC_DIR: &str = "package-data/public";
pub const PKG_WASM_DIR: &str = "package-data/wasm";

#[command(display(display_serializable))]
pub async fn list(#[context] ctx: RpcContext) -> Result<Vec<(PackageId, Version)>, Error> {
    let mut hdl = ctx.db.handle();
    let package_data = todo!(); /* crate::db::DatabaseModel::new()
                                .package_data()
                                .get(&mut hdl)
                                .await?;*/

    Ok(package_data
        .0
        .iter()
        .filter_map(|(id, pde)| match pde {
            PackageDataEntry::Installed { installed, .. } => {
                Some((id.clone(), installed.manifest.version.clone()))
            }
            _ => None,
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

#[command(
    custom_cli(cli_install(async, context(CliContext))),
    display(display_none),
    metadata(sync_db = true)
)]
#[instrument(skip(ctx))]
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

    if man.id.as_str() != id || !man.version.satisfies(&version) {
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

    let icon_type = man.assets.icon_type();
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
            tokio::io::copy(
                &mut response_to_reader(
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

    let progress = InstallProgress::new(s9pk.content_length());
    let static_files = StaticFiles::local(&man.id, &man.version, icon_type);
    let mut db_handle = ctx.db.handle();
    let mut tx = db_handle.begin().await?;
    let mut pde = todo!(); /* crate::db::DatabaseModel::new()
                           .package_data()
                           .idx_model(&man.id)
                           .get_mut(&mut tx)
                           .await?;*/
    match pde.take() {
        Some(PackageDataEntry::Installed {
            installed,
            static_files,
            ..
        }) => {
            *pde = Some(PackageDataEntry::Updating {
                install_progress: progress.clone(),
                static_files,
                installed,
                manifest: man.clone(),
            })
        }
        None => {
            *pde = Some(PackageDataEntry::Installing {
                install_progress: progress.clone(),
                static_files,
                manifest: man.clone(),
            })
        }
        _ => {
            return Err(Error::new(
                eyre!("Cannot install over a package in a transient state"),
                ErrorKind::InvalidRequest,
            ))
        }
    }
    pde.save(&mut tx).await?;
    tx.commit().await?;
    drop(db_handle);

    tokio::spawn(async move {
        let mut db_handle = ctx.db.handle();
        if let Err(e) = download_install_s9pk(
            &ctx,
            &man,
            Some(marketplace_url),
            InstallProgress::new(s9pk.content_length()),
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
#[instrument(skip(ctx))]
pub async fn sideload(
    #[context] ctx: RpcContext,
    #[arg] manifest: Manifest,
    #[arg] icon: Option<String>,
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
            .strip_prefix(&format!(
                "data:image/{};base64,",
                manifest.assets.icon_type()
            ))
            .ok_or_else(&invalid_data_url)?;
        let mut icon_file =
            File::create(public_dir_path.join(format!("icon.{}", manifest.assets.icon_type())))
                .await?;
        icon_file
            .write_all(&base64::decode(data).with_kind(ErrorKind::InvalidRequest)?)
            .await?;
        icon_file.sync_all().await?;
    }

    let handler = Box::new(|req: Request<Body>| {
        async move {
            let content_length = match req.headers().get(CONTENT_LENGTH).map(|a| a.to_str()) {
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
            let progress = InstallProgress::new(content_length);

            let mut hdl = new_ctx.db.handle();
            let mut tx = hdl.begin().await?;

            let mut pde = todo!(); /* crate::db::DatabaseModel::new()
                                   .package_data()
                                   .idx_model(&manifest.id)
                                   .get_mut(&mut tx)
                                   .await?;*/
            match pde.take() {
                Some(PackageDataEntry::Installed {
                    installed,
                    static_files,
                    ..
                }) => {
                    *pde = Some(PackageDataEntry::Updating {
                        install_progress: progress.clone(),
                        installed,
                        manifest: manifest.clone(),
                        static_files,
                    })
                }
                None => {
                    *pde = Some(PackageDataEntry::Installing {
                        install_progress: progress.clone(),
                        static_files: StaticFiles::local(
                            &manifest.id,
                            &manifest.version,
                            &manifest.assets.icon_type(),
                        ),
                        manifest: manifest.clone(),
                    })
                }
                _ => {
                    return Err(Error::new(
                        eyre!("Cannot install over a package in a transient state"),
                        ErrorKind::InvalidRequest,
                    ))
                }
            }
            pde.save(&mut tx).await?;
            tx.commit().await?;

            if let Err(e) = download_install_s9pk(
                &new_ctx,
                &manifest,
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
                        &ctx.db,
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

#[instrument(skip(ctx))]
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
        let icon_str = format!(
            "data:image/{};base64,{}",
            manifest.assets.icon_type(),
            base64::encode(&icon)
        );

        // rpc call remote sideload
        tracing::debug!("calling package.sideload");
        let guid = rpc_toolkit::command_helpers::call_remote(
            ctx.clone(),
            "package.sideload",
            serde_json::json!({ "manifest": manifest, "icon": icon_str }),
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
#[instrument(skip(ctx))]
pub async fn uninstall_dry(
    #[context] ctx: RpcContext,
    #[parent_data] id: PackageId,
) -> Result<BreakageRes, Error> {
    let db = ctx.db.peek().await?;

    let error = TaggedDependencyError {
        dependency: id.clone(),
        error: DependencyError::NotInstalled,
    };
    Ok(BreakageRes::default().add_breakage(&db, &id, error))
}

#[instrument(skip(ctx))]
pub async fn uninstall_impl(ctx: RpcContext, id: PackageId) -> Result<(), Error> {
    let mut handle = ctx.db.handle();
    let mut tx = handle.begin().await?;
    // crate::db::DatabaseModel::new()
    //     .package_data()
    //     .idx_model(&id)
    //     .lock(&mut tx, LockType::Write)
    //     .await?;

    let mut pde = todo!(); /*crate::db::DatabaseModel::new()
                           .package_data()
                           .idx_model(&id)
                           .get_mut(&mut tx)
                           .await?;*/
    let (manifest, static_files, installed) = match pde.take() {
        Some(PackageDataEntry::Installed {
            manifest,
            static_files,
            installed,
        }) => (manifest, static_files, installed),
        _ => {
            return Err(Error::new(
                eyre!("Package is not installed."),
                ErrorKind::NotFound,
            ));
        }
    };
    *pde = Some(PackageDataEntry::Removing {
        manifest,
        static_files,
        removing: installed,
    });
    pde.save(&mut tx).await?;
    tx.commit().await?;
    drop(handle);

    tokio::spawn(async move {
        if let Err(e) =
            async { cleanup::uninstall(&ctx, &mut ctx.secret_store.acquire().await?, &id).await }
                .await
        {
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

#[instrument(skip(ctx, temp_manifest, s9pk))]
pub async fn download_install_s9pk(
    ctx: &RpcContext,
    temp_manifest: &Manifest,
    marketplace_url: Option<Url>,
    progress: Arc<InstallProgress>,
    mut s9pk: impl AsyncRead + Unpin,
) -> Result<(), Error> {
    let pkg_id = &temp_manifest.id;
    let version = &temp_manifest.version;
    let mut previous_state: Option<MainStatus> = None;

    if let Err(e) = async {
        if todo!()
        /* crate::db::DatabaseModel::new()
        .package_data()
        .idx_model(&pkg_id)
        .and_then(|x| x.installed())
        .exists(&mut ctx.db.handle())
        .await
        .unwrap_or(false) */
        {
            previous_state = crate::control::stop_impl(ctx.clone(), pkg_id.clone())
                .await
                .ok();
        }
        let mut db_handle = ctx.db.handle();
        let mut tx = db_handle.begin().await?;
        // Build set of existing manifests
        let mut manifests = Vec::new();
        for pkg in crate::db::package::get_packages(&ctx.db).await? {
            if let Some(m) = crate::db::package::get_manifest(&ctx.db, &pkg).await? {
                manifests.push(m);
            }
        }
        // Build map of current port -> ssl mappings
        let port_map = ssl_port_status(&manifests);
        tracing::info!("SSL Port Map: {:?}", &port_map);

        // if any of the requested interface lan configs conflict with current state, fail the install
        for (_id, iface) in &temp_manifest.interfaces.0 {
            if let Some(cfg) = &iface.lan_config {
                for (p, lan) in cfg {
                    if p.0 == 80 && lan.ssl || p.0 == 443 && !lan.ssl {
                        return Err(Error::new(
                            eyre!("SSL Conflict with embassyOS"),
                            ErrorKind::LanPortConflict,
                        ));
                    }
                    match port_map.get(&p) {
                        Some((ssl, pkg)) => {
                            if *ssl != lan.ssl {
                                return Err(Error::new(
                                    eyre!("SSL Conflict with package: {}", pkg),
                                    ErrorKind::LanPortConflict,
                                ));
                            }
                        }
                        None => {
                            continue;
                        }
                    }
                }
            }
        }
        tx.save().await?;
        drop(db_handle);

        let pkg_archive_dir = ctx
            .datadir
            .join(PKG_ARCHIVE_DIR)
            .join(pkg_id)
            .join(version.as_str());
        tokio::fs::create_dir_all(&pkg_archive_dir).await?;
        let pkg_archive =
            pkg_archive_dir.join(AsRef::<Path>::as_ref(pkg_id).with_extension("s9pk"));

        let pkg_data_entry = todo!(); /* crate::db::DatabaseModel::new()
                                      .package_data()
                                      .idx_model(pkg_id);*/

        let progress_model = pkg_data_entry.and_then(|pde| pde.install_progress());

        File::delete(&pkg_archive).await?;
        let mut dst = OpenOptions::new()
            .create(true)
            .write(true)
            .read(true)
            .open(&pkg_archive)
            .await?;

        progress
            .track_download_during(progress_model.clone(), &ctx.db, || async {
                let mut progress_writer = InstallProgressTracker::new(&mut dst, progress.clone());
                tokio::io::copy(&mut s9pk, &mut progress_writer).await?;
                progress.download_complete();
                Ok(())
            })
            .await?;

        dst.seek(SeekFrom::Start(0)).await?;

        let progress_reader = InstallProgressTracker::new(dst, progress.clone());
        let mut s9pk_reader = progress
            .track_read_during(progress_model.clone(), &ctx.db, || {
                S9pkReader::from_reader(progress_reader, true)
            })
            .await?;

        install_s9pk(
            &ctx,
            pkg_id,
            version,
            marketplace_url,
            &mut s9pk_reader,
            progress,
        )
        .await?;

        Ok(())
    }
    .await
    {
        if previous_state.map(|x| x.running()).unwrap_or(false) {
            crate::control::start(ctx.clone(), pkg_id.clone()).await?;
        }

        let mut handle = ctx.db.handle();
        let mut tx = handle.begin().await?;
        let receipts = todo!(); //cleanup::CleanupFailedReceipts::new(&mut tx).await?;

        if let Err(e) = cleanup_failed(&ctx, pkg_id).await {
            tracing::error!("Failed to clean up {}@{}: {}", pkg_id, version, e);
            tracing::debug!("{:?}", e);
        } else {
            tx.commit().await?;
        }
        Err(e)
    } else {
        if previous_state.map(|x| x.running()).unwrap_or(false) {
            crate::control::start(ctx.clone(), pkg_id.clone()).await?;
        }
        Ok(())
    }
}

// #[instrument(skip(ctx, rdr))]
pub async fn install_s9pk<R: AsyncRead + AsyncSeek + Unpin + Send + Sync>(
    ctx: &RpcContext,
    pkg_id: &PackageId,
    version: &Version,
    marketplace_url: Option<Url>,
    rdr: &mut S9pkReader<InstallProgressTracker<R>>,
    progress: Arc<InstallProgress>,
) -> Result<(), Error> {
    rdr.validate().await?;
    rdr.validated();
    let developer_key = rdr.developer_key().clone();
    rdr.reset().await?;
    let model = todo!(); /* crate::db::DatabaseModel::new()
                         .package_data()
                         .idx_model(pkg_id); */
    let progress_model = model.clone().and_then(|m| m.install_progress());

    tracing::info!("Install {}@{}: Unpacking Manifest", pkg_id, version);
    let manifest = progress
        .track_read_during(progress_model.clone(), &ctx.db, || rdr.manifest())
        .await?;
    tracing::info!("Install {}@{}: Unpacked Manifest", pkg_id, version);

    tracing::info!("Install {}@{}: Fetching Dependency Info", pkg_id, version);
    let mut dependency_info = BTreeMap::new();
    for (dep, info) in &manifest.dependencies.0 {
        let manifest: Option<Manifest> = if let Some(local_man) = todo!()
        /* crate::db::DatabaseModel::new()
        .package_data()
        .idx_model(dep)
        .map::<_, Manifest>(|pde| pde.manifest())
        .get(&mut ctx.db.handle())
        .await?
        .into_owned()*/
        {
            Some(local_man)
        } else if let Some(marketplace_url) = &marketplace_url {
            match reqwest::get(format!(
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
            .with_kind(ErrorKind::Registry)?
        } else {
            None
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
                icon: if let Some(manifest) = &manifest {
                    format!(
                        "/public/package-data/{}/{}/icon.{}",
                        manifest.id,
                        manifest.version,
                        manifest.assets.icon_type()
                    )
                } else {
                    "/assets/img/package-icon.png".to_owned()
                },
                manifest,
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
    progress
        .track_read_during(progress_model.clone(), &ctx.db, || async {
            let license_path = public_dir_path.join("LICENSE.md");
            let mut dst = File::create(&license_path).await?;
            tokio::io::copy(&mut rdr.license().await?, &mut dst).await?;
            dst.sync_all().await?;
            Ok(())
        })
        .await?;
    tracing::info!("Install {}@{}: Unpacked LICENSE.md", pkg_id, version);

    tracing::info!("Install {}@{}: Unpacking INSTRUCTIONS.md", pkg_id, version);
    progress
        .track_read_during(progress_model.clone(), &ctx.db, || async {
            let instructions_path = public_dir_path.join("INSTRUCTIONS.md");
            let mut dst = File::create(&instructions_path).await?;
            tokio::io::copy(&mut rdr.instructions().await?, &mut dst).await?;
            dst.sync_all().await?;
            Ok(())
        })
        .await?;
    tracing::info!("Install {}@{}: Unpacked INSTRUCTIONS.md", pkg_id, version);

    let icon_path = Path::new("icon").with_extension(&manifest.assets.icon_type());
    tracing::info!(
        "Install {}@{}: Unpacking {}",
        pkg_id,
        version,
        icon_path.display()
    );
    progress
        .track_read_during(progress_model.clone(), &ctx.db, || async {
            let icon_path = public_dir_path.join(&icon_path);
            let mut dst = File::create(&icon_path).await?;
            tokio::io::copy(&mut rdr.icon().await?, &mut dst).await?;
            dst.sync_all().await?;
            Ok(())
        })
        .await?;
    tracing::info!(
        "Install {}@{}: Unpacked {}",
        pkg_id,
        version,
        icon_path.display()
    );

    tracing::info!("Install {}@{}: Unpacking Docker Images", pkg_id, version);
    progress
        .track_read_during(progress_model.clone(), &ctx.db, || async {
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
    progress
        .track_read_during(progress_model.clone(), &ctx.db, || async {
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

            Ok(())
        })
        .await?;
    tracing::info!("Install {}@{}: Unpacked Assets", pkg_id, version);

    progress.unpack_complete.store(true, Ordering::SeqCst);

    progress_model.put(&mut ctx.db.handle(), &progress).await?;

    let mut handle = ctx.db.handle();
    let mut tx = handle.begin().await?;
    let mut sql_tx = ctx.secret_store.begin().await?;

    tracing::info!("Install {}@{}: Creating volumes", pkg_id, version);
    manifest.volumes.install(ctx, pkg_id, version).await?;
    tracing::info!("Install {}@{}: Created volumes", pkg_id, version);

    tracing::info!("Install {}@{}: Installing interfaces", pkg_id, version);
    let interface_addresses = manifest.interfaces.install(&mut sql_tx, pkg_id).await?;
    tracing::info!("Install {}@{}: Installed interfaces", pkg_id, version);

    tracing::info!("Install {}@{}: Creating manager", pkg_id, version);
    ctx.managers
        .add(ctx.clone(), manifest.clone(), marketplace_url)
        .await?;
    tracing::info!("Install {}@{}: Created manager", pkg_id, version);

    let static_files = StaticFiles::local(pkg_id, version, manifest.assets.icon_type());
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
    let mut pde = model
        .clone()
        .expect(&mut tx)
        .await?
        .get_mut(&mut tx)
        .await?;
    let installed = InstalledPackageInfo {
        status: Status {
            configured: manifest.config.is_none(),
            main: MainStatus::Stopped,
            dependency_errors: DependencyErrors::default(),
        },
        marketplace_url,
        developer_key,
        manifest: manifest.clone(),
        last_backup: match &*pde {
            PackageDataEntry::Updating {
                installed:
                    InstalledPackageInfo {
                        last_backup: Some(time),
                        ..
                    },
                ..
            } => Some(*time),
            _ => None,
        },
        dependency_info,
        current_dependencies: current_dependencies.clone(),
        interface_addresses,
    };

    let prev = std::mem::replace(
        &mut *pde,
        PackageDataEntry::Installed {
            installed,
            manifest: manifest.clone(),
            static_files,
        },
    );
    pde.save(&mut tx).await?;
    let mut dep_errs = model
        .expect(&mut tx)
        .await?
        .installed()
        .expect(&mut tx)
        .await?
        .status()
        .dependency_errors()
        .get_mut(&mut tx)
        .await?;
    *dep_errs = DependencyErrors::init(ctx, &manifest, &current_dependencies).await?;
    dep_errs.save(&mut tx).await?;

    if let PackageDataEntry::Updating {
        installed: prev, ..
    } = prev
    {
        let prev_is_configured = prev.status.configured;
        let prev_migration = prev
            .manifest
            .migrations
            .to(
                ctx,
                version,
                pkg_id,
                &prev.manifest.version,
                &prev.manifest.volumes,
            )
            .map(futures::future::Either::Left);
        let migration = manifest
            .migrations
            .from(
                &manifest.containers,
                ctx,
                &prev.manifest.version,
                pkg_id,
                version,
                &manifest.volumes,
            )
            .map(futures::future::Either::Right);

        let viable_migration = if prev.manifest.version > manifest.version {
            prev_migration.or(migration)
        } else {
            migration.or(prev_migration)
        };

        let configured = if let Some(f) = viable_migration {
            f.await?.configured && prev_is_configured
        } else {
            false
        };
        if configured && manifest.config.is_some() {
            let breakages = BTreeMap::new();
            let overrides = Default::default();

            let configure_context = ConfigureContext {
                breakages,
                timeout: None,
                config: None,
                dry_run: false,
                overrides,
            };
            crate::config::configure(&ctx, pkg_id, configure_context).await?;
        } else {
            remove_from_current_dependents_lists(pkg_id, &prev.current_dependencies, &db).await?;
            // remove previous
        }
        if configured || manifest.config.is_none() {
            let mut main_status = todo!(); /*crate::db::DatabaseModel::new()
                                           .package_data()
                                           .idx_model(pkg_id)
                                           .expect(&mut tx)
                                           .await?
                                           .installed()
                                           .expect(&mut tx)
                                           .await?
                                           .status()
                                           .main()
                                           .get_mut(&mut tx)
                                           .await?;*/
            *main_status = prev.status.main;
            main_status.save(&mut tx).await?;
        }
        update_dependency_errors_of_dependents(ctx, pkg_id, &todo!()).await?;
        if &prev.manifest.version != version {
            cleanup(ctx, &prev.manifest.id, &prev.manifest.version).await?;
        }
    } else if let PackageDataEntry::Restoring { .. } = prev {
        manifest
            .backup
            .restore(
                ctx,
                pkg_id,
                version,
                &manifest.interfaces,
                &manifest.volumes,
            )
            .await?;
        update_dependency_errors_of_dependents(ctx, pkg_id, &todo!()).await?;
    } else {
        update_dependency_errors_of_dependents(ctx, pkg_id, &todo!()).await?;
    }

    sql_tx.commit().await?;
    tx.commit().await?;

    tracing::info!("Install {}@{}: Complete", pkg_id, version);

    Ok(())
}

#[instrument(skip(datadir))]
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
                                Some("s9pk") => {
                                    copy_and_shutdown(
                                        &mut S9pkReader::open(&path, false)
                                            .await?
                                            .docker_images()
                                            .await?,
                                        load_in,
                                    )
                                    .await?
                                }
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

fn ssl_port_status(manifests: &Vec<Manifest>) -> BTreeMap<Port, (bool, PackageId)> {
    let mut ret = BTreeMap::new();
    for m in manifests {
        for (_id, iface) in &m.interfaces.0 {
            match &iface.lan_config {
                None => {}
                Some(cfg) => {
                    for (p, lan) in cfg {
                        ret.insert(p.clone(), (lan.ssl, m.id.clone()));
                    }
                }
            }
        }
    }
    ret
}
