use std::collections::{BTreeMap, BTreeSet};
use std::io::SeekFrom;
use std::marker::PhantomData;
use std::path::{Path, PathBuf};
use std::process::Stdio;
use std::sync::atomic::Ordering;
use std::sync::Arc;
use std::time::{Duration, Instant};

use color_eyre::eyre::eyre;
use emver::VersionRange;
use futures::future::BoxFuture;
use futures::{FutureExt, StreamExt, TryStreamExt};
use http::header::CONTENT_LENGTH;
use http::{Request, Response, StatusCode};
use hyper::Body;
use patch_db::{DbHandle, LockType};
use rpc_toolkit::yajrc::RpcError;
use rpc_toolkit::{command, Context};
use tokio::fs::{File, OpenOptions};
use tokio::io::{AsyncRead, AsyncSeek, AsyncSeekExt};
use tokio::process::Command;
use tokio_stream::wrappers::ReadDirStream;
use tracing::instrument;

use self::cleanup::{cleanup_failed, remove_current_dependents};
use crate::context::{CliContext, RpcContext};
use crate::core::rpc_continuations::{RequestGuid, RpcContinuation};
use crate::db::model::{
    CurrentDependencyInfo, InstalledPackageDataEntry, PackageDataEntry, RecoveredPackageInfo,
    StaticDependencyInfo, StaticFiles,
};
use crate::db::util::WithRevision;
use crate::dependencies::{
    add_current_dependents, break_all_dependents_transitive, BreakageRes, DependencyError,
    DependencyErrors,
};
use crate::install::cleanup::{cleanup, update_dependents};
use crate::install::progress::{InstallProgress, InstallProgressTracker};
use crate::notifications::NotificationLevel;
use crate::s9pk::manifest::{Manifest, PackageId};
use crate::s9pk::reader::S9pkReader;
use crate::status::{MainStatus, Status};
use crate::util::io::copy_and_shutdown;
use crate::util::serde::{display_serializable, IoFormat};
use crate::util::{display_none, AsyncFileExt, Version};
use crate::version::{Current, VersionT};
use crate::volume::asset_dir;
use crate::{Error, ErrorKind, ResultExt};

pub mod cleanup;
pub mod progress;
pub mod update;

pub const PKG_ARCHIVE_DIR: &'static str = "package-data/archive";
pub const PKG_PUBLIC_DIR: &'static str = "package-data/public";
pub const PKG_DOCKER_DIR: &'static str = "package-data/docker";
pub const PKG_WASM_DIR: &'static str = "package-data/wasm";

#[command(display(display_serializable))]
pub async fn list(#[context] ctx: RpcContext) -> Result<Vec<(PackageId, Version)>, Error> {
    let mut hdl = ctx.db.handle();
    let package_data = crate::db::DatabaseModel::new()
        .package_data()
        .get(&mut hdl, true)
        .await?;

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

#[command(
    custom_cli(cli_install(async, context(CliContext))),
    display(display_none)
)]
#[instrument(skip(ctx))]
pub async fn install(
    #[context] ctx: RpcContext,
    #[arg] id: String,
    #[arg(rename = "version-spec")] version_spec: Option<String>,
) -> Result<WithRevision<()>, Error> {
    let version_str = match &version_spec {
        None => "*",
        Some(v) => &*v,
    };
    let version: VersionRange = version_str.parse()?;
    let reg_url = ctx.package_registry_url().await?;
    let (man_res, s9pk) = tokio::try_join!(
        reqwest::get(format!(
            "{}/package/manifest/{}?spec={}&eos-version-compat={}&arch={}",
            reg_url,
            id,
            version,
            Current::new().compat(),
            platforms::TARGET_ARCH,
        )),
        reqwest::get(format!(
            "{}/package/{}.s9pk?spec={}&eos-version-compat={}&arch={}",
            reg_url,
            id,
            version,
            Current::new().compat(),
            platforms::TARGET_ARCH,
        ))
    )
    .with_kind(crate::ErrorKind::Registry)?;
    let man: Manifest = man_res.json().await.with_kind(crate::ErrorKind::Registry)?;

    let progress = InstallProgress::new(s9pk.content_length());
    let static_files = StaticFiles::remote(&man.id, &man.version);
    let mut db_handle = ctx.db.handle();
    let mut tx = db_handle.begin().await?;
    let mut pde = crate::db::DatabaseModel::new()
        .package_data()
        .idx_model(&man.id)
        .get_mut(&mut tx)
        .await?;
    match pde.take() {
        Some(PackageDataEntry::Installed {
            installed,
            manifest,
            static_files,
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
                crate::ErrorKind::InvalidRequest,
            ))
        }
    }
    pde.save(&mut tx).await?;
    let res = tx.commit(None).await?;
    drop(db_handle);

    tokio::spawn(async move {
        let mut db_handle = ctx.db.handle();
        if let Err(e) = download_install_s9pk(
            &ctx,
            &man,
            InstallProgress::new(s9pk.content_length()),
            tokio_util::io::StreamReader::new(s9pk.bytes_stream().map_err(|e| {
                std::io::Error::new(
                    if e.is_connect() {
                        std::io::ErrorKind::ConnectionRefused
                    } else if e.is_timeout() {
                        std::io::ErrorKind::TimedOut
                    } else {
                        std::io::ErrorKind::Other
                    },
                    e,
                )
            })),
        )
        .await
        {
            let err_str = format!("Install of {}@{} Failed: {}", man.id, man.version, e);
            tracing::error!("{}", err_str);
            tracing::debug!("{:?}", e);
            if let Err(e) = ctx
                .notification_manager
                .notify(
                    &mut db_handle,
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

    Ok(WithRevision {
        revision: res,
        response: (),
    })
}

#[command(rpc_only, display(display_none))]
#[instrument(skip(ctx))]
pub async fn sideload(
    #[context] ctx: RpcContext,
    #[arg] manifest: Manifest,
) -> Result<RequestGuid, Error> {
    let new_ctx = ctx.clone();
    let guid = RequestGuid::new();
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

            let mut pde = crate::db::DatabaseModel::new()
                .package_data()
                .idx_model(&manifest.id)
                .get_mut(&mut tx)
                .await?;
            match pde.take() {
                Some(PackageDataEntry::Installed {
                    installed,
                    manifest,
                    static_files,
                }) => {
                    *pde = Some(PackageDataEntry::Updating {
                        install_progress: progress.clone(),
                        installed,
                        manifest,
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
                        crate::ErrorKind::InvalidRequest,
                    ))
                }
            }
            pde.save(&mut tx).await?;
            tx.commit(None).await?;
            drop(hdl);

            download_install_s9pk(
                &new_ctx,
                &manifest,
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
            .await?;
            Response::builder()
                .status(StatusCode::OK)
                .body(Body::empty())
                .with_kind(ErrorKind::Network)
        }
        .boxed()
    });
    let cont = RpcContinuation {
        created_at: Instant::now(), // TODO
        handler: handler,
    };
    // gc the map
    let mut guard = ctx.rpc_stream_continuations.lock().await;
    let gced = std::mem::take(&mut *guard)
        .into_iter()
        .filter(|(_, v)| v.created_at.elapsed() < Duration::from_secs(30))
        .collect::<BTreeMap<RequestGuid, RpcContinuation>>();
    *guard = gced;
    drop(guard);
    // insert the new continuation
    ctx.rpc_stream_continuations
        .lock()
        .await
        .insert(guid.clone(), cont);
    Ok(guid)
}

#[instrument(skip(ctx))]
async fn cli_install(
    ctx: CliContext,
    target: String,
    version_spec: Option<String>,
) -> Result<(), RpcError> {
    if target.ends_with(".s9pk") {
        let path = PathBuf::from(target);

        // inspect manifest no verify
        let manifest = crate::inspect::manifest(path.clone(), true, Some(IoFormat::Json)).await?;

        // rpc call remote sideload
        tracing::debug!("calling package.sideload");
        let guid = rpc_toolkit::command_helpers::call_remote(
            ctx.clone(),
            "package.sideload",
            serde_json::json!({ "manifest": manifest }),
            PhantomData::<RequestGuid>,
        )
        .await?
        .result?;
        tracing::debug!("package.sideload succeeded {:?}", guid);

        // hit continuation api with guid that comes back
        let file = tokio::fs::File::open(path).await?;
        let content_length = file.metadata().await?.len();
        let body = Body::wrap_stream(tokio_util::io::ReaderStream::new(file));
        let client = reqwest::Client::new();
        let res = client
            .post(format!(
                "{}://{}/rest/rpc/{}",
                ctx.protocol(),
                ctx.host(),
                guid
            ))
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
            (Some((pkg, v)), None) => serde_json::json!({ "id": pkg, "version-spec": v }),
            (Some((pkg, v)), Some(_)) => {
                return Err(crate::Error::new(
                    eyre!("Invalid package id {}", target),
                    ErrorKind::InvalidRequest,
                )
                .into())
            }
            (None, Some(v)) => serde_json::json!({ "id": target, "version-spec": v }),
            (None, None) => serde_json::json!({ "id": target }),
        };
        tracing::debug!("calling package.install");
        rpc_toolkit::command_helpers::call_remote(
            ctx,
            "package.install",
            params,
            PhantomData::<WithRevision<()>>,
        )
        .await?
        .result?;
        tracing::debug!("package.install succeeded");
    }
    Ok(())
}

#[command(
    subcommands(self(uninstall_impl(async)), uninstall_dry),
    display(display_none)
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
    let mut db = ctx.db.handle();
    let mut tx = db.begin().await?;
    let mut breakages = BTreeMap::new();
    break_all_dependents_transitive(&mut tx, &id, DependencyError::NotInstalled, &mut breakages)
        .await?;

    tx.abort().await?;

    Ok(BreakageRes(breakages))
}

#[instrument(skip(ctx))]
pub async fn uninstall_impl(ctx: RpcContext, id: PackageId) -> Result<WithRevision<()>, Error> {
    let mut handle = ctx.db.handle();
    let mut tx = handle.begin().await?;

    let mut pde = crate::db::DatabaseModel::new()
        .package_data()
        .idx_model(&id)
        .get_mut(&mut tx)
        .await?;
    let (manifest, static_files, installed) = match pde.take() {
        Some(PackageDataEntry::Installed {
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
    *pde = Some(PackageDataEntry::Removing {
        manifest,
        static_files,
        removing: installed,
    });
    pde.save(&mut tx).await?;
    let res = tx.commit(None).await?;
    drop(handle);

    tokio::spawn(async move {
        if let Err(e) = cleanup::uninstall(&ctx, &mut ctx.db.handle(), &id).await {
            let err_str = format!("Uninstall of {} Failed: {}", id, e);
            tracing::error!("{}", err_str);
            tracing::debug!("{:?}", e);
            if let Err(e) = ctx
                .notification_manager
                .notify(
                    &mut ctx.db.handle(), // allocating separate handle here because the lifetime of the previous one is the expression
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

    Ok(WithRevision {
        revision: res,
        response: (),
    })
}

#[instrument(skip(ctx, temp_manifest, s9pk))]
pub async fn download_install_s9pk(
    ctx: &RpcContext,
    temp_manifest: &Manifest,
    progress: Arc<InstallProgress>,
    mut s9pk: impl AsyncRead + Unpin,
) -> Result<(), Error> {
    let pkg_id = &temp_manifest.id;
    let version = &temp_manifest.version;

    let pkg_archive_dir = ctx
        .datadir
        .join(PKG_ARCHIVE_DIR)
        .join(pkg_id)
        .join(version.as_str());
    tokio::fs::create_dir_all(&pkg_archive_dir).await?;
    let pkg_archive = pkg_archive_dir.join(AsRef::<Path>::as_ref(pkg_id).with_extension("s9pk"));

    let pkg_data_entry = crate::db::DatabaseModel::new()
        .package_data()
        .idx_model(pkg_id);

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

    install_s9pk_or_cleanup(&ctx, pkg_id, version, &mut s9pk_reader, progress).await?;

    Ok(())
}

#[instrument(skip(ctx, rdr))]
pub async fn install_s9pk_or_cleanup<R: AsyncRead + AsyncSeek + Unpin>(
    ctx: &RpcContext,
    pkg_id: &PackageId,
    version: &Version,
    rdr: &mut S9pkReader<InstallProgressTracker<R>>,
    progress: Arc<InstallProgress>,
) -> Result<(), Error> {
    if let Err(e) = install_s9pk(ctx, pkg_id, version, rdr, progress).await {
        let mut handle = ctx.db.handle();
        let mut tx = handle.begin().await?;

        if let Err(e) = cleanup_failed(&ctx, &mut tx, pkg_id).await {
            let mut tx = handle.begin().await?;
            tracing::error!(
                "Failed to clean up {}@{}: {}: Adding to broken packages",
                pkg_id,
                version,
                e
            );
            tracing::debug!("{:?}", e);
            let mut broken = crate::db::DatabaseModel::new()
                .broken_packages()
                .get_mut(&mut tx)
                .await?;
            broken.push(pkg_id.clone());
            broken.save(&mut tx).await?;
            tx.commit(None).await?;
        } else {
            tx.commit(None).await?;
        }
        Err(e)
    } else {
        Ok(())
    }
}

#[instrument(skip(ctx, rdr))]
pub async fn install_s9pk<R: AsyncRead + AsyncSeek + Unpin>(
    ctx: &RpcContext,
    pkg_id: &PackageId,
    version: &Version,
    rdr: &mut S9pkReader<InstallProgressTracker<R>>,
    progress: Arc<InstallProgress>,
) -> Result<(), Error> {
    rdr.validate().await?;
    rdr.validated();
    rdr.reset().await?;
    let model = crate::db::DatabaseModel::new()
        .package_data()
        .idx_model(pkg_id);
    let progress_model = model.clone().and_then(|m| m.install_progress());

    tracing::info!("Install {}@{}: Unpacking Manifest", pkg_id, version);
    let manifest = progress
        .track_read_during(progress_model.clone(), &ctx.db, || rdr.manifest())
        .await?;
    tracing::info!("Install {}@{}: Unpacked Manifest", pkg_id, version);

    tracing::info!("Install {}@{}: Fetching Dependency Info", pkg_id, version);
    let mut dependency_info = BTreeMap::new();
    let reg_url = ctx.package_registry_url().await?;
    for (dep, info) in &manifest.dependencies.0 {
        let manifest: Option<Manifest> = match reqwest::get(format!(
            "{}/package/manifest/{}?spec={}&eos-version-compat={}&arch={}",
            reg_url,
            dep,
            info.version,
            Current::new().compat(),
            platforms::TARGET_ARCH,
        ))
        .await
        .with_kind(crate::ErrorKind::Registry)?
        .error_for_status()
        {
            Ok(a) => Ok(Some(
                a.json()
                    .await
                    .with_kind(crate::ErrorKind::Deserialization)?,
            )),
            Err(e) if e.status() == Some(StatusCode::BAD_REQUEST) => Ok(None),
            Err(e) => Err(e),
        }
        .with_kind(crate::ErrorKind::Registry)?;
        if let Some(manifest) = manifest {
            let dir = ctx
                .datadir
                .join(PKG_PUBLIC_DIR)
                .join(&manifest.id)
                .join(manifest.version.as_str());
            let icon_path = dir.join(format!("icon.{}", manifest.assets.icon_type()));
            if tokio::fs::metadata(&icon_path).await.is_err() {
                tokio::fs::create_dir_all(&dir).await?;
                let icon = reqwest::get(format!(
                    "{}/package/icon/{}?spec={}&eos-version-compat={}&arch={}",
                    reg_url,
                    dep,
                    info.version,
                    Current::new().compat(),
                    platforms::TARGET_ARCH,
                ))
                .await
                .with_kind(crate::ErrorKind::Registry)?;
                let mut dst = File::create(&icon_path).await?;
                tokio::io::copy(
                    &mut tokio_util::io::StreamReader::new(icon.bytes_stream().map_err(|e| {
                        std::io::Error::new(
                            if e.is_connect() {
                                std::io::ErrorKind::ConnectionRefused
                            } else if e.is_timeout() {
                                std::io::ErrorKind::TimedOut
                            } else {
                                std::io::ErrorKind::Other
                            },
                            e,
                        )
                    })),
                    &mut dst,
                )
                .await?;
                dst.sync_all().await?;
            }

            dependency_info.insert(
                dep.clone(),
                StaticDependencyInfo {
                    icon: format!(
                        "/public/package-data/{}/{}/icon.{}",
                        manifest.id,
                        manifest.version,
                        manifest.assets.icon_type()
                    ),
                    manifest: Some(manifest),
                },
            );
        }
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
            let image_tar_dir = ctx
                .datadir
                .join(PKG_DOCKER_DIR)
                .join(pkg_id)
                .join(version.as_str());
            if tokio::fs::metadata(&image_tar_dir).await.is_err() {
                tokio::fs::create_dir_all(&image_tar_dir)
                    .await
                    .with_ctx(|_| {
                        (
                            crate::ErrorKind::Filesystem,
                            image_tar_dir.display().to_string(),
                        )
                    })?;
            }
            let image_tar_path = image_tar_dir.join("image.tar");
            let mut tee = Command::new("tee")
                .arg(&image_tar_path)
                .stdin(Stdio::piped())
                .stdout(Stdio::piped())
                .spawn()?;
            let mut load = Command::new("docker")
                .arg("load")
                .stdin(Stdio::piped())
                .stderr(Stdio::piped())
                .spawn()?;
            let tee_in = tee.stdin.take().ok_or_else(|| {
                Error::new(
                    eyre!("Could not write to stdin of tee"),
                    crate::ErrorKind::Docker,
                )
            })?;
            let mut tee_out = tee.stdout.take().ok_or_else(|| {
                Error::new(
                    eyre!("Could not read from stdout of tee"),
                    crate::ErrorKind::Docker,
                )
            })?;
            let load_in = load.stdin.take().ok_or_else(|| {
                Error::new(
                    eyre!("Could not write to stdin of docker load"),
                    crate::ErrorKind::Docker,
                )
            })?;
            let mut docker_rdr = rdr.docker_images().await?;
            tokio::try_join!(
                copy_and_shutdown(&mut docker_rdr, tee_in),
                copy_and_shutdown(&mut tee_out, load_in),
            )?;
            let res = load.wait_with_output().await?;
            if !res.status.success() {
                Err(Error::new(
                    eyre!(
                        "{}",
                        String::from_utf8(res.stderr)
                            .unwrap_or_else(|e| format!("Could not parse stderr: {}", e))
                    ),
                    crate::ErrorKind::Docker,
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

            Ok(())
        })
        .await?;
    tracing::info!("Install {}@{}: Unpacked Assets", pkg_id, version);

    progress.unpack_complete.store(true, Ordering::SeqCst);

    progress_model.put(&mut ctx.db.handle(), &progress).await?;

    let mut handle = ctx.db.handle();
    let mut tx = handle.begin().await?;
    let mut sql_tx = ctx.secret_store.begin().await?;
    crate::db::DatabaseModel::new()
        .package_data()
        .lock(&mut tx, LockType::Write)
        .await;

    tracing::info!("Install {}@{}: Creating volumes", pkg_id, version);
    manifest.volumes.install(ctx, pkg_id, version).await?;
    tracing::info!("Install {}@{}: Created volumes", pkg_id, version);

    tracing::info!("Install {}@{}: Installing interfaces", pkg_id, version);
    let interface_addresses = manifest.interfaces.install(&mut sql_tx, pkg_id).await?;
    tracing::info!("Install {}@{}: Installed interfaces", pkg_id, version);

    tracing::info!("Install {}@{}: Creating manager", pkg_id, version);
    ctx.managers
        .add(
            ctx.clone(),
            manifest.clone(),
            manifest.interfaces.tor_keys(&mut sql_tx, pkg_id).await?,
        )
        .await?;
    tracing::info!("Install {}@{}: Created manager", pkg_id, version);

    let static_files = StaticFiles::local(pkg_id, version, manifest.assets.icon_type());
    let current_dependencies: BTreeMap<_, _> = manifest
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
        .collect();
    let current_dependents = {
        // search required dependencies
        let mut deps = BTreeMap::new();
        for package in crate::db::DatabaseModel::new()
            .package_data()
            .keys(&mut tx, true)
            .await?
        {
            if let Some(dep) = crate::db::DatabaseModel::new()
                .package_data()
                .idx_model(&package)
                .expect(&mut tx)
                .await?
                .installed()
                .and_then(|i| i.current_dependencies().idx_model(pkg_id))
                .get(&mut tx, true)
                .await?
                .to_owned()
            {
                deps.insert(package, dep);
            }
        }
        deps
    };
    let mut pde = model
        .clone()
        .expect(&mut tx)
        .await?
        .get_mut(&mut tx)
        .await?;
    let installed = InstalledPackageDataEntry {
        status: Status {
            configured: manifest.config.is_none(),
            main: MainStatus::Stopped,
            dependency_errors: DependencyErrors::default(),
        },
        manifest: manifest.clone(),
        last_backup: match &*pde {
            PackageDataEntry::Updating {
                installed:
                    InstalledPackageDataEntry {
                        last_backup: Some(time),
                        ..
                    },
                ..
            } => Some(*time),
            _ => None,
        },
        system_pointers: Vec::new(),
        dependency_info,
        current_dependents: current_dependents.clone(),
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
    *dep_errs = DependencyErrors::init(ctx, &mut tx, &manifest, &current_dependencies).await?;
    dep_errs.save(&mut tx).await?;

    if let PackageDataEntry::Updating {
        installed: prev,
        manifest: prev_manifest,
        ..
    } = prev
    {
        let prev_is_configured = prev.status.configured;
        let prev_migration = prev_manifest
            .migrations
            .to(
                ctx,
                version,
                pkg_id,
                &prev_manifest.version,
                &prev_manifest.volumes,
            )
            .map(futures::future::Either::Left);
        let migration = manifest
            .migrations
            .from(
                ctx,
                &prev_manifest.version,
                pkg_id,
                version,
                &manifest.volumes,
            )
            .map(futures::future::Either::Right);

        let viable_migration = if prev_manifest.version > manifest.version {
            prev_migration.or(migration)
        } else {
            migration.or(prev_migration)
        };

        let configured = prev_is_configured
            && if let Some(f) = viable_migration {
                f.await?.configured
            } else {
                false
            };
        if configured && manifest.config.is_some() {
            crate::config::configure(
                ctx,
                &mut tx,
                pkg_id,
                None,
                &None,
                false,
                &mut BTreeMap::new(),
                &mut BTreeMap::new(),
            )
            .await?;
            let mut main_status = crate::db::DatabaseModel::new()
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
                .await?;
            *main_status = prev.status.main;
            main_status.save(&mut tx).await?;
        }
        remove_current_dependents(&mut tx, pkg_id, prev.current_dependencies.keys()).await?; // remove previous
        add_current_dependents(&mut tx, pkg_id, &current_dependencies).await?; // add new
        update_dependents(
            ctx,
            &mut tx,
            pkg_id,
            current_dependents
                .keys()
                .chain(prev.current_dependents.keys())
                .collect::<BTreeSet<_>>(),
        )
        .await?;
        if &prev.manifest.version != version {
            cleanup(ctx, &prev.manifest.id, &prev.manifest.version).await?;
        }
    } else if let PackageDataEntry::Restoring { .. } = prev {
        manifest
            .backup
            .restore(
                ctx,
                &mut tx,
                &mut sql_tx,
                pkg_id,
                version,
                &manifest.interfaces,
                &manifest.volumes,
            )
            .await?;
        add_current_dependents(&mut tx, pkg_id, &current_dependencies).await?;
        update_dependents(ctx, &mut tx, pkg_id, current_dependents.keys()).await?;
    } else if let Some(recovered) = crate::db::DatabaseModel::new()
        .recovered_packages()
        .idx_model(pkg_id)
        .get(&mut tx, true)
        .await?
        .into_owned()
    {
        handle_recovered_package(recovered, manifest, ctx, pkg_id, version, &mut tx).await?;
        add_current_dependents(&mut tx, pkg_id, &current_dependencies).await?;
        update_dependents(ctx, &mut tx, pkg_id, current_dependents.keys()).await?;
    } else {
        add_current_dependents(&mut tx, pkg_id, &current_dependencies).await?;
        update_dependents(ctx, &mut tx, pkg_id, current_dependents.keys()).await?;
    }

    crate::db::DatabaseModel::new()
        .recovered_packages()
        .remove(&mut tx, pkg_id)
        .await?;

    sql_tx.commit().await?;
    tx.commit(None).await?;

    tracing::info!("Install {}@{}: Complete", pkg_id, version);

    Ok(())
}

#[instrument(skip(ctx, tx))]
async fn handle_recovered_package(
    recovered: RecoveredPackageInfo,
    manifest: Manifest,
    ctx: &RpcContext,
    pkg_id: &PackageId,
    version: &Version,
    tx: &mut patch_db::Transaction<&mut patch_db::PatchDbHandle>,
) -> Result<(), Error> {
    let configured = if let Some(migration) =
        manifest
            .migrations
            .from(ctx, &recovered.version, pkg_id, version, &manifest.volumes)
    {
        migration.await?.configured
    } else {
        false
    };
    if configured && manifest.config.is_some() {
        crate::config::configure(
            ctx,
            tx,
            pkg_id,
            None,
            &None,
            false,
            &mut BTreeMap::new(),
            &mut BTreeMap::new(),
        )
        .await?;
    }

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
                .map(|r| {
                    r.with_ctx(|_| (crate::ErrorKind::Filesystem, format!("{:?}", &docker_dir)))
                })
                .try_for_each(|entry| async move {
                    let m = entry.metadata().await?;
                    if m.is_file() {
                        if entry.path().extension().and_then(|ext| ext.to_str()) == Some("tar") {
                            let mut load = Command::new("docker")
                                .arg("load")
                                .stdin(Stdio::piped())
                                .stderr(Stdio::piped())
                                .spawn()?;
                            let load_in = load.stdin.take().ok_or_else(|| {
                                Error::new(
                                    eyre!("Could not write to stdin of docker load"),
                                    crate::ErrorKind::Docker,
                                )
                            })?;
                            let mut docker_rdr = File::open(&entry.path()).await?;
                            copy_and_shutdown(&mut docker_rdr, load_in).await?;
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
                                    crate::ErrorKind::Docker,
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
