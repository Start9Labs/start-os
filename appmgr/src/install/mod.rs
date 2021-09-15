use std::collections::HashSet;
use std::io::SeekFrom;
use std::path::Path;
use std::process::Stdio;
use std::sync::atomic::Ordering;
use std::sync::Arc;

use anyhow::anyhow;
use emver::VersionRange;
use futures::TryStreamExt;
use http::StatusCode;
use indexmap::IndexMap;
use patch_db::DbHandle;
use reqwest::Response;
use rpc_toolkit::command;
use tokio::fs::{File, OpenOptions};
use tokio::io::{AsyncRead, AsyncSeek, AsyncSeekExt};
use tokio::process::Command;

use self::cleanup::cleanup_failed;
use crate::context::RpcContext;
use crate::db::model::{
    CurrentDependencyInfo, InstalledPackageDataEntry, PackageDataEntry, StaticDependencyInfo,
    StaticFiles,
};
use crate::db::util::WithRevision;
use crate::dependencies::update_current_dependents;
use crate::install::cleanup::{cleanup, update_dependents};
use crate::install::progress::{InstallProgress, InstallProgressTracker};
use crate::s9pk::manifest::{Manifest, PackageId};
use crate::s9pk::reader::S9pkReader;
use crate::status::{DependencyErrors, MainStatus, Status};
use crate::util::io::copy_and_shutdown;
use crate::util::{display_none, AsyncFileExt, Version};
use crate::volume::asset_dir;
use crate::{Error, ResultExt};

pub mod cleanup;
pub mod progress;

pub const PKG_CACHE: &'static str = "package-data/cache";
pub const PKG_PUBLIC_DIR: &'static str = "package-data/public";
pub const PKG_DOCKER_DIR: &'static str = "package-data/docker";
pub const PKG_WASM_DIR: &'static str = "package-data/wasm";

#[command(display(display_none))]
pub async fn install(
    #[context] ctx: RpcContext,
    #[arg] id: String,
) -> Result<WithRevision<()>, Error> {
    let (pkg_id, version_str) = if let Some(split) = id.split_once("@") {
        split
    } else {
        (id.as_str(), "*")
    };
    let version: VersionRange = version_str.parse()?;
    let reg_url = ctx.package_registry_url().await?;
    let (man_res, s9pk) = tokio::try_join!(
        reqwest::get(format!(
            "{}/package/manifest/{}?version={}",
            reg_url, pkg_id, version
        )),
        reqwest::get(format!(
            "{}/package/{}.s9pk?version={}",
            reg_url, pkg_id, version
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
                manifest,
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
                anyhow!("Cannot install over a package in a transient state"),
                crate::ErrorKind::InvalidRequest,
            ))
        }
    }
    pde.save(&mut tx).await?;
    let res = tx.commit(None).await?;
    drop(db_handle);

    tokio::spawn(async move {
        if let Err(e) = download_install_s9pk(&ctx, &man, s9pk).await {
            log::error!("Install of {}@{} Failed: {}", man.id, man.version, e);
        }
    });

    Ok(WithRevision {
        revision: res,
        response: (),
    })
}

#[command(display(display_none))]
pub async fn uninstall(
    #[context] ctx: RpcContext,
    #[arg] id: PackageId,
) -> Result<WithRevision<()>, Error> {
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
                anyhow!("Package is not installed."),
                crate::ErrorKind::NotFound,
            ));
        }
    };
    *pde = Some(PackageDataEntry::Removing {
        manifest,
        static_files,
    });
    pde.save(&mut tx).await?;
    let res = tx.commit(None).await?;
    drop(handle);

    tokio::spawn(async move {
        if let Err(e) = cleanup::uninstall(&ctx, &mut ctx.db.handle(), &installed).await {
            log::error!("Uninstall of {} Failed: {}", id, e);
        }
    });

    Ok(WithRevision {
        revision: res,
        response: (),
    })
}

pub async fn download_install_s9pk(
    ctx: &RpcContext,
    temp_manifest: &Manifest,
    s9pk: Response,
) -> Result<(), Error> {
    let pkg_id = &temp_manifest.id;
    let version = &temp_manifest.version;

    let pkg_cache_dir = ctx
        .datadir
        .join(PKG_CACHE)
        .join(pkg_id)
        .join(version.as_str());
    tokio::fs::create_dir_all(&pkg_cache_dir).await?;
    let pkg_cache = AsRef::<Path>::as_ref(pkg_id).with_extension("s9pk");

    let pkg_data_entry = crate::db::DatabaseModel::new()
        .package_data()
        .idx_model(pkg_id);

    let res = (|| async {
        let progress = InstallProgress::new(s9pk.content_length());
        let progress_model = pkg_data_entry.and_then(|pde| pde.install_progress());

        File::delete(&pkg_cache).await?;
        let mut dst = OpenOptions::new()
            .create(true)
            .write(true)
            .read(true)
            .open(&pkg_cache)
            .await?;

        progress
            .track_download_during(progress_model.clone(), &ctx.db, || async {
                let mut progress_writer = InstallProgressTracker::new(&mut dst, progress.clone());
                tokio::io::copy(
                    &mut tokio_util::io::StreamReader::new(s9pk.bytes_stream().map_err(|e| {
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
                    &mut progress_writer,
                )
                .await?;
                progress.download_complete();
                Ok(())
            })
            .await?;

        dst.seek(SeekFrom::Start(0)).await?;

        let progress_reader = InstallProgressTracker::new(dst, progress.clone());
        let mut s9pk_reader = progress
            .track_read_during(progress_model.clone(), &ctx.db, || {
                S9pkReader::from_reader(progress_reader)
            })
            .await?;

        install_s9pk(&ctx, pkg_id, version, &mut s9pk_reader, progress).await?;

        Ok(())
    })()
    .await;

    if let Err(e) = res {
        let mut handle = ctx.db.handle();
        let mut tx = handle.begin().await?;

        if let Err(e) = cleanup_failed(&ctx, &mut tx, pkg_id, version).await {
            let mut tx = handle.begin().await?;
            log::error!(
                "Failed to clean up {}@{}: {}: Adding to broken packages",
                pkg_id,
                version,
                e
            );
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

pub async fn install_s9pk<R: AsyncRead + AsyncSeek + Unpin>(
    ctx: &RpcContext,
    pkg_id: &PackageId,
    version: &Version,
    rdr: &mut S9pkReader<InstallProgressTracker<R>>,
    progress: Arc<InstallProgress>,
) -> Result<(), Error> {
    rdr.validate().await?;
    rdr.validated();
    let model = crate::db::DatabaseModel::new()
        .package_data()
        .idx_model(pkg_id)
        .check(&mut ctx.db.handle())
        .await?
        .ok_or_else(|| {
            Error::new(
                anyhow!("PackageDataEntry does not exist"),
                crate::ErrorKind::Database,
            )
        })?;
    let progress_model = model.clone().install_progress();

    log::info!("Install {}@{}: Unpacking Manifest", pkg_id, version);
    let manifest = progress
        .track_read_during(progress_model.clone(), &ctx.db, || rdr.manifest())
        .await?;
    log::info!("Install {}@{}: Unpacked Manifest", pkg_id, version);

    log::info!("Install {}@{}: Fetching Dependency Info", pkg_id, version);
    let mut dependency_info = IndexMap::with_capacity(manifest.dependencies.0.len());
    let reg_url = ctx.package_registry_url().await?;
    for (dep, info) in &manifest.dependencies.0 {
        let manifest: Option<Manifest> = match reqwest::get(format!(
            "{}/package/manifest/{}?version={}",
            reg_url, dep, info.version
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
                    "{}/package/icon/{}?version={}",
                    reg_url, dep, info.version
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
    log::info!("Install {}@{}: Fetched Dependency Info", pkg_id, version);

    let public_dir_path = ctx
        .datadir
        .join(PKG_PUBLIC_DIR)
        .join(pkg_id)
        .join(version.as_str());
    tokio::fs::create_dir_all(&public_dir_path).await?;

    log::info!("Install {}@{}: Unpacking LICENSE.md", pkg_id, version);
    progress
        .track_read_during(progress_model.clone(), &ctx.db, || async {
            let license_path = public_dir_path.join("LICENSE.md");
            let mut dst = File::create(&license_path).await?;
            tokio::io::copy(&mut rdr.license().await?, &mut dst).await?;
            dst.sync_all().await?;
            Ok(())
        })
        .await?;
    log::info!("Install {}@{}: Unpacked LICENSE.md", pkg_id, version);

    log::info!("Install {}@{}: Unpacking INSTRUCTIONS.md", pkg_id, version);
    progress
        .track_read_during(progress_model.clone(), &ctx.db, || async {
            let instructions_path = public_dir_path.join("INSTRUCTIONS.md");
            let mut dst = File::create(&instructions_path).await?;
            tokio::io::copy(&mut rdr.instructions().await?, &mut dst).await?;
            dst.sync_all().await?;
            Ok(())
        })
        .await?;
    log::info!("Install {}@{}: Unpacked INSTRUCTIONS.md", pkg_id, version);

    let icon_path = Path::new("icon").with_extension(&manifest.assets.icon_type());
    log::info!(
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
    log::info!(
        "Install {}@{}: Unpacked {}",
        pkg_id,
        version,
        icon_path.display()
    );

    log::info!("Install {}@{}: Unpacking Docker Images", pkg_id, version);
    progress
        .track_read_during(progress_model.clone(), &ctx.db, || async {
            let image_tar_dir = Path::new(PKG_DOCKER_DIR)
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
                    anyhow!("Could not write to stdin of tee"),
                    crate::ErrorKind::Docker,
                )
            })?;
            let mut tee_out = tee.stdout.take().ok_or_else(|| {
                Error::new(
                    anyhow!("Could not read from stdout of tee"),
                    crate::ErrorKind::Docker,
                )
            })?;
            let load_in = load.stdin.take().ok_or_else(|| {
                Error::new(
                    anyhow!("Could not write to stdin of docker load"),
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
                    anyhow!(
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
    log::info!("Install {}@{}: Unpacked Docker Images", pkg_id, version,);

    log::info!("Install {}@{}: Unpacking Assets", pkg_id, version);
    progress
        .track_read_during(progress_model.clone(), &ctx.db, || async {
            let asset_dir = asset_dir(ctx, pkg_id, version);
            if tokio::fs::metadata(&asset_dir).await.is_err() {
                tokio::fs::create_dir_all(&asset_dir).await?;
            }
            let mut tar = tokio_tar::Archive::new(rdr.assets().await?);
            tar.unpack(asset_dir).await?;

            Ok(())
        })
        .await?;
    log::info!("Install {}@{}: Unpacked Assets", pkg_id, version);

    progress.unpack_complete.store(true, Ordering::SeqCst);

    progress_model.put(&mut ctx.db.handle(), &progress).await?;

    let mut handle = ctx.db.handle();
    let mut tx = handle.begin().await?;
    let mut sql_tx = ctx.secret_store.begin().await?;
    crate::db::DatabaseModel::new()
        .package_data()
        .lock(&mut tx, patch_db::LockType::Write)
        .await;

    log::info!("Install {}@{}: Creating volumes", pkg_id, version);
    manifest.volumes.install(ctx, pkg_id, version).await?;
    log::info!("Install {}@{}: Created volumes", pkg_id, version);

    log::info!("Install {}@{}: Installing interfaces", pkg_id, version);
    let interface_addresses = manifest.interfaces.install(&mut sql_tx, pkg_id).await?;
    log::info!("Install {}@{}: Installed interfaces", pkg_id, version);

    log::info!("Install {}@{}: Creating manager", pkg_id, version);
    ctx.managers
        .add(
            ctx.clone(),
            manifest.clone(),
            manifest.interfaces.tor_keys(&mut sql_tx, pkg_id).await?,
        )
        .await?;
    log::info!("Install {}@{}: Created manager", pkg_id, version);

    let static_files = StaticFiles::local(pkg_id, version, manifest.assets.icon_type());
    let current_dependencies = manifest
        .dependencies
        .0
        .iter()
        .filter_map(|(id, info)| {
            if info.optional.is_none() {
                Some((id.clone(), CurrentDependencyInfo::default()))
            } else {
                None
            }
        })
        .collect();
    update_current_dependents(&mut tx, pkg_id, &current_dependencies).await?;
    let current_dependents = {
        // search required dependencies
        let mut deps = IndexMap::new();
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
    let installed = InstalledPackageDataEntry {
        status: Status {
            configured: manifest.config.is_none(),
            main: MainStatus::Stopped,
            dependency_errors: DependencyErrors::init(
                ctx,
                &mut tx,
                &manifest,
                &current_dependencies,
            )
            .await?,
        },
        manifest: manifest.clone(),
        system_pointers: Vec::new(),
        dependency_info,
        current_dependents: current_dependents.clone(),
        current_dependencies,
        interface_addresses,
    };
    let mut pde = model.get_mut(&mut tx).await?;
    let prev = std::mem::replace(
        &mut *pde,
        PackageDataEntry::Installed {
            installed,
            manifest: manifest.clone(),
            static_files,
        },
    );
    pde.save(&mut tx).await?;

    if let PackageDataEntry::Updating {
        installed: prev,
        manifest: prev_manifest,
        ..
    } = prev
    {
        update_dependents(
            ctx,
            &mut tx,
            pkg_id,
            current_dependents
                .keys()
                .chain(prev.current_dependents.keys())
                .collect::<HashSet<_>>(),
        )
        .await?;
        let mut configured = prev.status.configured;
        if let Some(res) = prev_manifest
            .migrations
            .to(
                ctx,
                version,
                pkg_id,
                &prev_manifest.version,
                &prev_manifest.volumes,
            )
            .await?
        {
            configured &= res.configured;
        }
        if &prev.manifest.version != version {
            cleanup(ctx, &prev.manifest.id, &prev.manifest.version).await?;
        }
        if let Some(res) = manifest
            .migrations
            .from(
                ctx,
                &prev_manifest.version,
                pkg_id,
                version,
                &manifest.volumes,
            )
            .await?
        {
            configured &= res.configured;
        }
        if configured {
            crate::config::configure(
                ctx,
                &mut tx,
                pkg_id,
                None,
                &None,
                false,
                &mut IndexMap::new(),
                &mut IndexMap::new(),
            )
            .await?;
            todo!("set as running if viable");
        }
    } else {
        update_dependents(ctx, &mut tx, pkg_id, current_dependents.keys()).await?;
    }

    sql_tx.commit().await?;
    tx.commit(None).await?;

    log::info!("Install {}@{}: Complete", pkg_id, version);

    Ok(())
}
