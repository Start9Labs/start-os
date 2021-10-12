use std::collections::BTreeMap;
use std::path::{Path, PathBuf};
use std::sync::atomic::{AtomicU64, Ordering};
use std::time::Duration;

use color_eyre::eyre::eyre;
use futures::future::BoxFuture;
use futures::{FutureExt, TryStreamExt};
use rpc_toolkit::command;
use rpc_toolkit::yajrc::RpcError;
use serde::{Deserialize, Serialize};
use tokio::fs::File;
use tokio::io::AsyncWriteExt;
use tokio::process::Command;
use torut::onion::TorSecretKeyV3;
use tracing::instrument;

use crate::context::SetupContext;
use crate::db::model::RecoveredPackageInfo;
use crate::disk::main::DEFAULT_PASSWORD;
use crate::disk::util::{mount, unmount, DiskInfo};
use crate::id::Id;
use crate::install::PKG_PUBLIC_DIR;
use crate::s9pk::manifest::PackageId;
use crate::sound::BEETHOVEN;
use crate::util::io::from_yaml_async_reader;
use crate::util::{GeneralGuard, Invoke, Version};
use crate::volume::{data_dir, VolumeId};
use crate::{Error, ResultExt};

#[command(subcommands(status, disk, execute, recovery))]
pub fn setup() -> Result<(), Error> {
    Ok(())
}

#[derive(Debug, Deserialize, Serialize)]
#[serde(rename_all = "kebab-case")]
pub struct StatusRes {
    product_key: bool,
    migrating: bool,
}

#[command(rpc_only, metadata(authenticated = false))]
pub async fn status(#[context] ctx: SetupContext) -> Result<StatusRes, Error> {
    Ok(StatusRes {
        product_key: tokio::fs::metadata("/embassy-os/product_key.txt")
            .await
            .is_ok(),
        migrating: ctx.recovery_status.read().await.is_some(), // TODO
    })
}

#[command(subcommands(list_disks))]
pub fn disk() -> Result<(), Error> {
    Ok(())
}

#[command(rename = "list", rpc_only, metadata(authenticated = false))]
pub async fn list_disks() -> Result<Vec<DiskInfo>, Error> {
    crate::disk::list(None).await
}

#[command(subcommands(recovery_status))]
pub fn recovery() -> Result<(), Error> {
    Ok(())
}

#[derive(Debug, Clone, Deserialize, Serialize)]
#[serde(rename_all = "kebab-case")]
pub struct RecoveryStatus {
    bytes_transferred: u64,
    total_bytes: u64,
}

#[command(rename = "status", rpc_only, metadata(authenticated = false))]
pub async fn recovery_status(
    #[context] ctx: SetupContext,
) -> Result<Option<RecoveryStatus>, RpcError> {
    ctx.recovery_status.read().await.clone().transpose()
}

#[command(rpc_only)]
pub async fn execute(
    #[context] ctx: SetupContext,
    #[arg(rename = "embassy-logicalname")] embassy_logicalname: PathBuf,
    #[arg(rename = "embassy-password")] embassy_password: String,
    #[arg(rename = "recovery-drive")] recovery_drive: Option<DiskInfo>,
    #[arg(rename = "recovery-password")] recovery_password: Option<String>,
) -> Result<String, Error> {
    match execute_inner(
        ctx,
        embassy_logicalname,
        embassy_password,
        recovery_drive,
        recovery_password,
    )
    .await
    {
        Ok(a) => {
            tracing::info!("Setup Successful! Tor Address: {}", a);
            Ok(a)
        }
        Err(e) => {
            tracing::error!("Error Setting Up Embassy: {}", e);
            tracing::debug!("{:?}", e);
            Err(e)
        }
    }
}

#[instrument(skip(ctx))]
pub async fn complete_setup(ctx: SetupContext, guid: String) -> Result<(), Error> {
    let mut guid_file = File::create("/embassy-os/disk.guid").await?;
    guid_file.write_all(guid.as_bytes()).await?;
    guid_file.sync_all().await?;
    ctx.shutdown.send(()).expect("failed to shutdown");
    Ok(())
}

#[instrument(skip(ctx))]
pub async fn execute_inner(
    ctx: SetupContext,
    embassy_logicalname: PathBuf,
    embassy_password: String,
    recovery_drive: Option<DiskInfo>,
    recovery_password: Option<String>,
) -> Result<String, Error> {
    if ctx.recovery_status.read().await.is_some() {
        return Err(Error::new(
            eyre!("Cannot execute setup while in recovery!"),
            crate::ErrorKind::InvalidRequest,
        ));
    }
    let guid =
        crate::disk::main::create(&ctx.zfs_pool_name, [embassy_logicalname], DEFAULT_PASSWORD)
            .await?;
    let search_string = format!("id: {}", guid);
    let mut ctr = 0;
    while {
        ctr += 1;
        ctr < 30 // 30s timeout
    } && !String::from_utf8(
        Command::new("zpool")
            .arg("import")
            .invoke(crate::ErrorKind::Zfs)
            .await?,
    )?
    .lines()
    .any(|line| line.trim() == &search_string)
    {
        tokio::time::sleep(Duration::from_secs(1)).await;
    }
    crate::disk::main::load(&guid, &ctx.zfs_pool_name, &ctx.datadir, DEFAULT_PASSWORD).await?;
    let password = argon2::hash_encoded(
        embassy_password.as_bytes(),
        &rand::random::<[u8; 16]>()[..],
        &argon2::Config::default(),
    )
    .with_kind(crate::ErrorKind::PasswordHashGeneration)?;
    let tor_key = TorSecretKeyV3::generate();
    let key_vec = tor_key.as_bytes().to_vec();
    let sqlite_pool = ctx.secret_store().await?;
    sqlx::query!(
        "INSERT OR REPLACE INTO account (id, password, tor_key) VALUES (?, ?, ?)",
        0,
        password,
        key_vec,
    )
    .execute(&mut sqlite_pool.acquire().await?)
    .await?;
    sqlite_pool.close().await;

    if let Some(recovery_drive) = recovery_drive {
        if recovery_drive
            .embassy_os
            .as_ref()
            .map(|v| &*v.version < &emver::Version::new(0, 2, 8, 0))
            .unwrap_or(true)
        {
            return Err(Error::new(eyre!("Unsupported version of EmbassyOS. Please update to at least 0.2.8 before recovering."), crate::ErrorKind::VersionIncompatible));
        }
        tokio::spawn(async move {
            if let Err(e) = recover(ctx.clone(), guid, recovery_drive, recovery_password).await {
                BEETHOVEN.play().await.unwrap_or_default(); // ignore error in playing the song
                tracing::error!("Error recovering drive!: {}", e);
                tracing::debug!("{:?}", e);
                *ctx.recovery_status.write().await = Some(Err(e.into()));
            }
        });
    } else {
        complete_setup(ctx, guid).await?;
    }

    Ok(tor_key.public().get_onion_address().to_string())
}

#[instrument(skip(ctx))]
async fn recover(
    ctx: SetupContext,
    guid: String,
    recovery_drive: DiskInfo,
    recovery_password: Option<String>,
) -> Result<(), Error> {
    let recovery_version = recovery_drive
        .embassy_os
        .as_ref()
        .map(|i| i.version.clone())
        .unwrap_or_default();
    if recovery_version.major() == 0 && recovery_version.minor() == 2 {
        recover_v2(&ctx, recovery_drive).await?;
    } else if recovery_version.major() == 0 && recovery_version.minor() == 3 {
        recover_v3(&ctx, recovery_drive, recovery_password).await?;
    } else {
        return Err(Error::new(
            eyre!("Unsupported version of EmbassyOS: {}", recovery_version),
            crate::ErrorKind::VersionIncompatible,
        ));
    }

    complete_setup(ctx, guid).await
}

fn dir_size<'a, P: AsRef<Path> + 'a + Send + Sync>(
    path: P,
    res: &'a AtomicU64,
) -> BoxFuture<'a, Result<(), std::io::Error>> {
    async move {
        tokio_stream::wrappers::ReadDirStream::new(tokio::fs::read_dir(path.as_ref()).await?)
            .try_for_each(|e| async move {
                let m = e.metadata().await?;
                if m.is_file() {
                    res.fetch_add(m.len(), Ordering::Relaxed);
                } else if m.is_dir() {
                    dir_size(e.path(), res).await?;
                }
                Ok(())
            })
            .await
    }
    .boxed()
}

fn dir_copy<'a, P0: AsRef<Path> + 'a + Send + Sync, P1: AsRef<Path> + 'a + Send + Sync>(
    src: P0,
    dst: P1,
    ctr: &'a AtomicU64,
) -> BoxFuture<'a, Result<(), Error>> {
    async move {
        let dst_path = dst.as_ref();
        tokio_stream::wrappers::ReadDirStream::new(tokio::fs::read_dir(src.as_ref()).await?)
            .map_err(|e| Error::new(e, crate::ErrorKind::Filesystem))
            .try_for_each(|e| async move {
                let m = e.metadata().await?;
                let src_path = e.path();
                let dst_path = dst_path.join(e.file_name());
                if m.is_file() {
                    tokio::fs::copy(&src_path, &dst_path).await.with_ctx(|_| {
                        (
                            crate::ErrorKind::Filesystem,
                            format!("{} -> {}", src_path.display(), dst_path.display()),
                        )
                    })?;
                    ctr.fetch_add(m.len(), Ordering::Relaxed);
                } else if m.is_dir() {
                    tokio::fs::create_dir_all(&dst_path).await.with_ctx(|_| {
                        (
                            crate::ErrorKind::Filesystem,
                            format!("mkdir {}", dst_path.display()),
                        )
                    })?;
                    tokio::fs::set_permissions(&dst_path, m.permissions())
                        .await
                        .with_ctx(|_| {
                            (
                                crate::ErrorKind::Filesystem,
                                format!("chmod {}", dst_path.display()),
                            )
                        })?;
                    dir_copy(src_path, dst_path, ctr).await?;
                } else if m.file_type().is_symlink() {
                    tokio::fs::symlink(
                        tokio::fs::read_link(&src_path).await.with_ctx(|_| {
                            (
                                crate::ErrorKind::Filesystem,
                                format!("readlink {}", src_path.display()),
                            )
                        })?,
                        &dst_path,
                    )
                    .await
                    .with_ctx(|_| {
                        (
                            crate::ErrorKind::Filesystem,
                            format!("{} -> {}", src_path.display(), dst_path.display()),
                        )
                    })?;
                    tokio::fs::set_permissions(&dst_path, m.permissions())
                        .await
                        .with_ctx(|_| {
                            (
                                crate::ErrorKind::Filesystem,
                                format!("chmod {}", dst_path.display()),
                            )
                        })?;
                }
                Ok(())
            })
            .await?;
        Ok(())
    }
    .boxed()
}

#[instrument(skip(ctx))]
async fn recover_v2(ctx: &SetupContext, recovery_drive: DiskInfo) -> Result<(), Error> {
    let tmp_mountpoint = Path::new("/mnt/recovery");
    mount(
        &recovery_drive
            .partitions
            .get(1)
            .ok_or_else(|| {
                Error::new(
                    eyre!("missing rootfs partition"),
                    crate::ErrorKind::Filesystem,
                )
            })?
            .logicalname,
        tmp_mountpoint,
    )
    .await?;
    let mount_guard = GeneralGuard::new(|| tokio::spawn(unmount(tmp_mountpoint)));

    let secret_store = ctx.secret_store().await?;
    let db = ctx.db(&secret_store).await?;
    let mut handle = db.handle();

    let apps_yaml_path = tmp_mountpoint.join("root").join("appmgr").join("apps.yaml");
    #[derive(Deserialize)]
    struct LegacyAppInfo {
        title: String,
        version: Version,
    }
    let packages: BTreeMap<PackageId, LegacyAppInfo> =
        from_yaml_async_reader(File::open(&apps_yaml_path).await.with_ctx(|_| {
            (
                crate::ErrorKind::Filesystem,
                apps_yaml_path.display().to_string(),
            )
        })?)
        .await?;

    let volume_path = tmp_mountpoint.join("root/volumes");
    let total_bytes = AtomicU64::new(0);
    for (pkg_id, _) in &packages {
        let volume_src_path = volume_path.join(&pkg_id);
        dir_size(&volume_src_path, &total_bytes)
            .await
            .with_ctx(|_| {
                (
                    crate::ErrorKind::Filesystem,
                    volume_src_path.display().to_string(),
                )
            })?;
    }
    let total_bytes = total_bytes.load(Ordering::SeqCst);
    *ctx.recovery_status.write().await = Some(Ok(RecoveryStatus {
        bytes_transferred: 0,
        total_bytes,
    }));
    let bytes_transferred = AtomicU64::new(0);
    let volume_id = VolumeId::Custom(Id::try_from("main".to_owned())?);
    for (pkg_id, info) in packages {
        let volume_src_path = volume_path.join(&pkg_id);
        let volume_dst_path = data_dir(&ctx.datadir, &pkg_id, &info.version, &volume_id);
        tokio::fs::create_dir_all(&volume_dst_path)
            .await
            .with_ctx(|_| {
                (
                    crate::ErrorKind::Filesystem,
                    volume_dst_path.display().to_string(),
                )
            })?;
        tokio::select!(
            res = dir_copy(
                &volume_src_path,
                &volume_dst_path,
                &bytes_transferred
            ) => res?,
            _ = async {
                loop {
                    tokio::time::sleep(Duration::from_secs(1)).await;
                    *ctx.recovery_status.write().await = Some(Ok(RecoveryStatus {
                        bytes_transferred: bytes_transferred.load(Ordering::Relaxed),
                        total_bytes,
                    }));
                }
            } => (),
        );
        let icon_leaf = AsRef::<Path>::as_ref(&pkg_id)
            .join(info.version.as_str())
            .join("icon.png");
        let icon_src_path = tmp_mountpoint
            .join("root/agent/icons")
            .join(format!("{}.png", pkg_id));
        let icon_dst_path = ctx.datadir.join(PKG_PUBLIC_DIR).join(&icon_leaf);
        if let Some(parent) = icon_dst_path.parent() {
            tokio::fs::create_dir_all(&parent)
                .await
                .with_ctx(|_| (crate::ErrorKind::Filesystem, parent.display().to_string()))?;
        }
        tokio::fs::copy(&icon_src_path, &icon_dst_path)
            .await
            .with_ctx(|_| {
                (
                    crate::ErrorKind::Filesystem,
                    format!("{} -> {}", icon_src_path.display(), icon_dst_path.display()),
                )
            })?;
        let icon_url = Path::new("/public/package-data").join(&icon_leaf);
        crate::db::DatabaseModel::new()
            .recovered_packages()
            .idx_model(&pkg_id)
            .put(
                &mut handle,
                &RecoveredPackageInfo {
                    title: info.title,
                    icon: icon_url.display().to_string(),
                    version: info.version,
                },
            )
            .await?;
    }

    mount_guard
        .drop()
        .await
        .with_kind(crate::ErrorKind::Unknown)?
}

#[instrument(skip(ctx))]
async fn recover_v3(
    ctx: &SetupContext,
    recovery_drive: DiskInfo,
    recovery_password: Option<String>,
) -> Result<(), Error> {
    todo!()
}
