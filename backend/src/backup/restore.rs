use std::collections::BTreeMap;
use std::path::Path;
use std::sync::atomic::Ordering;
use std::sync::Arc;
use std::time::Duration;

use clap::ArgMatches;
use color_eyre::eyre::eyre;
use futures::future::BoxFuture;
use futures::FutureExt;
use openssl::x509::X509;
use patch_db::{DbHandle, PatchDbHandle};
use rpc_toolkit::command;
use tokio::fs::File;
use tokio::task::JoinHandle;
use torut::onion::OnionAddressV3;
use tracing::instrument;

use super::target::BackupTargetId;
use crate::backup::backup_bulk::OsBackup;
use crate::backup::BackupMetadata;
use crate::context::rpc::RpcContextConfig;
use crate::context::{RpcContext, SetupContext};
use crate::db::model::{PackageDataEntry, StaticFiles};
use crate::disk::mount::backup::{BackupMountGuard, PackageBackupMountGuard};
use crate::disk::mount::filesystem::ReadWrite;
use crate::disk::mount::guard::TmpMountGuard;
use crate::hostname::{get_hostname, Hostname};
use crate::init::init;
use crate::install::progress::InstallProgress;
use crate::install::{download_install_s9pk, PKG_PUBLIC_DIR};
use crate::net::ssl::SslManager;
use crate::notifications::NotificationLevel;
use crate::s9pk::manifest::{Manifest, PackageId};
use crate::s9pk::reader::S9pkReader;
use crate::setup::SetupStatus;
use crate::util::display_none;
use crate::util::io::dir_size;
use crate::util::serde::IoFormat;
use crate::volume::{backup_dir, BACKUP_DIR, PKG_VOLUME_DIR};
use crate::{Error, ResultExt};

fn parse_comma_separated(arg: &str, _: &ArgMatches) -> Result<Vec<PackageId>, Error> {
    arg.split(',')
        .map(|s| s.trim().parse().map_err(Error::from))
        .collect()
}

#[command(rename = "restore", display(display_none))]
#[instrument(skip(ctx, password))]
pub async fn restore_packages_rpc(
    #[context] ctx: RpcContext,
    #[arg(parse(parse_comma_separated))] ids: Vec<PackageId>,
    #[arg(rename = "target-id")] target_id: BackupTargetId,
    #[arg] password: String,
) -> Result<(), Error> {
    let mut db = ctx.db.handle();
    let fs = target_id
        .load(&mut ctx.secret_store.acquire().await?)
        .await?;
    let backup_guard =
        BackupMountGuard::mount(TmpMountGuard::mount(&fs, ReadWrite).await?, &password).await?;

    let (backup_guard, tasks, _) = restore_packages(&ctx, &mut db, backup_guard, ids).await?;

    tokio::spawn(async move {
        let res = futures::future::join_all(tasks).await;
        for res in res {
            match res.with_kind(crate::ErrorKind::Unknown) {
                Ok((Ok(_), _)) => (),
                Ok((Err(err), package_id)) => {
                    if let Err(err) = ctx
                        .notification_manager
                        .notify(
                            &mut db,
                            Some(package_id.clone()),
                            NotificationLevel::Error,
                            "Restoration Failure".to_string(),
                            format!("Error restoring package {}: {}", package_id, err),
                            (),
                            None,
                        )
                        .await
                    {
                        tracing::error!("Failed to notify: {}", err);
                        tracing::debug!("{:?}", err);
                    };
                    tracing::error!("Error restoring package {}: {}", package_id, err);
                    tracing::debug!("{:?}", err);
                }
                Err(e) => {
                    if let Err(err) = ctx
                        .notification_manager
                        .notify(
                            &mut db,
                            None,
                            NotificationLevel::Error,
                            "Restoration Failure".to_string(),
                            format!("Error during restoration: {}", e),
                            (),
                            None,
                        )
                        .await
                    {
                        tracing::error!("Failed to notify: {}", err);
                        tracing::debug!("{:?}", err);
                    }
                    tracing::error!("Error restoring packages: {}", e);
                    tracing::debug!("{:?}", e);
                }
            }
        }
        if let Err(e) = backup_guard.unmount().await {
            tracing::error!("Error unmounting backup drive: {}", e);
            tracing::debug!("{:?}", e);
        }
    });

    Ok(())
}

async fn approximate_progress(
    rpc_ctx: &RpcContext,
    progress: &mut ProgressInfo,
) -> Result<(), Error> {
    for (id, size) in &mut progress.target_volume_size {
        let dir = rpc_ctx.datadir.join(PKG_VOLUME_DIR).join(id).join("data");
        if tokio::fs::metadata(&dir).await.is_err() {
            *size = 0;
        } else {
            *size = dir_size(&dir).await?;
        }
    }
    Ok(())
}

async fn approximate_progress_loop(
    ctx: &SetupContext,
    rpc_ctx: &RpcContext,
    mut starting_info: ProgressInfo,
) {
    loop {
        if let Err(e) = approximate_progress(rpc_ctx, &mut starting_info).await {
            tracing::error!("Failed to approximate restore progress: {}", e);
            tracing::debug!("{:?}", e);
        } else {
            *ctx.setup_status.write().await = Some(Ok(starting_info.flatten()));
        }
        tokio::time::sleep(Duration::from_secs(1)).await;
    }
}

#[derive(Debug, Default)]
struct ProgressInfo {
    package_installs: BTreeMap<PackageId, Arc<InstallProgress>>,
    src_volume_size: BTreeMap<PackageId, u64>,
    target_volume_size: BTreeMap<PackageId, u64>,
}
impl ProgressInfo {
    fn flatten(&self) -> SetupStatus {
        let mut total_bytes = 0;
        let mut bytes_transferred = 0;

        for progress in self.package_installs.values() {
            total_bytes += ((progress.size.unwrap_or(0) as f64) * 2.2) as u64;
            bytes_transferred += progress.downloaded.load(Ordering::SeqCst);
            bytes_transferred += ((progress.validated.load(Ordering::SeqCst) as f64) * 0.2) as u64;
            bytes_transferred += progress.unpacked.load(Ordering::SeqCst);
        }

        for size in self.src_volume_size.values() {
            total_bytes += *size;
        }

        for size in self.target_volume_size.values() {
            bytes_transferred += *size;
        }

        if bytes_transferred > total_bytes {
            bytes_transferred = total_bytes;
        }

        SetupStatus {
            total_bytes: Some(total_bytes),
            bytes_transferred,
            complete: false,
        }
    }
}

#[instrument(skip(ctx))]
pub async fn recover_full_embassy(
    ctx: SetupContext,
    disk_guid: Arc<String>,
    embassy_password: String,
    recovery_source: TmpMountGuard,
    recovery_password: Option<String>,
) -> Result<(Arc<String>, Hostname, OnionAddressV3, X509), Error> {
    let backup_guard = BackupMountGuard::mount(
        recovery_source,
        recovery_password.as_deref().unwrap_or_default(),
    )
    .await?;

    let os_backup_path = backup_guard.as_ref().join("os-backup.cbor");
    let os_backup: OsBackup =
        IoFormat::Cbor.from_slice(&tokio::fs::read(&os_backup_path).await.with_ctx(|_| {
            (
                crate::ErrorKind::Filesystem,
                os_backup_path.display().to_string(),
            )
        })?)?;

    let password = argon2::hash_encoded(
        embassy_password.as_bytes(),
        &rand::random::<[u8; 16]>()[..],
        &argon2::Config::default(),
    )
    .with_kind(crate::ErrorKind::PasswordHashGeneration)?;
    let key_vec = os_backup.tor_key.as_bytes().to_vec();
    let secret_store = ctx.secret_store().await?;
    sqlx::query!(
        "INSERT INTO account (id, password, tor_key) VALUES ($1, $2, $3) ON CONFLICT (id) DO UPDATE SET password = $2, tor_key = $3",
        0,
        password,
        key_vec,
    )
    .execute(&mut secret_store.acquire().await?)
    .await?;

    SslManager::import_root_ca(
        secret_store.clone(),
        os_backup.root_ca_key,
        os_backup.root_ca_cert.clone(),
    )
    .await?;
    secret_store.close().await;

    let cfg = RpcContextConfig::load(ctx.config_path.clone()).await?;

    init(&cfg).await?;

    let rpc_ctx = RpcContext::init(ctx.config_path.clone(), disk_guid.clone()).await?;
    let mut db = rpc_ctx.db.handle();

    let receipts = crate::hostname::HostNameReceipt::new(&mut db).await?;
    let hostname = get_hostname(&mut db, &receipts).await?;

    drop(db);
    let mut db = rpc_ctx.db.handle();

    let ids = backup_guard
        .metadata
        .package_backups
        .keys()
        .cloned()
        .collect();
    let (backup_guard, tasks, progress_info) =
        restore_packages(&rpc_ctx, &mut db, backup_guard, ids).await?;

    tokio::select! {
        res = futures::future::join_all(tasks) => {
            for res in res {
                match res.with_kind(crate::ErrorKind::Unknown) {
                    Ok((Ok(_), _)) => (),
                    Ok((Err(err), package_id)) => {
                        if let Err(err) = rpc_ctx.notification_manager.notify(
                            &mut db,
                            Some(package_id.clone()),
                            NotificationLevel::Error,
                            "Restoration Failure".to_string(), format!("Error restoring package {}: {}", package_id,err), (), None).await{
                            tracing::error!("Failed to notify: {}", err);
                            tracing::debug!("{:?}", err);
                            };
                        tracing::error!("Error restoring package {}: {}", package_id, err);
                        tracing::debug!("{:?}", err);
                    },
                    Err(e) => {
                        if let Err(err) = rpc_ctx.notification_manager.notify(
                            &mut db,
                            None,
                            NotificationLevel::Error,
                            "Restoration Failure".to_string(), format!("Error during restoration: {}", e), (), None).await {

                            tracing::error!("Failed to notify: {}", err);
                            tracing::debug!("{:?}", err);
                        }
                        tracing::error!("Error restoring packages: {}", e);
                        tracing::debug!("{:?}", e);
                    },

                }
            }
        },
        _ = approximate_progress_loop(&ctx, &rpc_ctx, progress_info) => unreachable!(concat!(module_path!(), "::approximate_progress_loop should not terminate")),
    }

    backup_guard.unmount().await?;
    rpc_ctx.shutdown().await?;

    Ok((
        disk_guid,
        hostname,
        os_backup.tor_key.public().get_onion_address(),
        os_backup.root_ca_cert,
    ))
}

async fn restore_packages(
    ctx: &RpcContext,
    db: &mut PatchDbHandle,
    backup_guard: BackupMountGuard<TmpMountGuard>,
    ids: Vec<PackageId>,
) -> Result<
    (
        BackupMountGuard<TmpMountGuard>,
        Vec<JoinHandle<(Result<(), Error>, PackageId)>>,
        ProgressInfo,
    ),
    Error,
> {
    let guards = assure_restoring(ctx, db, ids, &backup_guard).await?;

    let mut progress_info = ProgressInfo::default();

    let mut tasks = Vec::with_capacity(guards.len());
    for (manifest, guard) in guards {
        let id = manifest.id.clone();
        let (progress, task) = restore_package(ctx.clone(), manifest, guard).await?;
        progress_info.package_installs.insert(id.clone(), progress);
        progress_info
            .src_volume_size
            .insert(id.clone(), dir_size(backup_dir(&id)).await?);
        progress_info.target_volume_size.insert(id.clone(), 0);
        let package_id = id.clone();
        tasks.push(tokio::spawn(
            async move {
                if let Err(e) = task.await {
                    tracing::error!("Error restoring package {}: {}", id, e);
                    tracing::debug!("{:?}", e);
                    Err(e)
                } else {
                    Ok(())
                }
            }
            .map(|x| (x, package_id)),
        ));
    }

    Ok((backup_guard, tasks, progress_info))
}

#[instrument(skip(ctx, db, backup_guard))]
async fn assure_restoring(
    ctx: &RpcContext,
    db: &mut PatchDbHandle,
    ids: Vec<PackageId>,
    backup_guard: &BackupMountGuard<TmpMountGuard>,
) -> Result<Vec<(Manifest, PackageBackupMountGuard)>, Error> {
    let mut tx = db.begin().await?;

    let mut guards = Vec::with_capacity(ids.len());

    for id in ids {
        let mut model = crate::db::DatabaseModel::new()
            .package_data()
            .idx_model(&id)
            .get_mut(&mut tx)
            .await?;

        if !model.is_none() {
            return Err(Error::new(
                eyre!("Can't restore over existing package: {}", id),
                crate::ErrorKind::InvalidRequest,
            ));
        }

        let guard = backup_guard.mount_package_backup(&id).await?;
        let s9pk_path = Path::new(BACKUP_DIR).join(&id).join(format!("{}.s9pk", id));
        let mut rdr = S9pkReader::open(&s9pk_path, false).await?;

        let manifest = rdr.manifest().await?;
        let version = manifest.version.clone();
        let progress = InstallProgress::new(Some(tokio::fs::metadata(&s9pk_path).await?.len()));

        let public_dir_path = ctx
            .datadir
            .join(PKG_PUBLIC_DIR)
            .join(&id)
            .join(version.as_str());
        tokio::fs::create_dir_all(&public_dir_path).await?;

        let license_path = public_dir_path.join("LICENSE.md");
        let mut dst = File::create(&license_path).await?;
        tokio::io::copy(&mut rdr.license().await?, &mut dst).await?;
        dst.sync_all().await?;

        let instructions_path = public_dir_path.join("INSTRUCTIONS.md");
        let mut dst = File::create(&instructions_path).await?;
        tokio::io::copy(&mut rdr.instructions().await?, &mut dst).await?;
        dst.sync_all().await?;

        let icon_path = Path::new("icon").with_extension(&manifest.assets.icon_type());
        let icon_path = public_dir_path.join(&icon_path);
        let mut dst = File::create(&icon_path).await?;
        tokio::io::copy(&mut rdr.icon().await?, &mut dst).await?;
        dst.sync_all().await?;

        *model = Some(PackageDataEntry::Restoring {
            install_progress: progress.clone(),
            static_files: StaticFiles::local(&id, &version, manifest.assets.icon_type()),
            manifest: manifest.clone(),
        });
        model.save(&mut tx).await?;

        guards.push((manifest, guard));
    }

    tx.commit().await?;
    Ok(guards)
}

#[instrument(skip(ctx, guard))]
async fn restore_package<'a>(
    ctx: RpcContext,
    manifest: Manifest,
    guard: PackageBackupMountGuard,
) -> Result<(Arc<InstallProgress>, BoxFuture<'static, Result<(), Error>>), Error> {
    let id = manifest.id.clone();
    let s9pk_path = Path::new(BACKUP_DIR)
        .join(&manifest.id)
        .join(format!("{}.s9pk", id));

    let metadata_path = Path::new(BACKUP_DIR).join(&id).join("metadata.cbor");
    let metadata: BackupMetadata =
        IoFormat::Cbor.from_slice(&tokio::fs::read(&metadata_path).await.with_ctx(|_| {
            (
                crate::ErrorKind::Filesystem,
                metadata_path.display().to_string(),
            )
        })?)?;
    for (iface, key) in metadata.tor_keys {
        let key_vec = base32::decode(base32::Alphabet::RFC4648 { padding: true }, &key)
            .ok_or_else(|| {
                Error::new(
                    eyre!("invalid base32 string"),
                    crate::ErrorKind::Deserialization,
                )
            })?;
        sqlx::query!(
                "INSERT INTO tor (package, interface, key) VALUES ($1, $2, $3) ON CONFLICT (package, interface) DO UPDATE SET key = $3",
                *id,
                *iface,
                key_vec,
            )
            .execute(&ctx.secret_store)
            .await?;
    }

    let len = tokio::fs::metadata(&s9pk_path)
        .await
        .with_ctx(|_| {
            (
                crate::ErrorKind::Filesystem,
                s9pk_path.display().to_string(),
            )
        })?
        .len();
    let file = File::open(&s9pk_path).await.with_ctx(|_| {
        (
            crate::ErrorKind::Filesystem,
            s9pk_path.display().to_string(),
        )
    })?;

    let progress = InstallProgress::new(Some(len));

    Ok((
        progress.clone(),
        async move {
            download_install_s9pk(&ctx, &manifest, None, progress, file).await?;

            guard.unmount().await?;

            Ok(())
        }
        .boxed(),
    ))
}
