use std::collections::BTreeMap;
use std::path::Path;
use std::sync::atomic::Ordering;
use std::sync::Arc;
use std::time::Duration;

use clap::{ArgMatches, Parser};
use futures::future::BoxFuture;
use futures::{stream, FutureExt, StreamExt};
use models::PackageId;
use openssl::x509::X509;
use serde::{Deserialize, Serialize};
use sqlx::Connection;
use tokio::fs::File;
use torut::onion::OnionAddressV3;
use tracing::instrument;

use super::target::BackupTargetId;
use crate::backup::os::OsBackup;
use crate::backup::BackupMetadata;
use crate::context::{RpcContext, SetupContext};
use crate::db::model::{PackageDataEntry, PackageDataEntryRestoring, StaticFiles};
use crate::disk::mount::backup::{BackupMountGuard, PackageBackupMountGuard};
use crate::disk::mount::filesystem::ReadWrite;
use crate::disk::mount::guard::TmpMountGuard;
use crate::hostname::Hostname;
use crate::init::init;
use crate::install::progress::InstallProgress;
use crate::install::PKG_PUBLIC_DIR;
use crate::notifications::NotificationLevel;
use crate::prelude::*;
use crate::s9pk::manifest::Manifest;
use crate::s9pk::S9pk;
use crate::service::Service;
use crate::setup::SetupStatus;
use crate::util::io::dir_size;
use crate::util::serde::IoFormat;
use crate::volume::{backup_dir, BACKUP_DIR, PKG_VOLUME_DIR};

fn parse_comma_separated(arg: &str, _: &ArgMatches) -> Result<Vec<PackageId>, Error> {
    arg.split(',')
        .map(|s| s.trim().parse().map_err(Error::from))
        .collect()
}

#[derive(Deserialize, Serialize, Parser)]
#[serde(rename_all = "kebab-case")]
#[command(rename_all = "kebab-case")]
pub struct RestorePackageParams {
    pub ids: Vec<PackageId>,
    pub target_id: BackupTargetId,
    pub password: String,
}

// TODO dr Why doesn't anything use this
// #[command(rename = "restore", display(display_none))]
#[instrument(skip(ctx, password))]
pub async fn restore_packages_rpc(
    ctx: RpcContext,
    RestorePackageParams {
        ids,
        target_id,
        password,
    }: RestorePackageParams,
) -> Result<(), Error> {
    let fs = target_id
        .load(ctx.secret_store.acquire().await?.as_mut())
        .await?;
    let backup_guard =
        BackupMountGuard::mount(TmpMountGuard::mount(&fs, ReadWrite).await?, &password).await?;

    let (backup_guard, tasks, _) = restore_packages(&ctx, backup_guard, ids).await?;

    tokio::spawn(async move {
        stream::iter(tasks.into_iter().map(|x| (x, ctx.clone())))
            .for_each_concurrent(5, |(res, ctx)| async move {
                match res.await {
                    (Ok(_), _) => (),
                    (Err(err), package_id) => {
                        if let Err(err) = ctx
                            .notification_manager
                            .notify(
                                ctx.db.clone(),
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
                }
            })
            .await;
        if let Err(e) = backup_guard.unmount().await {
            tracing::error!("Error unmounting backup drive: {}", e);
            tracing::debug!("{:?}", e);
        }
    });

    Ok(())
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
    let mut os_backup: OsBackup = IoFormat::Cbor.from_slice(
        &tokio::fs::read(&os_backup_path)
            .await
            .with_ctx(|_| (ErrorKind::Filesystem, os_backup_path.display().to_string()))?,
    )?;

    os_backup.account.password = argon2::hash_encoded(
        embassy_password.as_bytes(),
        &rand::random::<[u8; 16]>()[..],
        &argon2::Config::rfc9106_low_mem(),
    )
    .with_kind(ErrorKind::PasswordHashGeneration)?;

    let secret_store = ctx.secret_store().await?;

    os_backup.account.save(&secret_store).await?;

    secret_store.close().await;

    init(&ctx.config).await?;

    let rpc_ctx = RpcContext::init(&ctx.config, disk_guid.clone()).await?;

    let ids: Vec<_> = backup_guard
        .metadata
        .package_backups
        .keys()
        .cloned()
        .collect();
    let (backup_guard, tasks, progress_info) =
        restore_packages(&rpc_ctx, backup_guard, ids).await?;
    let task_consumer_rpc_ctx = rpc_ctx.clone();
    stream::iter(
        tasks
            .into_iter()
            .map(|x| (x, task_consumer_rpc_ctx.clone())),
    )
    .for_each_concurrent(5, |(res, ctx)| async move {
        match res.await {
            (Ok(_), _) => (),
            (Err(err), package_id) => {
                if let Err(err) = ctx
                    .notification_manager
                    .notify(
                        ctx.db.clone(),
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
        }
    })
    .await;

    backup_guard.unmount().await?;
    rpc_ctx.shutdown().await?;

    Ok((
        disk_guid,
        os_backup.account.hostname,
        os_backup.account.key.tor_address(),
        os_backup.account.root_ca_cert,
    ))
}

#[instrument(skip(ctx, backup_guard))]
async fn restore_packages(
    ctx: &RpcContext,
    backup_guard: BackupMountGuard<TmpMountGuard>,
    ids: Vec<PackageId>,
) -> Result<(), Error> {
    let guards = assure_restoring(ctx, ids, &backup_guard).await?;

    let mut progress_info = ProgressInfo::default();

    let mut tasks = Vec::with_capacity(guards.len());
    for (manifest, guard) in guards {
        let id = manifest.id.clone();
        let ctx = ctx.clone();
        let service = ctx.services.get(&id).await.ok_or_else(|| {
            Error::new(
                eyre!("Service not found with {id}"),
                ErrorKind::InvalidRequest,
            )
        })?;
        let (install_progress, task) = Service::restore(service, guard).await?;
        progress_info
            .package_installs
            .insert(id.clone(), Arc::new(install_progress));
        progress_info
            .src_volume_size
            .insert(id.clone(), dir_size(backup_dir(&id), None).await?);
        progress_info.target_volume_size.insert(id.clone(), 0);
        let package_id = id.clone();
        tasks.push(
            async move {
                if let Err(e) = task.await {
                    tracing::error!("Error restoring package {}: {}", id, e);
                    tracing::debug!("{:?}", e);
                    Err(e)
                } else {
                    Ok(())
                }
            }
            .map(|x| (x, package_id))
            .boxed(),
        );
    }

    Ok((backup_guard, tasks, progress_info))
}
