use std::collections::BTreeMap;
use std::sync::Arc;

use clap::Parser;
use futures::{stream, StreamExt};
use models::PackageId;
use openssl::x509::X509;
use patch_db::json_ptr::ROOT;
use serde::{Deserialize, Serialize};
use torut::onion::OnionAddressV3;
use tracing::instrument;
use ts_rs::TS;

use super::target::BackupTargetId;
use crate::backup::os::OsBackup;
use crate::context::{RpcContext, SetupContext};
use crate::db::model::Database;
use crate::disk::mount::backup::BackupMountGuard;
use crate::disk::mount::filesystem::ReadWrite;
use crate::disk::mount::guard::{GenericMountGuard, TmpMountGuard};
use crate::hostname::Hostname;
use crate::init::init;
use crate::prelude::*;
use crate::s9pk::S9pk;
use crate::service::service_map::DownloadInstallFuture;
use crate::util::serde::IoFormat;

#[derive(Deserialize, Serialize, Parser, TS)]
#[serde(rename_all = "camelCase")]
#[command(rename_all = "kebab-case")]
pub struct RestorePackageParams {
    pub ids: Vec<PackageId>,
    pub target_id: BackupTargetId,
    pub password: String,
}

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
    let fs = target_id.load(&ctx.db.peek().await)?;
    let backup_guard =
        BackupMountGuard::mount(TmpMountGuard::mount(&fs, ReadWrite).await?, &password).await?;

    let tasks = restore_packages(&ctx, backup_guard, ids).await?;

    tokio::spawn(async move {
        stream::iter(tasks)
            .for_each_concurrent(5, |(id, res)| async move {
                match async { res.await?.await }.await {
                    Ok(_) => (),
                    Err(err) => {
                        tracing::error!("Error restoring package {}: {}", id, err);
                        tracing::debug!("{:?}", err);
                    }
                }
            })
            .await;
    });

    Ok(())
}

#[instrument(skip(ctx))]
pub async fn recover_full_embassy(
    ctx: SetupContext,
    disk_guid: Arc<String>,
    start_os_password: String,
    recovery_source: TmpMountGuard,
    recovery_password: Option<String>,
) -> Result<(Arc<String>, Hostname, OnionAddressV3, X509), Error> {
    let backup_guard = BackupMountGuard::mount(
        recovery_source,
        recovery_password.as_deref().unwrap_or_default(),
    )
    .await?;

    let os_backup_path = backup_guard.path().join("os-backup.cbor");
    let mut os_backup: OsBackup = IoFormat::Cbor.from_slice(
        &tokio::fs::read(&os_backup_path)
            .await
            .with_ctx(|_| (ErrorKind::Filesystem, os_backup_path.display().to_string()))?,
    )?;

    os_backup.account.password = argon2::hash_encoded(
        start_os_password.as_bytes(),
        &rand::random::<[u8; 16]>()[..],
        &argon2::Config::rfc9106_low_mem(),
    )
    .with_kind(ErrorKind::PasswordHashGeneration)?;

    let db = ctx.db().await?;
    db.put(
        &ROOT,
        &Database::init(&os_backup.account, ctx.config.wifi_interface.clone())?,
    )
    .await?;
    drop(db);

    init(&ctx.config).await?;

    let rpc_ctx = RpcContext::init(&ctx.config, disk_guid.clone()).await?;

    let ids: Vec<_> = backup_guard
        .metadata
        .package_backups
        .keys()
        .cloned()
        .collect();
    let tasks = restore_packages(&rpc_ctx, backup_guard, ids).await?;
    stream::iter(tasks)
        .for_each_concurrent(5, |(id, res)| async move {
            match async { res.await?.await }.await {
                Ok(_) => (),
                Err(err) => {
                    tracing::error!("Error restoring package {}: {}", id, err);
                    tracing::debug!("{:?}", err);
                }
            }
        })
        .await;

    rpc_ctx.shutdown().await?;

    Ok((
        disk_guid,
        os_backup.account.hostname,
        os_backup.account.tor_key.public().get_onion_address(),
        os_backup.account.root_ca_cert,
    ))
}

#[instrument(skip(ctx, backup_guard))]
async fn restore_packages(
    ctx: &RpcContext,
    backup_guard: BackupMountGuard<TmpMountGuard>,
    ids: Vec<PackageId>,
) -> Result<BTreeMap<PackageId, DownloadInstallFuture>, Error> {
    let backup_guard = Arc::new(backup_guard);
    let mut tasks = BTreeMap::new();
    for id in ids {
        let backup_dir = backup_guard.clone().package_backup(&id);
        let task = ctx
            .services
            .install(
                ctx.clone(),
                S9pk::open(
                    backup_dir.path().join(&id).with_extension("s9pk"),
                    Some(&id),
                )
                .await?,
                Some(backup_dir),
            )
            .await?;
        tasks.insert(id, task);
    }

    Ok(tasks)
}
