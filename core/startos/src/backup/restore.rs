use std::collections::BTreeMap;
use std::sync::Arc;

use clap::Parser;
use futures::{stream, StreamExt};
use models::PackageId;
use patch_db::json_ptr::ROOT;
use serde::{Deserialize, Serialize};
use tokio::sync::Mutex;
use tracing::instrument;
use ts_rs::TS;

use super::target::BackupTargetId;
use crate::backup::os::OsBackup;
use crate::context::setup::SetupResult;
use crate::context::{RpcContext, SetupContext};
use crate::db::model::Database;
use crate::disk::mount::backup::BackupMountGuard;
use crate::disk::mount::filesystem::ReadWrite;
use crate::disk::mount::guard::{GenericMountGuard, TmpMountGuard};
use crate::init::init;
use crate::prelude::*;
use crate::s9pk::S9pk;
use crate::service::service_map::DownloadInstallFuture;
use crate::setup::SetupExecuteProgress;
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
    let peek = ctx.db.peek().await;
    let fs = target_id.load(&peek)?;
    let backup_guard = BackupMountGuard::mount(
        TmpMountGuard::mount(&fs, ReadWrite).await?,
        &peek.as_public().as_server_info().as_id().de()?,
        &password,
    )
    .await?;

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

#[instrument(skip_all)]
pub async fn recover_full_embassy(
    ctx: &SetupContext,
    disk_guid: Arc<String>,
    start_os_password: String,
    recovery_source: TmpMountGuard,
    server_id: &str,
    recovery_password: &str,
    SetupExecuteProgress {
        init_phases,
        restore_phase,
        rpc_ctx_phases,
    }: SetupExecuteProgress,
) -> Result<(SetupResult, RpcContext), Error> {
    let mut restore_phase = restore_phase.or_not_found("restore progress")?;

    let backup_guard =
        BackupMountGuard::mount(recovery_source, server_id, recovery_password).await?;

    let os_backup_path = backup_guard.path().join("os-backup.json");
    let mut os_backup: OsBackup = IoFormat::Json.from_slice(
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
    db.put(&ROOT, &Database::init(&os_backup.account)?).await?;
    drop(db);

    let init_result = init(&ctx.webserver, &ctx.config, init_phases).await?;

    let rpc_ctx = RpcContext::init(
        &ctx.webserver,
        &ctx.config,
        disk_guid.clone(),
        Some(init_result),
        rpc_ctx_phases,
    )
    .await?;

    restore_phase.start();
    let ids: Vec<_> = backup_guard
        .metadata
        .package_backups
        .keys()
        .cloned()
        .collect();
    let tasks = restore_packages(&rpc_ctx, backup_guard, ids).await?;
    restore_phase.set_total(tasks.len() as u64);
    let restore_phase = Arc::new(Mutex::new(restore_phase));
    stream::iter(tasks)
        .for_each_concurrent(5, |(id, res)| {
            let restore_phase = restore_phase.clone();
            async move {
                match async { res.await?.await }.await {
                    Ok(_) => (),
                    Err(err) => {
                        tracing::error!("Error restoring package {}: {}", id, err);
                        tracing::debug!("{:?}", err);
                    }
                }
                *restore_phase.lock().await += 1;
            }
        })
        .await;
    restore_phase.lock().await.complete();

    Ok(((&os_backup.account).try_into()?, rpc_ctx))
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
        let backup_dir = backup_guard.clone().package_backup(&id).await?;
        let s9pk_path = backup_dir.path().join(&id).with_extension("s9pk");
        let task = ctx
            .services
            .install(
                ctx.clone(),
                || S9pk::open(s9pk_path, Some(&id)),
                None, // TODO: pull from metadata?
                Some(backup_dir),
                None,
            )
            .await?;
        tasks.insert(id, task);
    }

    Ok(tasks)
}
