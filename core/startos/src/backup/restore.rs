use std::collections::BTreeMap;
use std::sync::Arc;

use clap::{ArgMatches, Parser};
use futures::{stream, FutureExt, StreamExt};
use models::PackageId;
use openssl::x509::X509;
use serde::{Deserialize, Serialize};
use sqlx::Connection;
use torut::onion::OnionAddressV3;
use tracing::instrument;

use super::target::BackupTargetId;
use crate::backup::os::OsBackup;
use crate::context::{RpcContext, SetupContext};
use crate::disk::mount::backup::BackupMountGuard;
use crate::disk::mount::filesystem::ReadWrite;
use crate::disk::mount::guard::{GenericMountGuard, SubPath, TmpMountGuard};
use crate::hostname::Hostname;
use crate::init::init;
use crate::prelude::*;
use crate::s9pk::S9pk;
use crate::service::service_map::DownloadInstallFuture;
use crate::util::serde::IoFormat;

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
    embassy_password: String,
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
        os_backup.account.key.tor_address(),
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
                Some(SubPath::new(backup_dir, "data")),
            )
            .await?;
        tasks.insert(id, task);
    }

    Ok(tasks)
}
