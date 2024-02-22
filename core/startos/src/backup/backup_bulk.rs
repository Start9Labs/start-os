use std::collections::BTreeMap;
use std::panic::UnwindSafe;
use std::path::{Path, PathBuf};
use std::sync::Arc;

use chrono::Utc;
use clap::Parser;
use color_eyre::eyre::eyre;
use helpers::AtomicFile;
use imbl::OrdSet;
use models::PackageId;
use serde::{Deserialize, Serialize};
use tokio::io::AsyncWriteExt;
use tracing::instrument;

use super::target::BackupTargetId;
use super::PackageBackupReport;
use crate::auth::check_password_against_db;
use crate::backup::os::OsBackup;
use crate::backup::{BackupReport, ServerBackupReport};
use crate::context::RpcContext;
use crate::db::model::BackupProgress;
use crate::disk::mount::backup::BackupMountGuard;
use crate::disk::mount::filesystem::ReadWrite;
use crate::disk::mount::guard::{GenericMountGuard, TmpMountGuard};
use crate::notifications::NotificationLevel;
use crate::prelude::*;
use crate::util::io::dir_copy;
use crate::util::serde::IoFormat;
use crate::version::VersionT;

#[derive(Deserialize, Serialize, Parser)]
#[serde(rename_all = "kebab-case")]
#[command(rename_all = "kebab-case")]
pub struct BackupParams {
    target_id: BackupTargetId,
    #[arg(long = "old-password")]
    old_password: Option<crate::auth::PasswordType>,
    #[arg(long = "package-ids")]
    package_ids: Option<Vec<PackageId>>,
    password: crate::auth::PasswordType,
}

#[instrument(skip(ctx, old_password, password))]
pub async fn backup_all(
    ctx: RpcContext,
    BackupParams {
        target_id,
        old_password,
        package_ids,
        password,
    }: BackupParams,
) -> Result<(), Error> {
    let old_password_decrypted = old_password
        .as_ref()
        .unwrap_or(&password)
        .clone()
        .decrypt(&ctx)?;
    let password = password.decrypt(&ctx)?;
    check_password_against_db(ctx.secret_store.acquire().await?.as_mut(), &password).await?;
    let fs = target_id
        .load(ctx.secret_store.acquire().await?.as_mut())
        .await?;
    let mut backup_guard = BackupMountGuard::mount(
        TmpMountGuard::mount(&fs, ReadWrite).await?,
        &old_password_decrypted,
    )
    .await?;
    let package_ids = if let Some(ids) = package_ids {
        ids.into_iter().collect()
    } else {
        todo!("all installed packages");
    };
    if old_password.is_some() {
        backup_guard.change_password(&password)?;
    }
    assure_backing_up(&ctx.db, &package_ids).await?;
    tokio::task::spawn(async move {
        let backup_res = perform_backup(&ctx, backup_guard, &package_ids).await;
        match backup_res {
            Ok(report) if report.iter().all(|(_, rep)| rep.error.is_none()) => ctx
                .notification_manager
                .notify(
                    ctx.db.clone(),
                    None,
                    NotificationLevel::Success,
                    "Backup Complete".to_owned(),
                    "Your backup has completed".to_owned(),
                    BackupReport {
                        server: ServerBackupReport {
                            attempted: true,
                            error: None,
                        },
                        packages: report,
                    },
                    None,
                )
                .await
                .expect("failed to send notification"),
            Ok(report) => ctx
                .notification_manager
                .notify(
                    ctx.db.clone(),
                    None,
                    NotificationLevel::Warning,
                    "Backup Complete".to_owned(),
                    "Your backup has completed, but some package(s) failed to backup".to_owned(),
                    BackupReport {
                        server: ServerBackupReport {
                            attempted: true,
                            error: None,
                        },
                        packages: report,
                    },
                    None,
                )
                .await
                .expect("failed to send notification"),
            Err(e) => {
                tracing::error!("Backup Failed: {}", e);
                tracing::debug!("{:?}", e);
                ctx.notification_manager
                    .notify(
                        ctx.db.clone(),
                        None,
                        NotificationLevel::Error,
                        "Backup Failed".to_owned(),
                        "Your backup failed to complete.".to_owned(),
                        BackupReport {
                            server: ServerBackupReport {
                                attempted: true,
                                error: Some(e.to_string()),
                            },
                            packages: BTreeMap::new(),
                        },
                        None,
                    )
                    .await
                    .expect("failed to send notification");
            }
        }
        ctx.db
            .mutate(|v| {
                v.as_public_mut()
                    .as_server_info_mut()
                    .as_status_info_mut()
                    .as_backup_progress_mut()
                    .ser(&None)
            })
            .await?;
        Ok::<(), Error>(())
    });
    Ok(())
}

#[instrument(skip(db, packages))]
async fn assure_backing_up(
    db: &PatchDb,
    packages: impl IntoIterator<Item = &PackageId> + UnwindSafe + Send,
) -> Result<(), Error> {
    db.mutate(|v| {
        let backing_up = v
            .as_public_mut()
            .as_server_info_mut()
            .as_status_info_mut()
            .as_backup_progress_mut();
        if backing_up
            .clone()
            .de()?
            .iter()
            .flat_map(|x| x.values())
            .fold(false, |acc, x| {
                if !x.complete {
                    return true;
                }
                acc
            })
        {
            return Err(Error::new(
                eyre!("Server is already backing up!"),
                ErrorKind::InvalidRequest,
            ));
        }
        backing_up.ser(&Some(
            packages
                .into_iter()
                .map(|x| (x.clone(), BackupProgress { complete: false }))
                .collect(),
        ))?;
        Ok(())
    })
    .await
}

#[instrument(skip(ctx, backup_guard))]
async fn perform_backup(
    ctx: &RpcContext,
    backup_guard: BackupMountGuard<TmpMountGuard>,
    package_ids: &OrdSet<PackageId>,
) -> Result<BTreeMap<PackageId, PackageBackupReport>, Error> {
    let mut backup_report = BTreeMap::new();
    let backup_guard = Arc::new(backup_guard);

    for id in package_ids {
        if let Some(service) = &*ctx.services.get(id).await {
            backup_report.insert(
                id.clone(),
                PackageBackupReport {
                    error: service
                        .backup(backup_guard.package_backup(id))
                        .await
                        .err()
                        .map(|e| e.to_string()),
                },
            );
        }
    }

    let mut backup_guard = Arc::try_unwrap(backup_guard).map_err(|_| {
        Error::new(
            eyre!("leaked reference to BackupMountGuard"),
            ErrorKind::Incoherent,
        )
    })?;

    let ui = ctx.db.peek().await.into_public().into_ui().de()?;

    let mut os_backup_file =
        AtomicFile::new(backup_guard.path().join("os-backup.cbor"), None::<PathBuf>)
            .await
            .with_kind(ErrorKind::Filesystem)?;
    os_backup_file
        .write_all(&IoFormat::Cbor.to_vec(&OsBackup {
            account: ctx.account.read().await.clone(),
            ui,
        })?)
        .await?;
    os_backup_file
        .save()
        .await
        .with_kind(ErrorKind::Filesystem)?;

    let luks_folder_old = backup_guard.path().join("luks.old");
    if tokio::fs::metadata(&luks_folder_old).await.is_ok() {
        tokio::fs::remove_dir_all(&luks_folder_old).await?;
    }
    let luks_folder_bak = backup_guard.path().join("luks");
    if tokio::fs::metadata(&luks_folder_bak).await.is_ok() {
        tokio::fs::rename(&luks_folder_bak, &luks_folder_old).await?;
    }
    let luks_folder = Path::new("/media/embassy/config/luks");
    if tokio::fs::metadata(&luks_folder).await.is_ok() {
        dir_copy(&luks_folder, &luks_folder_bak, None).await?;
    }

    let timestamp = Some(Utc::now());

    backup_guard.unencrypted_metadata.version = crate::version::Current::new().semver().into();
    backup_guard.unencrypted_metadata.full = true;
    backup_guard.metadata.version = crate::version::Current::new().semver().into();
    backup_guard.metadata.timestamp = timestamp;

    backup_guard.save_and_unmount().await?;

    ctx.db
        .mutate(|v| {
            v.as_public_mut()
                .as_server_info_mut()
                .as_last_backup_mut()
                .ser(&timestamp)
        })
        .await?;

    Ok(backup_report)
}
