use std::collections::BTreeMap;
use std::path::{Path, PathBuf};
use std::sync::Arc;

use chrono::Utc;
use clap::Parser;
use color_eyre::eyre::eyre;
use imbl::OrdSet;
use serde::{Deserialize, Serialize};
use tokio::io::AsyncWriteExt;
use tracing::instrument;
use ts_rs::TS;

use super::PackageBackupReport;
use super::target::{BackupTargetId, PackageBackupInfo};
use crate::PackageId;
use crate::backup::os::OsBackup;
use crate::backup::{BackupReport, ServerBackupReport};
use crate::context::RpcContext;
use crate::db::model::public::BackupProgress;
use crate::db::model::{Database, DatabaseModel};
use crate::disk::mount::backup::BackupMountGuard;
use crate::disk::mount::filesystem::ReadWrite;
use crate::disk::mount::guard::{GenericMountGuard, TmpMountGuard};
use crate::middleware::auth::session::SessionAuthContext;
use crate::notifications::{NotificationLevel, notify};
use crate::prelude::*;
use crate::util::io::{AtomicFile, dir_copy};
use crate::util::serde::IoFormat;
use crate::version::VersionT;

#[derive(Deserialize, Serialize, Parser, TS)]
#[ts(export)]
#[serde(rename_all = "camelCase")]
#[command(rename_all = "kebab-case")]
pub struct BackupParams {
    #[arg(help = "help.arg.backup-target-id")]
    target_id: BackupTargetId,
    #[arg(long = "old-password", help = "help.arg.old-backup-password")]
    old_password: Option<crate::auth::PasswordType>,
    #[arg(long = "package-ids", help = "help.arg.package-ids-to-backup")]
    package_ids: Option<Vec<PackageId>>,
    #[arg(help = "help.arg.backup-password")]
    password: crate::auth::PasswordType,
}

struct BackupStatusGuard(Option<TypedPatchDb<Database>>);
impl BackupStatusGuard {
    fn new(db: TypedPatchDb<Database>) -> Self {
        Self(Some(db))
    }
    async fn handle_result(
        mut self,
        result: Result<BTreeMap<PackageId, PackageBackupReport>, Error>,
    ) -> Result<(), Error> {
        if let Some(db) = self.0.as_ref() {
            db.mutate(|v| {
                v.as_public_mut()
                    .as_server_info_mut()
                    .as_status_info_mut()
                    .as_backup_progress_mut()
                    .ser(&None)
            })
            .await
            .result?;
        }
        if let Some(db) = self.0.take() {
            match result {
                Ok(report) if report.iter().all(|(_, rep)| rep.error.is_none()) => {
                    db.mutate(|db| {
                        notify(
                            db,
                            None,
                            NotificationLevel::Success,
                            t!("backup.bulk.complete-title").to_string(),
                            t!("backup.bulk.complete-message").to_string(),
                            BackupReport {
                                server: ServerBackupReport {
                                    attempted: true,
                                    error: None,
                                },
                                packages: report,
                            },
                        )
                    })
                    .await
                }
                Ok(report) => {
                    db.mutate(|db| {
                        notify(
                            db,
                            None,
                            NotificationLevel::Warning,
                            t!("backup.bulk.complete-title").to_string(),
                            t!("backup.bulk.complete-with-failures").to_string(),
                            BackupReport {
                                server: ServerBackupReport {
                                    attempted: true,
                                    error: None,
                                },
                                packages: report,
                            },
                        )
                    })
                    .await
                }
                Err(e) => {
                    tracing::error!("{}", t!("backup.bulk.failed-error", error = e));
                    tracing::debug!("{:?}", e);
                    let err_string = e.to_string();
                    db.mutate(|db| {
                        notify(
                            db,
                            None,
                            NotificationLevel::Error,
                            t!("backup.bulk.failed-title").to_string(),
                            t!("backup.bulk.failed-message").to_string(),
                            BackupReport {
                                server: ServerBackupReport {
                                    attempted: true,
                                    error: Some(err_string),
                                },
                                packages: BTreeMap::new(),
                            },
                        )
                    })
                    .await
                }
            }
            .result?;
        }
        Ok(())
    }
}
impl Drop for BackupStatusGuard {
    fn drop(&mut self) {
        if let Some(db) = self.0.take() {
            tokio::spawn(async move {
                db.mutate(|v| {
                    v.as_public_mut()
                        .as_server_info_mut()
                        .as_status_info_mut()
                        .as_backup_progress_mut()
                        .ser(&None)
                })
                .await
                .result
                .log_err()
            });
        }
    }
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

    let ((fs, package_ids, server_id), status_guard) = (
        ctx.db
            .mutate(|db| {
                RpcContext::check_password(db, &password)?;
                let fs = target_id.load(db)?;
                let package_ids = if let Some(ids) = package_ids {
                    ids.into_iter().collect()
                } else {
                    db.as_public()
                        .as_package_data()
                        .as_entries()?
                        .into_iter()
                        .filter(|(_, m)| m.as_state_info().expect_installed().is_ok())
                        .map(|(id, _)| id)
                        .collect()
                };
                assure_backing_up(db, &package_ids)?;
                Ok((
                    fs,
                    package_ids,
                    db.as_public().as_server_info().as_id().de()?,
                ))
            })
            .await
            .result?,
        BackupStatusGuard::new(ctx.db.clone()),
    );

    let mut backup_guard = BackupMountGuard::mount(
        TmpMountGuard::mount(&fs, ReadWrite).await?,
        &server_id,
        &old_password_decrypted,
    )
    .await?;
    if old_password.is_some() {
        backup_guard.change_password(&password)?;
    }
    tokio::task::spawn(async move {
        status_guard
            .handle_result(perform_backup(&ctx, backup_guard, &package_ids).await)
            .await
            .unwrap();
    });
    Ok(())
}

#[instrument(skip(db, packages))]
fn assure_backing_up<'a>(
    db: &mut DatabaseModel,
    packages: impl IntoIterator<Item = &'a PackageId>,
) -> Result<(), Error> {
    let backing_up = db
        .as_public_mut()
        .as_server_info_mut()
        .as_status_info_mut()
        .as_backup_progress_mut();
    if backing_up.transpose_ref().is_some() {
        return Err(Error::new(
            eyre!("{}", t!("backup.bulk.already-backing-up")),
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
}

#[instrument(skip(ctx, backup_guard))]
async fn perform_backup(
    ctx: &RpcContext,
    backup_guard: BackupMountGuard<TmpMountGuard>,
    package_ids: &OrdSet<PackageId>,
) -> Result<BTreeMap<PackageId, PackageBackupReport>, Error> {
    let db = ctx.db.peek().await;
    let mut backup_report = BTreeMap::new();
    let backup_guard = Arc::new(backup_guard);
    let mut package_backups: BTreeMap<PackageId, PackageBackupInfo> =
        backup_guard.metadata.package_backups.clone();

    for id in package_ids {
        if let Some(service) = &*ctx.services.get(id).await {
            let backup_result = service
                .backup(backup_guard.package_backup(id).await?)
                .await
                .err()
                .map(|e| e.to_string());
            if backup_result.is_none() {
                let manifest = db
                    .as_public()
                    .as_package_data()
                    .as_idx(id)
                    .or_not_found(id)?
                    .as_state_info()
                    .expect_installed()?
                    .as_manifest();

                package_backups.insert(
                    id.clone(),
                    PackageBackupInfo {
                        os_version: manifest.as_metadata().as_os_version().de()?,
                        version: manifest.as_version().de()?,
                        title: manifest.as_metadata().as_title().de()?,
                        timestamp: Utc::now(),
                    },
                );

                ctx.db
                    .mutate(|db| {
                        if let Some(progress) = db
                            .as_public_mut()
                            .as_server_info_mut()
                            .as_status_info_mut()
                            .as_backup_progress_mut()
                            .transpose_mut()
                        {
                            progress.insert(&id, &BackupProgress { complete: true })?;
                        }
                        Ok(())
                    })
                    .await
                    .result?;
            }
            backup_report.insert(
                id.clone(),
                PackageBackupReport {
                    error: backup_result,
                },
            );
        }
    }

    let mut backup_guard = Arc::try_unwrap(backup_guard).map_err(|_| {
        Error::new(
            eyre!("{}", t!("backup.bulk.leaked-reference")),
            ErrorKind::Incoherent,
        )
    })?;

    let ui = ctx.db.peek().await.into_public().into_ui().de()?;

    let mut os_backup_file =
        AtomicFile::new(backup_guard.path().join("os-backup.json"), None::<PathBuf>).await?;
    os_backup_file
        .write_all(&IoFormat::Json.to_vec(&OsBackup {
            account: ctx.account.peek(|a| a.clone()),
            ui,
        })?)
        .await?;
    os_backup_file.save().await?;

    let luks_folder_old = backup_guard.path().join("luks.old");
    crate::util::io::delete_dir(&luks_folder_old).await?;
    let luks_folder_bak = backup_guard.path().join("luks");
    if tokio::fs::metadata(&luks_folder_bak).await.is_ok() {
        tokio::fs::rename(&luks_folder_bak, &luks_folder_old).await?;
    }
    let luks_folder = Path::new("/media/startos/config/luks");
    if tokio::fs::metadata(&luks_folder).await.is_ok() {
        dir_copy(luks_folder, &luks_folder_bak, None).await?;
    }

    let timestamp = Utc::now();

    backup_guard.unencrypted_metadata.version = crate::version::Current::default().semver().into();
    backup_guard.unencrypted_metadata.hostname = ctx.account.peek(|a| a.hostname.hostname.clone());
    backup_guard.unencrypted_metadata.timestamp = timestamp.clone();
    backup_guard.metadata.version = crate::version::Current::default().semver().into();
    backup_guard.metadata.timestamp = Some(timestamp);
    backup_guard.metadata.package_backups = package_backups;

    backup_guard.save_and_unmount().await?;

    ctx.db
        .mutate(|v| {
            v.as_public_mut()
                .as_server_info_mut()
                .as_last_backup_mut()
                .ser(&Some(timestamp))
        })
        .await
        .result?;

    Ok(backup_report)
}
