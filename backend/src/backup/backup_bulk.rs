use std::collections::{BTreeMap, BTreeSet};
use std::panic::UnwindSafe;
use std::path::PathBuf;
use std::sync::Arc;

use chrono::Utc;
use clap::ArgMatches;
use color_eyre::eyre::eyre;
use helpers::AtomicFile;
use rpc_toolkit::command;
use tokio::io::AsyncWriteExt;
use tokio::sync::Mutex;
use tracing::instrument;

use super::target::BackupTargetId;
use super::PackageBackupReport;
use crate::auth::check_password_against_db;
use crate::backup::os::OsBackup;
use crate::backup::{BackupReport, ServerBackupReport};
use crate::context::RpcContext;
use crate::db::model::BackupProgress;
use crate::db::package::get_packages;
use crate::disk::mount::backup::BackupMountGuard;
use crate::disk::mount::filesystem::ReadWrite;
use crate::disk::mount::guard::TmpMountGuard;
use crate::manager::BackupReturn;
use crate::notifications::NotificationLevel;
use crate::prelude::*;
use crate::s9pk::manifest::PackageId;
use crate::util::display_none;
use crate::util::serde::IoFormat;
use crate::version::VersionT;

fn parse_comma_separated(arg: &str, _: &ArgMatches) -> Result<BTreeSet<PackageId>, Error> {
    arg.split(',')
        .map(|s| s.trim().parse().map_err(Error::from))
        .collect()
}

#[command(rename = "create", display(display_none))]
#[instrument(skip(ctx, old_password, password))]
pub async fn backup_all(
    #[context] ctx: RpcContext,
    #[arg(rename = "target-id")] target_id: BackupTargetId,
    #[arg(rename = "old-password", long = "old-password")] old_password: Option<
        crate::auth::PasswordType,
    >,
    #[arg(
        rename = "package-ids",
        long = "package-ids",
        parse(parse_comma_separated)
    )]
    package_ids: Option<BTreeSet<PackageId>>,
    #[arg] password: crate::auth::PasswordType,
) -> Result<(), Error> {
    let old_password_decrypted = old_password
        .as_ref()
        .unwrap_or(&password)
        .clone()
        .decrypt(&ctx)?;
    let password = password.decrypt(&ctx)?;
    check_password_against_db(&mut ctx.secret_store.acquire().await?, &password).await?;
    let fs = target_id
        .load(&mut ctx.secret_store.acquire().await?)
        .await?;
    let mut backup_guard = BackupMountGuard::mount(
        TmpMountGuard::mount(&fs, ReadWrite).await?,
        &old_password_decrypted,
    )
    .await?;
    let all_packages = get_packages(&ctx.db).await?;
    let package_ids = package_ids.unwrap_or(all_packages);
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
                    &ctx.db,
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
                    &ctx.db,
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
                        &ctx.db,
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
            .apply_fn(|v| v.server_info().status_info().backup_progress().set(&None))
            .await?;
        backup_res
    });
    Ok(())
}

#[instrument(skip(db, packages))]
async fn assure_backing_up(
    db: &PatchDb,
    packages: impl IntoIterator<Item = &PackageId> + UnwindSafe + Send,
) -> Result<(), Error> {
    db.apply_fn(|mut v| {
        let mut backing_up = v.server_info().status_info().backup_progress();
        if backing_up
            .get()?
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
        backing_up.set(&Some(
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
    package_ids: &BTreeSet<PackageId>,
) -> Result<BTreeMap<PackageId, PackageBackupReport>, Error> {
    let mut backup_report = BTreeMap::new();
    let backup_guard = Arc::new(Mutex::new(backup_guard));

    for package_id in package_ids {
        let (response, report) = match ctx
            .managers
            .get(package_id)
            .await
            .ok_or_else(|| Error::new(eyre!("Manager not found"), ErrorKind::InvalidRequest))?
            .backup(backup_guard.clone())
            .await
        {
            BackupReturn::Ran { report, res } => (res, report),
            BackupReturn::AlreadyRunning(report) => {
                backup_report.insert(package_id.clone(), report);
                continue;
            }
            BackupReturn::Error(error) => {
                tracing::warn!("Backup thread error");
                tracing::debug!("{error:?}");
                backup_report.insert(
                    package_id.clone(),
                    PackageBackupReport {
                        error: Some("Backup thread error".to_owned()),
                    },
                );
                continue;
            }
        };
        backup_report.insert(
            package_id.clone(),
            PackageBackupReport {
                error: response.as_ref().err().map(|e| e.to_string()),
            },
        );

        if let Ok(pkg_meta) = response {
            backup_guard
                .lock()
                .await
                .metadata
                .package_backups
                .insert(package_id.clone(), pkg_meta);
        }

        todo!("Manager backup fn should handle updating progress, since it needs to happen atomically with updating the status");
    }

    let ui = ctx.db.apply_fn(|mut v| Ok(v.ui().clone())).await?;

    let mut os_backup_file = AtomicFile::new(
        backup_guard.lock().await.as_ref().join("os-backup.cbor"),
        None::<PathBuf>,
    )
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

    let timestamp = Some(Utc::now());
    let mut backup_guard = Arc::try_unwrap(backup_guard)
        .map_err(|_err| {
            Error::new(
                eyre!("Backup guard could not ensure that the others where dropped"),
                ErrorKind::Unknown,
            )
        })?
        .into_inner();

    backup_guard.unencrypted_metadata.version = crate::version::Current::new().semver().into();
    backup_guard.unencrypted_metadata.full = true;
    backup_guard.metadata.version = crate::version::Current::new().semver().into();
    backup_guard.metadata.timestamp = timestamp;

    backup_guard.save_and_unmount().await?;

    ctx.db
        .apply_fn(|mut v| v.server_info().last_backup().set(&timestamp))
        .await?;
    Ok(backup_report)
}
