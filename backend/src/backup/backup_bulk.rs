use std::collections::{BTreeMap, BTreeSet};
use std::path::PathBuf;

use chrono::Utc;
use clap::ArgMatches;
use color_eyre::eyre::eyre;
use helpers::AtomicFile;
use openssl::pkey::{PKey, Private};
use openssl::x509::X509;
use patch_db::{DbHandle, LockType, PatchDbHandle};
use rpc_toolkit::command;
use serde::{Deserialize, Serialize};
use serde_json::Value;
use tokio::io::AsyncWriteExt;
use torut::onion::TorSecretKeyV3;
use tracing::instrument;

use super::target::BackupTargetId;
use super::PackageBackupReport;
use crate::auth::check_password_against_db;
use crate::backup::{BackupReport, ServerBackupReport};
use crate::context::RpcContext;
use crate::db::model::BackupProgress;
use crate::disk::mount::backup::BackupMountGuard;
use crate::disk::mount::filesystem::ReadWrite;
use crate::disk::mount::guard::TmpMountGuard;
use crate::notifications::NotificationLevel;
use crate::s9pk::manifest::PackageId;
use crate::status::MainStatus;
use crate::util::display_none;
use crate::util::serde::IoFormat;
use crate::version::VersionT;
use crate::{Error, ErrorKind, ResultExt};

#[derive(Debug)]
pub struct OsBackup {
    pub tor_key: TorSecretKeyV3,
    pub root_ca_key: PKey<Private>,
    pub root_ca_cert: X509,
    pub ui: Value,
}
impl<'de> Deserialize<'de> for OsBackup {
    fn deserialize<D>(deserializer: D) -> Result<Self, D::Error>
    where
        D: serde::Deserializer<'de>,
    {
        #[derive(Deserialize)]
        #[serde(rename = "kebab-case")]
        struct OsBackupDe {
            tor_key: String,
            root_ca_key: String,
            root_ca_cert: String,
            ui: Value,
        }
        let int = OsBackupDe::deserialize(deserializer)?;
        let key_vec = base32::decode(base32::Alphabet::RFC4648 { padding: true }, &int.tor_key)
            .ok_or_else(|| {
                serde::de::Error::invalid_value(
                    serde::de::Unexpected::Str(&int.tor_key),
                    &"an RFC4648 encoded string",
                )
            })?;
        if key_vec.len() != 64 {
            return Err(serde::de::Error::invalid_value(
                serde::de::Unexpected::Str(&int.tor_key),
                &"a 64 byte value encoded as an RFC4648 string",
            ));
        }
        let mut key_slice = [0; 64];
        key_slice.clone_from_slice(&key_vec);
        Ok(OsBackup {
            tor_key: TorSecretKeyV3::from(key_slice),
            root_ca_key: PKey::<Private>::private_key_from_pem(int.root_ca_key.as_bytes())
                .map_err(serde::de::Error::custom)?,
            root_ca_cert: X509::from_pem(int.root_ca_cert.as_bytes())
                .map_err(serde::de::Error::custom)?,
            ui: int.ui,
        })
    }
}
impl Serialize for OsBackup {
    fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
    where
        S: serde::Serializer,
    {
        #[derive(Serialize)]
        #[serde(rename = "kebab-case")]
        struct OsBackupSer<'a> {
            tor_key: String,
            root_ca_key: String,
            root_ca_cert: String,
            ui: &'a Value,
        }
        OsBackupSer {
            tor_key: base32::encode(
                base32::Alphabet::RFC4648 { padding: true },
                &self.tor_key.as_bytes(),
            ),
            root_ca_key: String::from_utf8(
                self.root_ca_key
                    .private_key_to_pem_pkcs8()
                    .map_err(serde::ser::Error::custom)?,
            )
            .map_err(serde::ser::Error::custom)?,
            root_ca_cert: String::from_utf8(
                self.root_ca_cert
                    .to_pem()
                    .map_err(serde::ser::Error::custom)?,
            )
            .map_err(serde::ser::Error::custom)?,
            ui: &self.ui,
        }
        .serialize(serializer)
    }
}

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
    let mut db = ctx.db.handle();
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
    let all_packages = crate::db::DatabaseModel::new()
        .package_data()
        .get(&mut db, false)
        .await?
        .0
        .keys()
        .into_iter()
        .cloned()
        .collect();
    let package_ids = package_ids.unwrap_or(all_packages);
    if old_password.is_some() {
        backup_guard.change_password(&password)?;
    }
    assure_backing_up(&mut db, &package_ids).await?;
    tokio::task::spawn(async move {
        let backup_res = perform_backup(&ctx, &mut db, backup_guard, &package_ids).await;
        let backup_progress = crate::db::DatabaseModel::new()
            .server_info()
            .status_info()
            .backup_progress();
        backup_progress
            .clone()
            .lock(&mut db, LockType::Write)
            .await
            .expect("failed to lock server status");
        match backup_res {
            Ok(report) if report.iter().all(|(_, rep)| rep.error.is_none()) => ctx
                .notification_manager
                .notify(
                    &mut db,
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
                    &mut db,
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
                        &mut db,
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
        backup_progress
            .delete(&mut db)
            .await
            .expect("failed to change server status");
    });
    Ok(())
}

#[instrument(skip(db, packages))]
async fn assure_backing_up(
    db: &mut PatchDbHandle,
    packages: impl IntoIterator<Item = &PackageId>,
) -> Result<(), Error> {
    let mut tx = db.begin().await?;
    let mut backing_up = crate::db::DatabaseModel::new()
        .server_info()
        .status_info()
        .backup_progress()
        .get_mut(&mut tx)
        .await?;

    if backing_up
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
            crate::ErrorKind::InvalidRequest,
        ));
    }
    *backing_up = Some(
        packages
            .into_iter()
            .map(|x| (x.clone(), BackupProgress { complete: false }))
            .collect(),
    );
    backing_up.save(&mut tx).await?;
    tx.commit().await?;
    Ok(())
}

#[instrument(skip(ctx, db, backup_guard))]
async fn perform_backup<Db: DbHandle>(
    ctx: &RpcContext,
    mut db: Db,
    mut backup_guard: BackupMountGuard<TmpMountGuard>,
    package_ids: &BTreeSet<PackageId>,
) -> Result<BTreeMap<PackageId, PackageBackupReport>, Error> {
    let mut backup_report = BTreeMap::new();

    for package_id in crate::db::DatabaseModel::new()
        .package_data()
        .keys(&mut db, false)
        .await?
        .into_iter()
        .filter(|id| package_ids.contains(id))
    {
        let mut tx = db.begin().await?; // for lock scope
        let installed_model = if let Some(installed_model) = crate::db::DatabaseModel::new()
            .package_data()
            .idx_model(&package_id)
            .and_then(|m| m.installed())
            .check(&mut tx)
            .await?
        {
            installed_model
        } else {
            continue;
        };
        let main_status_model = installed_model.clone().status().main();

        main_status_model.lock(&mut tx, LockType::Write).await?;
        let (started, health) = match main_status_model.get(&mut tx, true).await?.into_owned() {
            MainStatus::Starting { .. } => (Some(Utc::now()), Default::default()),
            MainStatus::Running { started, health } => (Some(started), health.clone()),
            MainStatus::Stopped | MainStatus::Stopping | MainStatus::Restarting => {
                (None, Default::default())
            }
            MainStatus::BackingUp { .. } => {
                backup_report.insert(
                    package_id,
                    PackageBackupReport {
                        error: Some(
                            "Can't do backup because service is in a backing up state".to_owned(),
                        ),
                    },
                );
                continue;
            }
        };
        main_status_model
            .put(
                &mut tx,
                &MainStatus::BackingUp {
                    started,
                    health: health.clone(),
                },
            )
            .await?;
        tx.save().await?; // drop locks

        let manifest = installed_model
            .clone()
            .manifest()
            .get(&mut db, false)
            .await?;

        ctx.managers
            .get(&(manifest.id.clone(), manifest.version.clone()))
            .await
            .ok_or_else(|| {
                Error::new(eyre!("Manager not found"), crate::ErrorKind::InvalidRequest)
            })?
            .synchronize()
            .await;

        let mut tx = db.begin().await?;

        installed_model.lock(&mut tx, LockType::Write).await?;

        let guard = backup_guard.mount_package_backup(&package_id).await?;
        let res = manifest
            .backup
            .create(
                ctx,
                &mut tx,
                &package_id,
                &manifest.title,
                &manifest.version,
                &manifest.interfaces,
                &manifest.volumes,
            )
            .await;
        guard.unmount().await?;
        backup_report.insert(
            package_id.clone(),
            PackageBackupReport {
                error: res.as_ref().err().map(|e| e.to_string()),
            },
        );

        if let Ok(pkg_meta) = res {
            installed_model
                .last_backup()
                .put(&mut tx, &Some(pkg_meta.timestamp))
                .await?;
            backup_guard
                .metadata
                .package_backups
                .insert(package_id.clone(), pkg_meta);
        }

        main_status_model
            .put(
                &mut tx,
                &match started {
                    Some(started) => MainStatus::Running { started, health },
                    None => MainStatus::Stopped,
                },
            )
            .await?;

        let mut backup_progress = crate::db::DatabaseModel::new()
            .server_info()
            .status_info()
            .backup_progress()
            .get_mut(&mut tx)
            .await?;
        if backup_progress.is_none() {
            *backup_progress = Some(Default::default());
        }
        if let Some(mut backup_progress) = backup_progress
            .as_mut()
            .and_then(|bp| bp.get_mut(&package_id))
        {
            (*backup_progress).complete = true;
        }
        backup_progress.save(&mut tx).await?;
        tx.save().await?;
    }

    crate::db::DatabaseModel::new()
        .lock(&mut db, LockType::Write)
        .await?;

    let (root_ca_key, root_ca_cert) = ctx.net_controller.ssl.export_root_ca().await?;
    let mut os_backup_file = AtomicFile::new(
        backup_guard.as_ref().join("os-backup.cbor"),
        None::<PathBuf>,
    )
    .await
    .with_kind(ErrorKind::Filesystem)?;
    os_backup_file
        .write_all(
            &IoFormat::Cbor.to_vec(&OsBackup {
                tor_key: ctx.net_controller.tor.embassyd_tor_key().await,
                root_ca_key,
                root_ca_cert,
                ui: crate::db::DatabaseModel::new()
                    .ui()
                    .get(&mut db, true)
                    .await?
                    .into_owned(),
            })?,
        )
        .await?;
    os_backup_file
        .save()
        .await
        .with_kind(ErrorKind::Filesystem)?;

    let timestamp = Some(Utc::now());

    backup_guard.unencrypted_metadata.version = crate::version::Current::new().semver().into();
    backup_guard.unencrypted_metadata.full = true;
    backup_guard.metadata.version = crate::version::Current::new().semver().into();
    backup_guard.metadata.timestamp = timestamp;

    backup_guard.save_and_unmount().await?;

    crate::db::DatabaseModel::new()
        .server_info()
        .last_backup()
        .put(&mut db, &timestamp)
        .await?;
    Ok(backup_report)
}
