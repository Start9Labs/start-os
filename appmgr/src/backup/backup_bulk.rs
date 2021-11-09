use std::collections::BTreeMap;
use std::path::PathBuf;
use std::sync::Arc;

use chrono::Utc;
use color_eyre::eyre::eyre;
use openssl::pkey::{PKey, Private};
use openssl::x509::X509;
use patch_db::{DbHandle, LockType, PatchDbHandle, Revision};
use rpc_toolkit::command;
use serde::{Deserialize, Serialize};
use serde_json::Value;
use tokio::io::AsyncWriteExt;
use torut::onion::TorSecretKeyV3;
use tracing::instrument;

use super::PackageBackupReport;
use crate::auth::check_password_against_db;
use crate::backup::{BackupReport, ServerBackupReport};
use crate::context::RpcContext;
use crate::db::model::ServerStatus;
use crate::db::util::WithRevision;
use crate::disk::util::{BackupMountGuard, TmpMountGuard};
use crate::notifications::NotificationLevel;
use crate::s9pk::manifest::PackageId;
use crate::status::MainStatus;
use crate::util::{display_none, AtomicFile, IoFormat};
use crate::version::VersionT;
use crate::Error;

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
            .ok_or(serde::de::Error::invalid_value(
                serde::de::Unexpected::Str(&int.tor_key),
                &"an RFC4648 encoded string",
            ))?;
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

#[command(rename = "create", display(display_none))]
#[instrument(skip(ctx, old_password, password))]
pub async fn backup_all(
    #[context] ctx: RpcContext,
    #[arg] logicalname: PathBuf,
    #[arg(rename = "old-password", long = "old-password")] old_password: Option<String>,
    #[arg] password: String,
) -> Result<WithRevision<()>, Error> {
    let mut db = ctx.db.handle();
    check_password_against_db(&mut ctx.secret_store.acquire().await?, &password).await?;
    let mut backup_guard = BackupMountGuard::mount(
        TmpMountGuard::mount(&logicalname, None).await?,
        old_password.as_ref().unwrap_or(&password),
    )
    .await?;
    if old_password.is_some() {
        backup_guard.change_password(&password)?;
    }
    let revision = assure_backing_up(&mut db).await?;
    tokio::task::spawn(async move {
        match perform_backup(&ctx, &mut db, backup_guard).await {
            Ok(report) => ctx
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
        crate::db::DatabaseModel::new()
            .server_info()
            .status()
            .put(&mut db, &ServerStatus::Running)
            .await
            .expect("failed to change server status");
    });
    Ok(WithRevision {
        response: (),
        revision,
    })
}

#[instrument(skip(db))]
async fn assure_backing_up(db: &mut PatchDbHandle) -> Result<Option<Arc<Revision>>, Error> {
    let mut tx = db.begin().await?;
    let mut info = crate::db::DatabaseModel::new()
        .server_info()
        .get_mut(&mut tx)
        .await?;
    match &info.status {
        ServerStatus::Updating => {
            return Err(Error::new(
                eyre!("Server is updating!"),
                crate::ErrorKind::InvalidRequest,
            ))
        }
        ServerStatus::Updated => {
            return Err(Error::new(
                eyre!("Server is updated and needs to be reset"),
                crate::ErrorKind::InvalidRequest,
            ))
        }
        ServerStatus::BackingUp => {
            return Err(Error::new(
                eyre!("Server is already backing up!"),
                crate::ErrorKind::InvalidRequest,
            ))
        }
        ServerStatus::Running => (),
    }
    info.status = ServerStatus::BackingUp;
    info.save(&mut tx).await?;
    Ok(tx.commit(None).await?)
}

#[instrument(skip(ctx, db, backup_guard))]
async fn perform_backup<Db: DbHandle>(
    ctx: &RpcContext,
    mut db: Db,
    mut backup_guard: BackupMountGuard<TmpMountGuard>,
) -> Result<BTreeMap<PackageId, PackageBackupReport>, Error> {
    let mut backup_report = BTreeMap::new();

    for package_id in crate::db::DatabaseModel::new()
        .package_data()
        .keys(&mut db, true)
        .await?
    {
        let installed_model = if let Some(installed_model) = crate::db::DatabaseModel::new()
            .package_data()
            .idx_model(&package_id)
            .and_then(|m| m.installed())
            .check(&mut db)
            .await?
        {
            installed_model
        } else {
            continue;
        };
        let main_status_model = installed_model.clone().status().main();

        let mut tx = db.begin().await?; // for lock scope
        let (started, health) = match main_status_model.get(&mut tx, true).await?.into_owned() {
            MainStatus::Running { started, health } => (Some(started.clone()), health.clone()),
            MainStatus::Stopped | MainStatus::Stopping => (None, Default::default()),
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
                    started: started.clone(),
                    health: health.clone(),
                },
            )
            .await?;
        tx.save().await?; // drop locks

        let manifest = installed_model
            .clone()
            .manifest()
            .get(&mut db, true)
            .await?;

        ctx.managers
            .get(&(manifest.id.clone(), manifest.version.clone()))
            .await
            .ok_or_else(|| {
                Error::new(eyre!("Manager not found"), crate::ErrorKind::InvalidRequest)
            })?
            .synchronize()
            .await;

        installed_model.lock(&mut db, LockType::Write).await;

        let guard = backup_guard.mount_package_backup(&package_id).await?;
        let res = manifest
            .backup
            .create(
                &ctx,
                &package_id,
                &manifest.title,
                &manifest.version,
                &manifest.interfaces,
                &manifest.volumes,
            )
            .await;
        drop(guard);
        backup_report.insert(
            package_id.clone(),
            PackageBackupReport {
                error: res.as_ref().err().map(|e| e.to_string()),
            },
        );

        let mut tx = db.begin().await?;
        if let Ok(pkg_meta) = res {
            installed_model
                .last_backup()
                .put(&mut tx, &Some(pkg_meta.timestamp))
                .await?;
            backup_guard
                .metadata
                .package_backups
                .insert(package_id, pkg_meta);
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
        tx.save().await?;
    }

    let (root_ca_key, root_ca_cert) = ctx.net_controller.ssl.export_root_ca().await?;
    let mut os_backup_file = AtomicFile::new(backup_guard.as_ref().join("os-backup.cbor")).await?;
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
    os_backup_file.save().await?;

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
