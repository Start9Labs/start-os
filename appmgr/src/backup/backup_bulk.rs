use std::collections::BTreeMap;
use std::path::{Path, PathBuf};
use std::sync::Arc;

use color_eyre::eyre::eyre;
use openssl::pkey::{PKey, Private};
use openssl::x509::X509;
use patch_db::{DbHandle, LockType, PatchDbHandle, Revision};
use rpc_toolkit::command;
use serde::{Deserialize, Serialize};
use serde_json::Value;
use tokio::process::Command;
use torut::onion::TorSecretKeyV3;
use tracing::instrument;

use super::PackageBackupReport;
use crate::auth::check_password;
use crate::backup::{BackupReport, ServerBackupReport};
use crate::context::RpcContext;
use crate::db::model::ServerStatus;
use crate::db::util::WithRevision;
use crate::disk::util::{mount, mount_ecryptfs, unmount};
use crate::disk::BackupInfo;
use crate::install::PKG_ARCHIVE_DIR;
use crate::notifications::NotificationLevel;
use crate::s9pk::manifest::PackageId;
use crate::status::MainStatus;
use crate::util::{display_none, GeneralGuard, Invoke, IoFormat};
use crate::version::VersionT;
use crate::volume::{BACKUP_DIR, BACKUP_DIR_CRYPT, BACKUP_MNT};
use crate::{Error, ErrorKind, ResultExt};

#[derive(Debug)]
pub struct OsBackup {
    pub tor_key: TorSecretKeyV3,
    pub root_ca_key: PKey<Private>,
    pub root_ca_cert: X509,
    pub int_ca_key: PKey<Private>,
    pub int_ca_cert: X509,
    pub ui: Value,
}
impl<'de> Deserialize<'de> for OsBackup {
    fn deserialize<D>(deserializer: D) -> Result<Self, D::Error>
    where
        D: serde::Deserializer<'de>,
    {
        #[derive(Deserialize)]
        struct OsBackupDe {
            tor_key: TorSecretKeyV3,
            root_ca_key: String,
            root_ca_cert: String,
            int_ca_key: String,
            int_ca_cert: String,
            ui: Value,
        }
        let int = OsBackupDe::deserialize(deserializer)?;
        Ok(OsBackup {
            tor_key: int.tor_key,
            root_ca_key: PKey::<Private>::private_key_from_pem(int.root_ca_key.as_bytes())
                .map_err(serde::de::Error::custom)?,
            root_ca_cert: X509::from_pem(int.root_ca_cert.as_bytes())
                .map_err(serde::de::Error::custom)?,
            int_ca_key: PKey::<Private>::private_key_from_pem(int.int_ca_key.as_bytes())
                .map_err(serde::de::Error::custom)?,
            int_ca_cert: X509::from_pem(int.int_ca_cert.as_bytes())
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
        struct OsBackupSer<'a> {
            tor_key: &'a TorSecretKeyV3,
            root_ca_key: String,
            root_ca_cert: String,
            int_ca_key: String,
            int_ca_cert: String,
            ui: &'a Value,
        }
        OsBackupSer {
            tor_key: &self.tor_key,
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
            int_ca_key: String::from_utf8(
                self.int_ca_key
                    .private_key_to_pem_pkcs8()
                    .map_err(serde::ser::Error::custom)?,
            )
            .map_err(serde::ser::Error::custom)?,
            int_ca_cert: String::from_utf8(
                self.int_ca_cert
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
pub async fn backup_all(
    #[context] ctx: RpcContext,
    #[arg] logicalname: PathBuf,
    #[arg] password: String,
) -> Result<WithRevision<()>, Error> {
    let mut db = ctx.db.handle();
    check_password(&mut ctx.secret_store.acquire().await?, &password).await?;
    let revision = assure_backing_up(&mut db).await?;
    tokio::task::spawn(async move {
        match perform_backup(&ctx, &mut db, logicalname, &password).await {
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
                )
                .await
                .expect("failed to send notification"),
            Err(e) => ctx
                .notification_manager
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
                )
                .await
                .expect("failed to send notification"),
        }
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

#[instrument(skip(ctx, db, password))]
async fn perform_backup<Db: DbHandle>(
    ctx: &RpcContext,
    mut db: Db,
    logical_name: PathBuf,
    password: &str,
) -> Result<BTreeMap<PackageId, PackageBackupReport>, Error> {
    mount(logical_name, BACKUP_MNT).await?;
    mount_ecryptfs(BACKUP_DIR_CRYPT, BACKUP_DIR, password).await?;

    let mounted = GeneralGuard::new(|| {
        tokio::spawn(async move {
            unmount(BACKUP_DIR).await?;
            unmount(BACKUP_MNT).await
        })
    });

    let metadata_path = Path::new(BACKUP_DIR).join("metadata.cbor");
    let metadata_tmp_path = Path::new(BACKUP_DIR).join(".metadata.cbor.tmp");
    let mut metadata: BackupInfo = if tokio::fs::metadata(&metadata_path).await.is_ok() {
        IoFormat::Cbor.from_slice(&tokio::fs::read(&metadata_path).await.with_ctx(|_| {
            (
                crate::ErrorKind::Filesystem,
                metadata_path.display().to_string(),
            )
        })?)?
    } else {
        Default::default()
    };
    metadata.version = crate::version::Current::new().semver().into();

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
        installed_model.lock(&mut db, LockType::Write).await;
        let manifest = installed_model
            .clone()
            .manifest()
            .get(&mut db, true)
            .await?;
        let main_status_model = installed_model.clone().status().main();
        let (started, health) = match main_status_model.get(&mut db, true).await?.into_owned() {
            MainStatus::Running { started, health } => (Some(started.clone()), health.clone()),
            MainStatus::Stopped | MainStatus::Stopping => (None, Default::default()),
            MainStatus::Restoring { .. } => {
                backup_report.insert(
                    package_id,
                    PackageBackupReport {
                        error: Some(
                            "Can't do backup because service is in a restoring state".to_owned(),
                        ),
                    },
                );
                continue;
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
                &mut db,
                &MainStatus::BackingUp {
                    started: started.clone(),
                    health: health.clone(),
                },
            )
            .await?;

        let res = manifest
            .backup
            .create(
                &ctx,
                &package_id,
                &manifest.version,
                &manifest.interfaces,
                &manifest.volumes,
            )
            .await;
        backup_report.insert(
            package_id,
            PackageBackupReport {
                error: res.err().map(|e| e.to_string()),
            },
        );

        main_status_model
            .put(
                &mut db,
                &match started {
                    Some(started) => MainStatus::Running { started, health },
                    None => MainStatus::Stopped,
                },
            )
            .await?;
    }
    let backup_tmp_path = Path::new(BACKUP_DIR).join("os-backup.cbor.tmp");
    let backup_path = Path::new(BACKUP_DIR).join("os-backup.cbor");

    // let secrets

    let s9pk = ctx.datadir.join(PKG_ARCHIVE_DIR);
    Command::new("cp")
        .arg("-r")
        .arg(s9pk)
        .arg(Path::new(BACKUP_DIR).join("archive"))
        .invoke(crate::ErrorKind::Filesystem)
        .await?;

    mounted.drop().await.with_kind(ErrorKind::Unknown)??;
    Ok(backup_report)
}
