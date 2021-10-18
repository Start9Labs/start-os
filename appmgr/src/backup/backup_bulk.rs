use std::collections::BTreeMap;
use std::path::{Path, PathBuf};
use std::sync::Arc;

use chrono::{DateTime, Utc};
use color_eyre::eyre::eyre;
use openssl::pkey::{PKey, Private};
use openssl::x509::X509;
use patch_db::{DbHandle, LockType, PatchDbHandle, Revision};
use rpc_toolkit::command;
use serde::{Deserialize, Serialize};
use serde_json::Value;
use tokio::fs::File;
use tokio::io::AsyncWriteExt;
use tokio::process::Command;
use torut::onion::TorSecretKeyV3;
use tracing::instrument;

use super::PackageBackupReport;
use crate::auth::{check_password, check_password_against_db};
use crate::backup::{BackupReport, ServerBackupReport};
use crate::context::RpcContext;
use crate::db::model::ServerStatus;
use crate::db::util::WithRevision;
use crate::disk::util::{mount, mount_ecryptfs, unmount, EmbassyOsRecoveryInfo};
use crate::disk::BackupInfo;
use crate::install::PKG_ARCHIVE_DIR;
use crate::middleware::encrypt::{decrypt_slice, encrypt_slice};
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
            ui: Value,
        }
        let int = OsBackupDe::deserialize(deserializer)?;
        Ok(OsBackup {
            tor_key: int.tor_key,
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
        struct OsBackupSer<'a> {
            tor_key: &'a TorSecretKeyV3,
            root_ca_key: String,
            root_ca_cert: String,
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
            ui: &self.ui,
        }
        .serialize(serializer)
    }
}

#[command(rename = "create", display(display_none))]
pub async fn backup_all(
    #[context] ctx: RpcContext,
    #[arg] logicalname: PathBuf,
    #[arg(rename = "old-password", long = "old-password")] old_password: Option<String>,
    #[arg] password: String,
) -> Result<WithRevision<()>, Error> {
    let mut db = ctx.db.handle();
    check_password_against_db(&mut ctx.secret_store.acquire().await?, &password).await?;
    let revision = assure_backing_up(&mut db).await?;
    tokio::task::spawn(async move {
        match perform_backup(
            &ctx,
            &mut db,
            logicalname,
            old_password.as_deref(),
            &password,
        )
        .await
        {
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

async fn write_cbor_file<T: Serialize>(
    value: &T,
    tmp_path: impl AsRef<Path>,
    path: impl AsRef<Path>,
) -> Result<(), Error> {
    let tmp_path = tmp_path.as_ref();
    let path = path.as_ref();
    let mut file = File::create(tmp_path)
        .await
        .with_ctx(|_| (ErrorKind::Filesystem, tmp_path.display().to_string()))?;
    file.write_all(&IoFormat::Cbor.to_vec(value)?).await?;
    file.flush().await?;
    file.shutdown().await?;
    file.sync_all().await?;
    drop(file);
    tokio::fs::rename(tmp_path, path).await.with_ctx(|_| {
        (
            ErrorKind::Filesystem,
            format!("mv {} -> {}", tmp_path.display(), path.display()),
        )
    })
}

#[instrument(skip(ctx, db, password))]
async fn perform_backup<Db: DbHandle>(
    ctx: &RpcContext,
    mut db: Db,
    logical_name: PathBuf,
    old_password: Option<&str>,
    password: &str,
) -> Result<BTreeMap<PackageId, PackageBackupReport>, Error> {
    mount(logical_name, BACKUP_MNT).await?;
    let tmp_guard = GeneralGuard::new(|| tokio::spawn(unmount(BACKUP_MNT)));

    let unencrypted_metadata_path =
        Path::new(BACKUP_MNT).join("EmbassyBackups/unencrypted-metadata.cbor");
    let unencrypted_metadata_tmp_path =
        Path::new(BACKUP_MNT).join("EmbassyBackups/.unencrypted-metadata.cbor.tmp");
    let mut unencrypted_metadata: EmbassyOsRecoveryInfo =
        if tokio::fs::metadata(&unencrypted_metadata_path)
            .await
            .is_ok()
        {
            IoFormat::Cbor.from_slice(
                &tokio::fs::read(&unencrypted_metadata_path)
                    .await
                    .with_ctx(|_| {
                        (
                            crate::ErrorKind::Filesystem,
                            unencrypted_metadata_path.display().to_string(),
                        )
                    })?,
            )?
        } else {
            Default::default()
        };
    let enc_key = if let (Some(hash), Some(wrapped_key)) = (
        unencrypted_metadata.password_hash.as_ref(),
        unencrypted_metadata.wrapped_key.as_ref(),
    ) {
        if let Some(old_password) = old_password {
            check_password(hash, old_password)?;
            String::from_utf8(decrypt_slice(wrapped_key, old_password))?
        } else {
            check_password(hash, password)?;
            String::from_utf8(decrypt_slice(wrapped_key, password))?
        }
    } else {
        base32::encode(
            base32::Alphabet::RFC4648 { padding: false },
            &rand::random::<[u8; 32]>()[..],
        )
    };
    unencrypted_metadata.version = crate::version::Current::new().semver().into();
    unencrypted_metadata.full = true;
    unencrypted_metadata.password_hash = Some(
        argon2::hash_encoded(
            password.as_bytes(),
            &rand::random::<[u8; 16]>()[..],
            &argon2::Config::default(),
        )
        .with_kind(crate::ErrorKind::PasswordHashGeneration)?,
    );
    unencrypted_metadata.wrapped_key = Some(base32::encode(
        base32::Alphabet::RFC4648 { padding: false },
        &encrypt_slice(enc_key, password),
    ));

    mount_ecryptfs(BACKUP_DIR_CRYPT, BACKUP_DIR, &enc_key).await?;

    tmp_guard.drop_without_action();
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
                error: res.as_ref().err().map(|e| e.to_string()),
            },
        );
        if let Ok(pkg_meta) = res {
            metadata
                .package_backups
                .insert(package_id.clone(), pkg_meta);
        }

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

    write_cbor_file(
        &OsBackup {
            tor_key: ctx.net_controller.tor.embassyd_tor_key().await,
            root_ca_key: ctx.net_controller.nginx.ssl.root_ca_key().await?,
            root_ca_cert: ctx.net_controller.nginx.ssl.root_ca_cert().await?,
            ui: crate::db::DatabaseModel::new()
                .ui()
                .get(&mut db, true)
                .await?
                .into_owned(),
        },
        backup_tmp_path,
        backup_path,
    )
    .await?;

    metadata.timestamp = Some(Utc::now());

    write_cbor_file(&metadata, metadata_tmp_path, metadata_path).await?;

    write_cbor_file(
        &unencrypted_metadata,
        unencrypted_metadata_tmp_path,
        unencrypted_metadata_path,
    )
    .await?;

    mounted.drop().await.with_kind(ErrorKind::Unknown)??;
    Ok(backup_report)
}
