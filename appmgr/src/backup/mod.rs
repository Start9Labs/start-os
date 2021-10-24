use std::collections::BTreeMap;
use std::path::Path;

use chrono::{DateTime, Utc};
use color_eyre::eyre::eyre;
use patch_db::HasModel;
use rpc_toolkit::command;
use serde::{Deserialize, Serialize};
use tokio::fs::File;
use tokio::io::AsyncWriteExt;
use torut::onion::TorSecretKeyV3;
use tracing::instrument;

use crate::action::{ActionImplementation, NoOutput};
use crate::context::RpcContext;
use crate::disk::PackageBackupInfo;
use crate::install::PKG_ARCHIVE_DIR;
use crate::net::interface::{InterfaceId, Interfaces};
use crate::s9pk::manifest::PackageId;
use crate::util::{IoFormat, Version};
use crate::version::{Current, VersionT};
use crate::volume::{backup_dir, Volume, VolumeId, Volumes, BACKUP_DIR};
use crate::{Error, ResultExt};

mod backup_bulk;

#[derive(Debug, Deserialize, Serialize)]
pub struct BackupReport {
    server: ServerBackupReport,
    packages: BTreeMap<PackageId, PackageBackupReport>,
}

#[derive(Debug, Deserialize, Serialize)]
pub struct ServerBackupReport {
    attempted: bool,
    error: Option<String>,
}

#[derive(Debug, Deserialize, Serialize)]
pub struct PackageBackupReport {
    error: Option<String>,
}

#[command(subcommands(backup_bulk::backup_all))]
pub fn backup() -> Result<(), Error> {
    Ok(())
}

#[derive(Deserialize, Serialize)]
struct BackupMetadata {
    pub timestamp: DateTime<Utc>,
    pub tor_keys: BTreeMap<InterfaceId, TorSecretKeyV3>,
}

#[derive(Clone, Debug, Deserialize, Serialize, HasModel)]
pub struct BackupActions {
    pub create: ActionImplementation,
    pub restore: ActionImplementation,
}
impl BackupActions {
    #[instrument(skip(ctx))]
    pub async fn create(
        &self,
        ctx: &RpcContext,
        pkg_id: &PackageId,
        pkg_title: &str,
        pkg_version: &Version,
        interfaces: &Interfaces,
        volumes: &Volumes,
    ) -> Result<PackageBackupInfo, Error> {
        let mut volumes = volumes.to_readonly();
        volumes.insert(VolumeId::Backup, Volume::Backup { readonly: false });
        let backup_dir = backup_dir(pkg_id);
        if tokio::fs::metadata(&backup_dir).await.is_err() {
            tokio::fs::create_dir_all(&backup_dir).await?
        }
        self.create
            .execute::<(), NoOutput>(
                ctx,
                pkg_id,
                pkg_version,
                Some("CreateBackup"),
                &volumes,
                None,
                false,
            )
            .await?
            .map_err(|e| eyre!("{}", e.1))
            .with_kind(crate::ErrorKind::Backup)?;
        let tor_keys = interfaces
            .tor_keys(&mut ctx.secret_store.acquire().await?, pkg_id)
            .await?;
        let tmp_path = Path::new(BACKUP_DIR)
            .join(pkg_id)
            .join(format!("{}.s9pk", pkg_id));
        let real_path = Path::new(BACKUP_DIR)
            .join(pkg_id)
            .join(format!(".{}.s9pk.tmp", pkg_id));
        let s9pk_path = ctx
            .datadir
            .join(PKG_ARCHIVE_DIR)
            .join(pkg_id)
            .join(pkg_version.as_str())
            .join(format!("{}.s9pk", pkg_id));
        let mut infile = File::open(&s9pk_path).await?;
        let mut outfile = File::create(&tmp_path).await?;
        tokio::io::copy(&mut infile, &mut outfile)
            .await
            .with_ctx(|_| {
                (
                    crate::ErrorKind::Filesystem,
                    format!("cp {} -> {}", s9pk_path.display(), tmp_path.display()),
                )
            })?;
        outfile.flush().await?;
        outfile.shutdown().await?;
        outfile.sync_all().await?;
        tokio::fs::rename(&tmp_path, &real_path)
            .await
            .with_ctx(|_| {
                (
                    crate::ErrorKind::Filesystem,
                    format!("mv {} -> {}", tmp_path.display(), real_path.display()),
                )
            })?;
        let timestamp = Utc::now();
        let tmp_path = Path::new(BACKUP_DIR)
            .join(pkg_id)
            .join(".metadata.cbor.tmp");
        let real_path = Path::new(BACKUP_DIR).join(pkg_id).join("metadata.cbor");
        let mut outfile = File::create(&tmp_path).await?;
        outfile
            .write_all(&IoFormat::Cbor.to_vec(&BackupMetadata {
                timestamp,
                tor_keys,
            })?)
            .await?;
        outfile.flush().await?;
        outfile.shutdown().await?;
        outfile.sync_all().await?;
        tokio::fs::rename(&tmp_path, &real_path)
            .await
            .with_ctx(|_| {
                (
                    crate::ErrorKind::Filesystem,
                    format!("mv {} -> {}", tmp_path.display(), real_path.display()),
                )
            })?;
        Ok(PackageBackupInfo {
            os_version: Current::new().semver().into(),
            title: pkg_title.to_owned(),
            version: pkg_version.clone(),
            timestamp,
        })
    }

    pub async fn restore(
        &self,
        ctx: &RpcContext,
        pkg_id: &PackageId,
        pkg_version: &Version,
        volumes: &Volumes,
    ) -> Result<(), Error> {
        let mut volumes = volumes.clone();
        volumes.insert(VolumeId::Backup, Volume::Backup { readonly: true });
        self.restore
            .execute::<(), NoOutput>(
                ctx,
                pkg_id,
                pkg_version,
                Some("RestoreBackup"),
                &volumes,
                None,
                false,
            )
            .await?
            .map_err(|e| eyre!("{}", e.1))
            .with_kind(crate::ErrorKind::Restore)?;
        let metadata_path = Path::new(BACKUP_DIR).join(pkg_id).join("metadata.cbor");
        let metadata: BackupMetadata = IoFormat::Cbor.from_slice(
            &tokio::fs::read(&metadata_path).await.with_ctx(|_| {
                (
                    crate::ErrorKind::Filesystem,
                    metadata_path.display().to_string(),
                )
            })?,
        )?;
        let mut sql_handle = ctx.secret_store.acquire().await?;
        for (iface, key) in metadata.tor_keys {
            let key_vec = key.as_bytes().to_vec();
            sqlx::query!(
                "REPLACE INTO tor (package, interface, key) VALUES (?, ?, ?)",
                **pkg_id,
                *iface,
                key_vec,
            )
            .execute(&mut sql_handle)
            .await?;
        }
        Ok(())
    }
}
