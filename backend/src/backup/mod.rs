use std::collections::{BTreeMap, BTreeSet};
use std::path::{Path, PathBuf};

use chrono::{DateTime, Utc};
use color_eyre::eyre::eyre;
use helpers::AtomicFile;
use patch_db::{DbHandle, HasModel, LockType};
use reqwest::Url;
use rpc_toolkit::command;
use serde::{Deserialize, Serialize};
use sqlx::{Executor, Postgres};
use tokio::fs::File;
use tokio::io::AsyncWriteExt;
use tracing::instrument;

use self::target::PackageBackupInfo;
use crate::context::RpcContext;
use crate::dependencies::reconfigure_dependents_with_live_pointers;
use crate::id::ImageId;
use crate::install::PKG_ARCHIVE_DIR;
use crate::net::interface::{InterfaceId, Interfaces};
use crate::procedure::{NoOutput, PackageProcedure, ProcedureName};
use crate::s9pk::manifest::PackageId;
use crate::util::serde::IoFormat;
use crate::util::Version;
use crate::version::{Current, VersionT};
use crate::volume::{backup_dir, Volume, VolumeId, Volumes, BACKUP_DIR};
use crate::{Error, ErrorKind, ResultExt};

pub mod backup_bulk;
pub mod restore;
pub mod target;

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

#[command(subcommands(backup_bulk::backup_all, target::target))]
pub fn backup() -> Result<(), Error> {
    Ok(())
}

#[command(rename = "backup", subcommands(restore::restore_packages_rpc))]
pub fn package_backup() -> Result<(), Error> {
    Ok(())
}

#[derive(Deserialize, Serialize)]
struct BackupMetadata {
    pub timestamp: DateTime<Utc>,
    pub tor_keys: BTreeMap<InterfaceId, String>,
    pub marketplace_url: Option<Url>,
}

#[derive(Clone, Debug, Deserialize, Serialize, HasModel)]
pub struct BackupActions {
    pub create: PackageProcedure,
    pub restore: PackageProcedure,
}
impl BackupActions {
    pub fn validate(
        &self,
        eos_version: &Version,
        volumes: &Volumes,
        image_ids: &BTreeSet<ImageId>,
    ) -> Result<(), Error> {
        self.create
            .validate(eos_version, volumes, image_ids, false)
            .with_ctx(|_| (crate::ErrorKind::ValidateS9pk, "Backup Create"))?;
        self.restore
            .validate(eos_version, volumes, image_ids, false)
            .with_ctx(|_| (crate::ErrorKind::ValidateS9pk, "Backup Restore"))?;
        Ok(())
    }

    #[instrument(skip(ctx, db))]
    pub async fn create<Db: DbHandle>(
        &self,
        ctx: &RpcContext,
        db: &mut Db,
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
                ProcedureName::CreateBackup,
                &volumes,
                None,
                false,
                None,
            )
            .await?
            .map_err(|e| eyre!("{}", e.1))
            .with_kind(crate::ErrorKind::Backup)?;
        let tor_keys = interfaces
            .tor_keys(&mut ctx.secret_store.acquire().await?, pkg_id)
            .await?
            .into_iter()
            .map(|(id, key)| {
                (
                    id,
                    base32::encode(base32::Alphabet::RFC4648 { padding: true }, &key.as_bytes()),
                )
            })
            .collect();
        let marketplace_url = crate::db::DatabaseModel::new()
            .package_data()
            .idx_model(pkg_id)
            .expect(db)
            .await?
            .installed()
            .expect(db)
            .await?
            .marketplace_url()
            .get(db, true)
            .await?
            .into_owned();
        let tmp_path = Path::new(BACKUP_DIR)
            .join(pkg_id)
            .join(format!("{}.s9pk", pkg_id));
        let s9pk_path = ctx
            .datadir
            .join(PKG_ARCHIVE_DIR)
            .join(pkg_id)
            .join(pkg_version.as_str())
            .join(format!("{}.s9pk", pkg_id));
        let mut infile = File::open(&s9pk_path).await?;
        let mut outfile = AtomicFile::new(&tmp_path, None::<PathBuf>)
            .await
            .with_kind(ErrorKind::Filesystem)?;
        tokio::io::copy(&mut infile, &mut *outfile)
            .await
            .with_ctx(|_| {
                (
                    crate::ErrorKind::Filesystem,
                    format!("cp {} -> {}", s9pk_path.display(), tmp_path.display()),
                )
            })?;
        outfile.save().await.with_kind(ErrorKind::Filesystem)?;
        let timestamp = Utc::now();
        let metadata_path = Path::new(BACKUP_DIR).join(pkg_id).join("metadata.cbor");
        let mut outfile = AtomicFile::new(&metadata_path, None::<PathBuf>)
            .await
            .with_kind(ErrorKind::Filesystem)?;
        outfile
            .write_all(&IoFormat::Cbor.to_vec(&BackupMetadata {
                timestamp,
                tor_keys,
                marketplace_url,
            })?)
            .await?;
        outfile.save().await.with_kind(ErrorKind::Filesystem)?;
        Ok(PackageBackupInfo {
            os_version: Current::new().semver().into(),
            title: pkg_title.to_owned(),
            version: pkg_version.clone(),
            timestamp,
        })
    }

    #[instrument(skip(ctx, db, secrets))]
    pub async fn restore<Ex, Db: DbHandle>(
        &self,
        ctx: &RpcContext,
        db: &mut Db,
        secrets: &mut Ex,
        pkg_id: &PackageId,
        pkg_version: &Version,
        interfaces: &Interfaces,
        volumes: &Volumes,
    ) -> Result<(), Error>
    where
        for<'a> &'a mut Ex: Executor<'a, Database = Postgres>,
    {
        let mut volumes = volumes.clone();
        volumes.insert(VolumeId::Backup, Volume::Backup { readonly: true });
        self.restore
            .execute::<(), NoOutput>(
                ctx,
                pkg_id,
                pkg_version,
                ProcedureName::RestoreBackup,
                &volumes,
                None,
                false,
                None,
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
        for (iface, key) in metadata.tor_keys {
            let key_vec = base32::decode(base32::Alphabet::RFC4648 { padding: true }, &key)
                .ok_or_else(|| {
                    Error::new(
                        eyre!("invalid base32 string"),
                        crate::ErrorKind::Deserialization,
                    )
                })?;
            sqlx::query!(
                "INSERT INTO tor (package, interface, key) VALUES ($1, $2, $3) ON CONFLICT (package, interface) DO UPDATE SET key = $3",
                **pkg_id,
                *iface,
                key_vec,
            )
            .execute(&mut *secrets)
            .await?;
        }
        crate::db::DatabaseModel::new()
            .package_data()
            .lock(db, LockType::Write)
            .await?;
        let pde = crate::db::DatabaseModel::new()
            .package_data()
            .idx_model(pkg_id)
            .expect(db)
            .await?
            .installed()
            .expect(db)
            .await?;
        pde.clone()
            .interface_addresses()
            .put(db, &interfaces.install(&mut *secrets, pkg_id).await?)
            .await?;
        pde.marketplace_url()
            .put(db, &metadata.marketplace_url)
            .await?;

        let entry = crate::db::DatabaseModel::new()
            .package_data()
            .idx_model(pkg_id)
            .expect(db)
            .await?
            .installed()
            .expect(db)
            .await?
            .get(db, true)
            .await?;

        let receipts = crate::config::ConfigReceipts::new(db).await?;
        reconfigure_dependents_with_live_pointers(ctx, db, &receipts, &entry).await?;

        Ok(())
    }
}
