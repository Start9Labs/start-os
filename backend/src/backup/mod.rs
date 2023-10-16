use std::collections::{BTreeMap, BTreeSet};
use std::path::{Path, PathBuf};
use std::sync::Arc;

use chrono::{DateTime, Utc};
use color_eyre::eyre::eyre;
use helpers::AtomicFile;
use models::{ImageId, OptionExt};
use reqwest::Url;
use rpc_toolkit::command;
use serde::{Deserialize, Serialize};
use tokio::fs::File;
use tokio::io::AsyncWriteExt;
use tracing::instrument;

use self::target::PackageBackupInfo;
use crate::context::RpcContext;
use crate::install::PKG_ARCHIVE_DIR;
use crate::manager::manager_seed::ManagerSeed;
use crate::net::interface::InterfaceId;
use crate::net::keys::Key;
use crate::prelude::*;
use crate::procedure::docker::DockerContainers;
use crate::procedure::{NoOutput, PackageProcedure, ProcedureName};
use crate::s9pk::manifest::PackageId;
use crate::util::serde::{Base32, Base64, IoFormat};
use crate::util::Version;
use crate::version::{Current, VersionT};
use crate::volume::{backup_dir, Volume, VolumeId, Volumes, BACKUP_DIR};
use crate::{Error, ErrorKind, ResultExt};

pub mod backup_bulk;
pub mod os;
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
    pub error: Option<String>,
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
    #[serde(default)]
    pub network_keys: BTreeMap<InterfaceId, Base64<[u8; 32]>>,
    #[serde(default)]
    pub tor_keys: BTreeMap<InterfaceId, Base32<[u8; 64]>>, // DEPRECATED
    pub marketplace_url: Option<Url>,
}

#[derive(Clone, Debug, Deserialize, Serialize, HasModel)]
#[model = "Model<Self>"]
pub struct BackupActions {
    pub create: PackageProcedure,
    pub restore: PackageProcedure,
}
impl BackupActions {
    pub fn validate(
        &self,
        _container: &Option<DockerContainers>,
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

    #[instrument(skip_all)]
    pub async fn create(&self, seed: Arc<ManagerSeed>) -> Result<PackageBackupInfo, Error> {
        let manifest = &seed.manifest;
        let mut volumes = seed.manifest.volumes.to_readonly();
        let ctx = &seed.ctx;
        let pkg_id = &manifest.id;
        let pkg_version = &manifest.version;
        volumes.insert(VolumeId::Backup, Volume::Backup { readonly: false });
        let backup_dir = backup_dir(&manifest.id);
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
                None,
            )
            .await?
            .map_err(|e| eyre!("{}", e.1))
            .with_kind(crate::ErrorKind::Backup)?;
        let (network_keys, tor_keys): (Vec<_>, Vec<_>) =
            Key::for_package(&ctx.secret_store, pkg_id)
                .await?
                .into_iter()
                .filter_map(|k| {
                    let interface = k.interface().map(|(_, i)| i)?;
                    Some((
                        (interface.clone(), Base64(k.as_bytes())),
                        (interface, Base32(k.tor_key().as_bytes())),
                    ))
                })
                .unzip();
        let marketplace_url = ctx
            .db
            .peek()
            .await
            .as_package_data()
            .as_idx(&pkg_id)
            .or_not_found(pkg_id)?
            .expect_as_installed()?
            .as_installed()
            .as_marketplace_url()
            .de()?;
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
        let network_keys = network_keys.into_iter().collect();
        let tor_keys = tor_keys.into_iter().collect();
        outfile
            .write_all(&IoFormat::Cbor.to_vec(&BackupMetadata {
                timestamp,
                network_keys,
                tor_keys,
                marketplace_url,
            })?)
            .await?;
        outfile.save().await.with_kind(ErrorKind::Filesystem)?;
        Ok(PackageBackupInfo {
            os_version: Current::new().semver().into(),
            title: manifest.title.clone(),
            version: pkg_version.clone(),
            timestamp,
        })
    }

    #[instrument(skip_all)]
    pub async fn restore(
        &self,
        ctx: &RpcContext,
        pkg_id: &PackageId,
        pkg_version: &Version,
        volumes: &Volumes,
    ) -> Result<Option<Url>, Error> {
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

        Ok(metadata.marketplace_url)
    }
}
