use std::path::{Path, PathBuf};
use std::{collections::BTreeMap, sync::Arc};

use chrono::{DateTime, Utc};
use helpers::AtomicFile;
use models::{InterfaceId, ProcedureName};
use reqwest::Url;
use rpc_toolkit::command;
use serde::{Deserialize, Serialize};
use tokio::fs::File;
use tokio::io::AsyncWriteExt;
use tracing::instrument;

use self::target::PackageBackupInfo;
use crate::install::PKG_ARCHIVE_DIR;
use crate::manager::Manager;
use crate::net::keys::Key;
use crate::prelude::*;
use crate::s9pk::manifest::PackageId;
use crate::script::NoOutput;
use crate::util::serde::{Base32, Base64, IoFormat};
use crate::version::{Current, VersionT};
use crate::volume::BACKUP_DIR;

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

#[instrument(skip(manager))]
pub async fn create(manager: Arc<Manager>) -> Result<PackageBackupInfo, Error> {
    manager
        .clone()
        .run_procedure::<(), NoOutput>(ProcedureName::Main, None, None)
        .await?;

    let (network_keys, tor_keys) =
        Key::for_package(&manager.seed.ctx.secret_store, &manager.seed.manifest.id)
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
    let tmp_path = Path::new(BACKUP_DIR)
        .join(&manager.seed.manifest.id)
        .join(format!("{}.s9pk", &manager.seed.manifest.id));
    let s9pk_path = manager
        .seed
        .ctx
        .datadir
        .join(PKG_ARCHIVE_DIR)
        .join(&manager.seed.manifest.id)
        .join(&manager.seed.manifest.version.as_str())
        .join(format!("{}.s9pk", &manager.seed.manifest.id));
    let mut infile = File::open(&s9pk_path).await?;
    let mut outfile = AtomicFile::new(&tmp_path, None::<PathBuf>)
        .await
        .with_kind(ErrorKind::Filesystem)?;
    tokio::io::copy(&mut infile, &mut *outfile)
        .await
        .with_ctx(|_| {
            (
                ErrorKind::Filesystem,
                format!("cp {} -> {}", s9pk_path.display(), tmp_path.display()),
            )
        })?;
    outfile.save().await.with_kind(ErrorKind::Filesystem)?;
    let timestamp = Utc::now();
    let metadata_path = Path::new(BACKUP_DIR)
        .join(&manager.seed.manifest.id)
        .join("metadata.cbor");
    let mut outfile = AtomicFile::new(&metadata_path, None::<PathBuf>)
        .await
        .with_kind(ErrorKind::Filesystem)?;
    outfile
        .write_all(&IoFormat::Cbor.to_vec(&BackupMetadata {
            timestamp,
            network_keys,
            tor_keys,
            marketplace_url: manager.seed.marketplace_url.clone(),
        })?)
        .await?;
    outfile.save().await.with_kind(ErrorKind::Filesystem)?;
    Ok(PackageBackupInfo {
        os_version: Current::new().semver().into(),
        title: manager.seed.manifest.title.clone(),
        version: manager.seed.manifest.version.clone(),
        timestamp,
    })
}

#[instrument(skip(manager))]
pub async fn restore(manager: Arc<Manager>) -> Result<(), Error> {
    manager
        .run_procedure::<(), NoOutput>(ProcedureName::RestoreBackup, None, None)
        .await?;

    Ok(())
}
