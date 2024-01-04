use std::collections::{BTreeMap, BTreeSet};
use std::path::{Path, PathBuf};
use std::sync::Arc;

use chrono::{DateTime, Utc};
use color_eyre::eyre::eyre;
use helpers::AtomicFile;
use models::{ImageId, OptionExt, PackageId, ProcedureName};
use reqwest::Url;
use rpc_toolkit::{command, from_fn_async, ParentHandler};
use serde::{Deserialize, Serialize};
use tokio::fs::File;
use tokio::io::AsyncWriteExt;
use tracing::instrument;

use self::target::PackageBackupInfo;
use crate::context::{CliContext, RpcContext};
use crate::install::PKG_ARCHIVE_DIR;
use crate::manager::persistent_container::PersistentContainer;
use crate::net::interface::InterfaceId;
use crate::net::keys::Key;
use crate::prelude::*;
use crate::util::serde::{Base32, Base64, IoFormat, NoOutput};
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

// #[command(subcommands(backup_bulk::backup_all, target::target))]
pub fn backup() -> ParentHandler {
    ParentHandler::new()
        .subcommand(
            "create",
            from_fn_async(backup_bulk::backup_all)
                .no_display()
                .with_remote_cli::<CliContext>(),
        )
        .subcommand("target", target::target())
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

#[instrument(skip_all)]
pub async fn restore(
    ctx: &RpcContext,
    pkg_id: &PackageId,
    pkg_version: &Version,
    volumes: &Volumes,
) -> Result<Option<Url>, Error> {
    // let mut volumes = volumes.clone();
    // volumes.insert(VolumeId::Backup, Volume::Backup { readonly: true });
    // self.restore
    //     .execute::<(), NoOutput>(
    //         ctx,
    //         pkg_id,
    //         pkg_version,
    //         ProcedureName::RestoreBackup,
    //         &volumes,
    //         None,
    //         None,
    //     )
    //     .await?
    //     .map_err(|e| eyre!("{}", e.1))
    //     .with_kind(crate::ErrorKind::Restore)?;
    // let metadata_path = Path::new(BACKUP_DIR).join(pkg_id).join("metadata.cbor");
    // let metadata: BackupMetadata =
    //     IoFormat::Cbor.from_slice(&tokio::fs::read(&metadata_path).await.with_ctx(|_| {
    //         (
    //             crate::ErrorKind::Filesystem,
    //             metadata_path.display().to_string(),
    //         )
    //     })?)?;

    // Ok(metadata.marketplace_url)
    todo!()
}
