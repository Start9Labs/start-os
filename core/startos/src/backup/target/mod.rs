use std::collections::BTreeMap;
use std::path::{Path, PathBuf};

use async_trait::async_trait;
use chrono::{DateTime, Utc};
use clap::{ArgMatches, Parser};
use color_eyre::eyre::eyre;
use digest::generic_array::GenericArray;
use digest::OutputSizeUser;
use models::PackageId;
use rpc_toolkit::{command, from_fn_async, ParentHandler};
use serde::{Deserialize, Serialize};
use sha2::Sha256;
use sqlx::{Executor, Postgres};
use tokio::sync::Mutex;
use tracing::instrument;

use self::cifs::CifsBackupTarget;
use crate::context::{CliContext, RpcContext};
use crate::disk::mount::backup::BackupMountGuard;
use crate::disk::mount::filesystem::block_dev::BlockDev;
use crate::disk::mount::filesystem::cifs::Cifs;
use crate::disk::mount::filesystem::{FileSystem, MountType, ReadWrite};
use crate::disk::mount::guard::TmpMountGuard;
use crate::disk::util::PartitionInfo;
use crate::prelude::*;
use crate::util::serde::{deserialize_from_str, display_serializable, serialize_display};
use crate::util::Version;

pub mod cifs;

#[derive(Debug, Deserialize, Serialize)]
#[serde(tag = "type")]
#[serde(rename_all = "kebab-case")]
pub enum BackupTarget {
    #[serde(rename_all = "kebab-case")]
    Disk {
        vendor: Option<String>,
        model: Option<String>,
        #[serde(flatten)]
        partition_info: PartitionInfo,
    },
    Cifs(CifsBackupTarget),
}

#[derive(Debug, PartialEq, Eq, PartialOrd, Ord, Clone)]
pub enum BackupTargetId {
    Disk { logicalname: PathBuf },
    Cifs { id: i32 },
}
impl BackupTargetId {
    pub async fn load<Ex>(self, secrets: &mut Ex) -> Result<BackupTargetFS, Error>
    where
        for<'a> &'a mut Ex: Executor<'a, Database = Postgres>,
    {
        Ok(match self {
            BackupTargetId::Disk { logicalname } => {
                BackupTargetFS::Disk(BlockDev::new(logicalname))
            }
            BackupTargetId::Cifs { id } => BackupTargetFS::Cifs(cifs::load(secrets, id).await?),
        })
    }
}
impl std::fmt::Display for BackupTargetId {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            BackupTargetId::Disk { logicalname } => write!(f, "disk-{}", logicalname.display()),
            BackupTargetId::Cifs { id } => write!(f, "cifs-{}", id),
        }
    }
}
impl std::str::FromStr for BackupTargetId {
    type Err = Error;
    fn from_str(s: &str) -> Result<Self, Self::Err> {
        match s.split_once('-') {
            Some(("disk", logicalname)) => Ok(BackupTargetId::Disk {
                logicalname: Path::new(logicalname).to_owned(),
            }),
            Some(("cifs", id)) => Ok(BackupTargetId::Cifs { id: id.parse()? }),
            _ => Err(Error::new(
                eyre!("Invalid Backup Target ID"),
                ErrorKind::InvalidBackupTargetId,
            )),
        }
    }
}
impl<'de> Deserialize<'de> for BackupTargetId {
    fn deserialize<D>(deserializer: D) -> Result<Self, D::Error>
    where
        D: serde::Deserializer<'de>,
    {
        deserialize_from_str(deserializer)
    }
}
impl Serialize for BackupTargetId {
    fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
    where
        S: serde::Serializer,
    {
        serialize_display(self, serializer)
    }
}

#[derive(Debug, Deserialize, Serialize)]
#[serde(tag = "type")]
#[serde(rename_all = "kebab-case")]
pub enum BackupTargetFS {
    Disk(BlockDev<PathBuf>),
    Cifs(Cifs),
}
#[async_trait]
impl FileSystem for BackupTargetFS {
    async fn mount<P: AsRef<Path> + Send + Sync>(
        &self,
        mountpoint: P,
        mount_type: MountType,
    ) -> Result<(), Error> {
        match self {
            BackupTargetFS::Disk(a) => a.mount(mountpoint, mount_type).await,
            BackupTargetFS::Cifs(a) => a.mount(mountpoint, mount_type).await,
        }
    }
    async fn source_hash(
        &self,
    ) -> Result<GenericArray<u8, <Sha256 as OutputSizeUser>::OutputSize>, Error> {
        match self {
            BackupTargetFS::Disk(a) => a.source_hash().await,
            BackupTargetFS::Cifs(a) => a.source_hash().await,
        }
    }
}

// #[command(subcommands(cifs::cifs, list, info, mount, umount))]
pub fn target() -> ParentHandler {
    ParentHandler::new()
        .subcommand("cifs", cifs::cifs())
        .subcommand("list", from_fn_async(list).remote_cli::<CliContext>())
        .subcommand("info", from_fn_async(info).remote_cli::<CliContext>())
}

// #[command(display(display_serializable))]
pub async fn list(ctx: RpcContext) -> Result<BTreeMap<BackupTargetId, BackupTarget>, Error> {
    let mut sql_handle = ctx.secret_store.acquire().await?;
    let (disks_res, cifs) = tokio::try_join!(
        crate::disk::util::list(&ctx.os_partitions),
        cifs::list(sql_handle.as_mut()),
    )?;
    Ok(disks_res
        .into_iter()
        .flat_map(|mut disk| {
            std::mem::take(&mut disk.partitions)
                .into_iter()
                .map(|part| {
                    (
                        BackupTargetId::Disk {
                            logicalname: part.logicalname.clone(),
                        },
                        BackupTarget::Disk {
                            vendor: disk.vendor.clone(),
                            model: disk.model.clone(),
                            partition_info: part,
                        },
                    )
                })
                .collect::<Vec<_>>()
        })
        .chain(
            cifs.into_iter()
                .map(|(id, cifs)| (BackupTargetId::Cifs { id }, BackupTarget::Cifs(cifs))),
        )
        .collect())
}

#[derive(Clone, Debug, Default, Deserialize, Serialize)]
#[serde(rename_all = "kebab-case")]
pub struct BackupInfo {
    pub version: Version,
    pub timestamp: Option<DateTime<Utc>>,
    pub package_backups: BTreeMap<PackageId, PackageBackupInfo>,
}

#[derive(Clone, Debug, Deserialize, Serialize)]
#[serde(rename_all = "kebab-case")]
pub struct PackageBackupInfo {
    pub title: String,
    pub version: Version,
    pub os_version: Version,
    pub timestamp: DateTime<Utc>,
}

fn display_backup_info(info: BackupInfo, matches: &ArgMatches) {
    use prettytable::*;

    if matches.is_present("format") {
        return display_serializable(info, matches);
    }

    let mut table = Table::new();
    table.add_row(row![bc =>
        "ID",
        "VERSION",
        "OS VERSION",
        "TIMESTAMP",
    ]);
    table.add_row(row![
        "EMBASSY OS",
        info.version.as_str(),
        info.version.as_str(),
        &if let Some(ts) = &info.timestamp {
            ts.to_string()
        } else {
            "N/A".to_owned()
        },
    ]);
    for (id, info) in info.package_backups {
        let row = row![
            &*id,
            info.version.as_str(),
            info.os_version.as_str(),
            &info.timestamp.to_string(),
        ];
        table.add_row(row);
    }
    table.print_tty(false).unwrap();
}

#[derive(Deserialize, Serialize, Parser)]
#[serde(rename_all = "kebab-case")]
#[command(rename_all = "kebab-case")]
pub struct InfoParams {
    target_id: BackupTargetId,
    password: String,
}

#[instrument(skip(ctx, password))]
pub async fn info(
    ctx: RpcContext,
    InfoParams {
        target_id,
        password,
    }: InfoParams,
) -> Result<BackupInfo, Error> {
    let guard = BackupMountGuard::mount(
        TmpMountGuard::mount(
            &target_id
                .load(ctx.secret_store.acquire().await?.as_mut())
                .await?,
            ReadWrite,
        )
        .await?,
        &password,
    )
    .await?;

    let res = guard.metadata.clone();

    guard.unmount().await?;

    Ok(res)
}

lazy_static::lazy_static! {
    static ref USER_MOUNTS: Mutex<BTreeMap<BackupTargetId, BackupMountGuard<TmpMountGuard>>> =
        Mutex::new(BTreeMap::new());
}

#[derive(Deserialize, Serialize, Parser)]
#[serde(rename_all = "kebab-case")]
#[command(rename_all = "kebab-case")]
pub struct MountParams {
    target_id: BackupTargetId,
    password: String,
}

#[instrument(skip_all)]
pub async fn mount(
    ctx: RpcContext,
    MountParams {
        target_id,
        password,
    }: MountParams,
) -> Result<String, Error> {
    let mut mounts = USER_MOUNTS.lock().await;

    if let Some(existing) = mounts.get(&target_id) {
        return Ok(existing.as_ref().display().to_string());
    }

    let guard = BackupMountGuard::mount(
        TmpMountGuard::mount(
            &target_id
                .clone()
                .load(ctx.secret_store.acquire().await?.as_mut())
                .await?,
            ReadWrite,
        )
        .await?,
        &password,
    )
    .await?;

    let res = guard.as_ref().display().to_string();

    mounts.insert(target_id, guard);

    Ok(res)
}

#[derive(Deserialize, Serialize, Parser)]
#[serde(rename_all = "kebab-case")]
#[command(rename_all = "kebab-case")]
pub struct UmountParams {
    target_id: Option<BackupTargetId>,
}

#[instrument(skip_all)]
pub async fn umount(_: RpcContext, UmountParams { target_id }: UmountParams) -> Result<(), Error> {
    let mut mounts = USER_MOUNTS.lock().await; // TODO: move to context
    if let Some(target_id) = target_id {
        if let Some(existing) = mounts.remove(&target_id) {
            existing.unmount().await?;
        }
    } else {
        for (_, existing) in std::mem::take(&mut *mounts) {
            existing.unmount().await?;
        }
    }

    Ok(())
}
