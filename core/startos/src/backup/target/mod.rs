use std::collections::BTreeMap;
use std::path::{Path, PathBuf};

use chrono::{DateTime, Utc};
use clap::builder::ValueParserFactory;
use clap::Parser;
use color_eyre::eyre::eyre;
use digest::generic_array::GenericArray;
use digest::OutputSizeUser;
use models::PackageId;
use rpc_toolkit::{from_fn_async, Context, HandlerExt, ParentHandler};
use serde::{Deserialize, Serialize};
use sha2::Sha256;
use tokio::sync::Mutex;
use tracing::instrument;
use ts_rs::TS;

use self::cifs::CifsBackupTarget;
use crate::context::{CliContext, RpcContext};
use crate::db::model::DatabaseModel;
use crate::disk::mount::backup::BackupMountGuard;
use crate::disk::mount::filesystem::block_dev::BlockDev;
use crate::disk::mount::filesystem::cifs::Cifs;
use crate::disk::mount::filesystem::{FileSystem, MountType, ReadWrite};
use crate::disk::mount::guard::{GenericMountGuard, TmpMountGuard};
use crate::disk::util::PartitionInfo;
use crate::prelude::*;
use crate::util::clap::FromStrParser;
use crate::util::serde::{
    deserialize_from_str, display_serializable, serialize_display, HandlerExtSerde, WithIoFormat,
};
use crate::util::VersionString;

pub mod cifs;

#[derive(Debug, Deserialize, Serialize)]
#[serde(tag = "type")]
#[serde(rename_all = "camelCase")]
pub enum BackupTarget {
    #[serde(rename_all = "camelCase")]
    Disk {
        vendor: Option<String>,
        model: Option<String>,
        #[serde(flatten)]
        partition_info: PartitionInfo,
    },
    Cifs(CifsBackupTarget),
}

#[derive(Debug, PartialEq, Eq, PartialOrd, Ord, Clone, TS)]
#[ts(type = "string")]
pub enum BackupTargetId {
    Disk { logicalname: PathBuf },
    Cifs { id: u32 },
}
impl BackupTargetId {
    pub fn load(self, db: &DatabaseModel) -> Result<BackupTargetFS, Error> {
        Ok(match self {
            BackupTargetId::Disk { logicalname } => {
                BackupTargetFS::Disk(BlockDev::new(logicalname))
            }
            BackupTargetId::Cifs { id } => BackupTargetFS::Cifs(cifs::load(db, id)?),
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
impl ValueParserFactory for BackupTargetId {
    type Parser = FromStrParser<Self>;
    fn value_parser() -> Self::Parser {
        FromStrParser::new()
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

#[derive(Debug, Deserialize, Serialize, TS)]
#[serde(tag = "type")]
#[serde(rename_all = "camelCase")]
pub enum BackupTargetFS {
    Disk(BlockDev<PathBuf>),
    Cifs(Cifs),
}
impl FileSystem for BackupTargetFS {
    async fn mount<P: AsRef<Path> + Send>(
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
pub fn target<C: Context>() -> ParentHandler<C> {
    ParentHandler::new()
        .subcommand("cifs", cifs::cifs::<C>())
        .subcommand(
            "list",
            from_fn_async(list)
                .with_display_serializable()
                .with_call_remote::<CliContext>(),
        )
        .subcommand(
            "info",
            from_fn_async(info)
                .with_display_serializable()
                .with_custom_display_fn::<CliContext, _>(|params, info| {
                    Ok(display_backup_info(params.params, info))
                })
                .with_call_remote::<CliContext>(),
        )
}

// #[command(display(display_serializable))]
pub async fn list(ctx: RpcContext) -> Result<BTreeMap<BackupTargetId, BackupTarget>, Error> {
    let peek = ctx.db.peek().await;
    let (disks_res, cifs) = tokio::try_join!(
        crate::disk::util::list(&ctx.os_partitions),
        cifs::list(&peek),
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
#[serde(rename_all = "camelCase")]
pub struct BackupInfo {
    pub version: VersionString,
    pub timestamp: Option<DateTime<Utc>>,
    pub package_backups: BTreeMap<PackageId, PackageBackupInfo>,
}

#[derive(Clone, Debug, Deserialize, Serialize)]
#[serde(rename_all = "camelCase")]
pub struct PackageBackupInfo {
    pub title: String,
    pub version: VersionString,
    pub os_version: VersionString,
    pub timestamp: DateTime<Utc>,
}

fn display_backup_info(params: WithIoFormat<InfoParams>, info: BackupInfo) {
    use prettytable::*;

    if let Some(format) = params.format {
        return display_serializable(format, info);
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

#[derive(Deserialize, Serialize, Parser, TS)]
#[serde(rename_all = "camelCase")]
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
        TmpMountGuard::mount(&target_id.load(&ctx.db.peek().await)?, ReadWrite).await?,
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

#[derive(Deserialize, Serialize, Parser, TS)]
#[serde(rename_all = "camelCase")]
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
        return Ok(existing.path().display().to_string());
    }

    let guard = BackupMountGuard::mount(
        TmpMountGuard::mount(&target_id.clone().load(&ctx.db.peek().await)?, ReadWrite).await?,
        &password,
    )
    .await?;

    let res = guard.path().display().to_string();

    mounts.insert(target_id, guard);

    Ok(res)
}

#[derive(Deserialize, Serialize, Parser, TS)]
#[serde(rename_all = "camelCase")]
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
