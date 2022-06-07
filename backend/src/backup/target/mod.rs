use std::collections::BTreeMap;
use std::path::{Path, PathBuf};

use async_trait::async_trait;
use chrono::{DateTime, Utc};
use clap::ArgMatches;
use color_eyre::eyre::eyre;
use digest::generic_array::GenericArray;
use digest::{Digest, OutputSizeUser};
use rpc_toolkit::command;
use serde::{Deserialize, Serialize};
use sha2::Sha256;
use sqlx::{Executor, Sqlite};
use tracing::instrument;

use self::cifs::CifsBackupTarget;
use crate::context::RpcContext;
use crate::disk::mount::backup::BackupMountGuard;
use crate::disk::mount::filesystem::block_dev::BlockDev;
use crate::disk::mount::filesystem::cifs::Cifs;
use crate::disk::mount::filesystem::{FileSystem, MountType, ReadOnly};
use crate::disk::mount::guard::TmpMountGuard;
use crate::disk::util::PartitionInfo;
use crate::s9pk::manifest::PackageId;
use crate::util::serde::{deserialize_from_str, display_serializable, serialize_display};
use crate::util::Version;
use crate::Error;

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

#[derive(Debug, PartialEq, Eq, PartialOrd, Ord)]
pub enum BackupTargetId {
    Disk { logicalname: PathBuf },
    Cifs { id: u32 },
}
impl BackupTargetId {
    pub async fn load<Ex>(self, secrets: &mut Ex) -> Result<BackupTargetFS, Error>
    where
        for<'a> &'a mut Ex: Executor<'a, Database = Sqlite>,
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
        match s.split_once("-") {
            Some(("disk", logicalname)) => Ok(BackupTargetId::Disk {
                logicalname: Path::new(logicalname).to_owned(),
            }),
            Some(("cifs", id)) => Ok(BackupTargetId::Cifs { id: id.parse()? }),
            _ => Err(Error::new(
                eyre!("Invalid Backup Target ID"),
                crate::ErrorKind::InvalidBackupTargetId,
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

#[command(subcommands(cifs::cifs, list, info))]
pub fn target() -> Result<(), Error> {
    Ok(())
}

// TODO: incorporate reconnect into this response as well
#[command(display(display_serializable))]
pub async fn list(
    #[context] ctx: RpcContext,
) -> Result<BTreeMap<BackupTargetId, BackupTarget>, Error> {
    let mut sql_handle = ctx.secret_store.acquire().await?;
    let (disks_res, cifs) =
        tokio::try_join!(crate::disk::util::list(), cifs::list(&mut sql_handle),)?;
    Ok(disks_res
        .disks
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

fn display_backup_info(info: BackupInfo, matches: &ArgMatches<'_>) {
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
            id.as_str(),
            info.version.as_str(),
            info.os_version.as_str(),
            &info.timestamp.to_string(),
        ];
        table.add_row(row);
    }
    table.print_tty(false);
}

#[command(display(display_backup_info))]
#[instrument(skip(ctx, password))]
pub async fn info(
    #[context] ctx: RpcContext,
    #[arg(rename = "target-id")] target_id: BackupTargetId,
    #[arg] password: String,
) -> Result<BackupInfo, Error> {
    let guard = BackupMountGuard::mount(
        TmpMountGuard::mount(
            &target_id
                .load(&mut ctx.secret_store.acquire().await?)
                .await?,
            ReadOnly,
        )
        .await?,
        &password,
    )
    .await?;

    let res = guard.metadata.clone();

    guard.unmount().await?;

    Ok(res)
}
