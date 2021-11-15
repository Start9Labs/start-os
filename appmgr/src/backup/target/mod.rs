use std::collections::BTreeMap;
use std::path::{Path, PathBuf};

use async_trait::async_trait;
use color_eyre::eyre::eyre;
use digest::generic_array::GenericArray;
use digest::Digest;
use rpc_toolkit::command;
use serde::{Deserialize, Serialize};
use sha2::Sha256;
use sqlx::{Executor, Sqlite};

use self::cifs::CifsBackupTarget;
use crate::context::RpcContext;
use crate::disk::mount::filesystem::block_dev::BlockDev;
use crate::disk::mount::filesystem::cifs::Cifs;
use crate::disk::mount::filesystem::FileSystem;
use crate::disk::util::PartitionInfo;
use crate::util::serde::{deserialize_from_str, display_serializable, serialize_display};
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
#[serde(rename_all = "kebab-case")]
pub enum BackupTargetFS {
    Disk(BlockDev<PathBuf>),
    Cifs(Cifs),
}
#[async_trait]
impl FileSystem for BackupTargetFS {
    async fn mount<P: AsRef<Path> + Send + Sync>(&self, mountpoint: P) -> Result<(), Error> {
        match self {
            BackupTargetFS::Disk(a) => a.mount(mountpoint).await,
            BackupTargetFS::Cifs(a) => a.mount(mountpoint).await,
        }
    }
    async fn source_hash(&self) -> Result<GenericArray<u8, <Sha256 as Digest>::OutputSize>, Error> {
        match self {
            BackupTargetFS::Disk(a) => a.source_hash().await,
            BackupTargetFS::Cifs(a) => a.source_hash().await,
        }
    }
}

#[command(subcommands(cifs::cifs, list))]
pub fn target() -> Result<(), Error> {
    Ok(())
}

#[command(display(display_serializable))]
pub async fn list(
    #[context] ctx: RpcContext,
) -> Result<BTreeMap<BackupTargetId, BackupTarget>, Error> {
    let mut sql_handle = ctx.secret_store.acquire().await?;
    let (disks, cifs) = tokio::try_join!(crate::disk::util::list(), cifs::list(&mut sql_handle),)?;
    Ok(disks
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
