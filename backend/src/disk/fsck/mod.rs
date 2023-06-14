use std::path::Path;

use color_eyre::eyre::eyre;
use tokio::process::Command;

use crate::disk::fsck::btrfs::{btrfs_check_readonly, btrfs_check_repair};
use crate::disk::fsck::ext4::{e2fsck_aggressive, e2fsck_preen};
use crate::util::Invoke;
use crate::Error;

pub mod btrfs;
pub mod ext4;

#[derive(Debug, Clone, Copy)]
#[must_use]
pub struct RequiresReboot(pub bool);
impl std::ops::BitOrAssign for RequiresReboot {
    fn bitor_assign(&mut self, rhs: Self) {
        self.0 |= rhs.0
    }
}

#[derive(Debug, Clone, Copy)]
pub enum RepairStrategy {
    Preen,
    Aggressive,
}
impl RepairStrategy {
    pub async fn fsck(
        &self,
        logicalname: impl AsRef<Path> + std::fmt::Debug,
    ) -> Result<RequiresReboot, Error> {
        match &*String::from_utf8(
            Command::new("grub-probe")
                .arg("-d")
                .arg(logicalname.as_ref())
                .invoke(crate::ErrorKind::DiskManagement)
                .await?,
        )? {
            "ext2" => self.e2fsck(logicalname).await,
            "btrfs" => self.btrfs_check(logicalname).await,
            fs => {
                return Err(Error::new(
                    eyre!("Unknown filesystem {fs}"),
                    crate::ErrorKind::DiskManagement,
                ))
            }
        }
    }
    pub async fn e2fsck(
        &self,
        logicalname: impl AsRef<Path> + std::fmt::Debug,
    ) -> Result<RequiresReboot, Error> {
        match self {
            RepairStrategy::Preen => e2fsck_preen(logicalname).await,
            RepairStrategy::Aggressive => e2fsck_aggressive(logicalname).await,
        }
    }
    pub async fn btrfs_check(
        &self,
        logicalname: impl AsRef<Path> + std::fmt::Debug,
    ) -> Result<RequiresReboot, Error> {
        match self {
            RepairStrategy::Preen => btrfs_check_readonly(logicalname).await,
            RepairStrategy::Aggressive => btrfs_check_repair(logicalname).await,
        }
    }
}
