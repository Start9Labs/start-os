use std::path::Path;

use tokio::process::Command;
use tracing::instrument;

use crate::disk::fsck::RequiresReboot;
use crate::util::Invoke;
use crate::Error;

#[instrument(skip_all)]
pub async fn btrfs_check_readonly(logicalname: impl AsRef<Path>) -> Result<RequiresReboot, Error> {
    Command::new("btrfs")
        .arg("check")
        .arg("--readonly")
        .arg(logicalname.as_ref())
        .invoke(crate::ErrorKind::DiskManagement)
        .await?;

    Ok(RequiresReboot(false))
}

pub async fn btrfs_check_repair(logicalname: impl AsRef<Path>) -> Result<RequiresReboot, Error> {
    Command::new("btrfs")
        .arg("check")
        .arg("--repair")
        .arg(logicalname.as_ref())
        .invoke(crate::ErrorKind::DiskManagement)
        .await?;

    Ok(RequiresReboot(false))
}
