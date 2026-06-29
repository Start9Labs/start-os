use std::io::Cursor;
use std::path::Path;

use tokio::process::Command;

use crate::Error;
use crate::disk::fsck::RequiresReboot;
use crate::util::Invoke;

pub async fn btrfs_check_repair(logicalname: impl AsRef<Path>) -> Result<RequiresReboot, Error> {
    Command::new("btrfs")
        .arg("check")
        .arg("--repair")
        .arg(logicalname.as_ref())
        .input(Some(&mut Cursor::new(b"y\n")))
        .invoke(crate::ErrorKind::DiskManagement)
        .await?;

    Ok(RequiresReboot(false))
}
