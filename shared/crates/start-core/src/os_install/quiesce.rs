use std::path::{Path, PathBuf};

use color_eyre::eyre::eyre;
use tokio::process::Command;

use crate::prelude::*;
use crate::util::Invoke;

/// `/dev/<part>` paths for every partition currently exposed on `disk_path`.
pub async fn list_partitions(disk_path: &Path) -> Result<Vec<PathBuf>, Error> {
    let disk_name = disk_path
        .file_name()
        .and_then(|s| s.to_str())
        .ok_or_else(|| {
            Error::new(
                eyre!("invalid disk path: {}", disk_path.display()),
                ErrorKind::BlockDevice,
            )
        })?;
    let sys_block = Path::new("/sys/block").join(disk_name);
    let mut out = Vec::new();
    let mut entries = match tokio::fs::read_dir(&sys_block).await {
        Ok(e) => e,
        Err(_) => return Ok(out),
    };
    while let Some(entry) = entries.next_entry().await.map_err(|e| {
        Error::new(
            eyre!("reading {}: {e}", sys_block.display()),
            ErrorKind::Filesystem,
        )
    })? {
        if tokio::fs::metadata(entry.path().join("partition"))
            .await
            .is_ok()
        {
            out.push(Path::new("/dev").join(entry.file_name()));
        }
    }
    Ok(out)
}

/// Best-effort teardown of OS-level claims (mounts, swap, LVM, dm, btrfs scan
/// cache) on every partition of `disk_path`. Doesn't touch on-disk bytes, so
/// `protect`ed partitions stay intact. Failures are logged and ignored — the
/// subsequent `partx --update` is the source of truth.
pub async fn quiesce_disk(disk_path: &Path) -> Result<(), Error> {
    let partitions = list_partitions(disk_path).await?;

    if let Err(e) = Command::new("swapoff")
        .arg("-a")
        .invoke(ErrorKind::DiskManagement)
        .await
    {
        tracing::warn!("swapoff -a failed during quiesce: {e}");
    }

    // umount -A all mountpoints; lazy-unmount on busy.
    for part in &partitions {
        if Command::new("umount")
            .arg("-A")
            .arg(part)
            .invoke(ErrorKind::Filesystem)
            .await
            .is_err()
        {
            if let Err(e) = Command::new("umount")
                .arg("-A")
                .arg("-l")
                .arg(part)
                .invoke(ErrorKind::Filesystem)
                .await
            {
                tracing::warn!("lazy umount of {} failed: {e}", part.display());
            }
        }
    }

    if let Err(e) = Command::new("vgchange")
        .arg("-an")
        .invoke(ErrorKind::DiskManagement)
        .await
    {
        tracing::warn!("vgchange -an failed during quiesce: {e}");
    }
    if let Err(e) = Command::new("dmsetup")
        .arg("remove_all")
        .arg("--force")
        .invoke(ErrorKind::DiskManagement)
        .await
    {
        tracing::warn!("dmsetup remove_all failed during quiesce: {e}");
    }

    // Drop btrfs scan cache so a later mount doesn't race with a pending re-scan.
    if let Err(e) = Command::new("btrfs")
        .arg("device")
        .arg("scan")
        .arg("--forget")
        .invoke(ErrorKind::DiskManagement)
        .await
    {
        tracing::warn!("btrfs device scan --forget failed during quiesce: {e}");
    }

    Ok(())
}

/// Per-entry BLKPG update via `partx`. Tolerates a still-busy partition on the
/// same disk (unlike BLKRRPART, which would fail with EBUSY).
pub async fn update_partition_table(disk_path: &Path) -> Result<(), Error> {
    Command::new("partx")
        .arg("--update")
        .arg(disk_path)
        .invoke(ErrorKind::DiskManagement)
        .await?;
    Command::new("udevadm")
        .arg("settle")
        .invoke(ErrorKind::DiskManagement)
        .await?;
    // Forget any btrfs scan refs udev just re-installed, for the same reason as
    // in quiesce_disk.
    if let Err(e) = Command::new("btrfs")
        .arg("device")
        .arg("scan")
        .arg("--forget")
        .invoke(ErrorKind::DiskManagement)
        .await
    {
        tracing::warn!("btrfs device scan --forget after partx failed: {e}");
    }
    Ok(())
}
