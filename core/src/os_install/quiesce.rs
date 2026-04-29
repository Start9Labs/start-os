use std::path::{Path, PathBuf};

use color_eyre::eyre::eyre;
use tokio::process::Command;

use crate::prelude::*;
use crate::util::Invoke;

/// List `/dev/<part>` paths for every partition currently exposed on `disk_path`.
///
/// Walks `/sys/block/<disk>/` and picks entries that have a `partition` attribute file
/// (the kernel's marker for "this is a partition of the parent block device").
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

/// Best-effort teardown of every kernel-side reference to the partitions on `disk_path`,
/// so the on-disk partition table can be rewritten and `partx --update` will accept the
/// new layout.
///
/// This is intentionally aggressive: VGs are deactivated, dm/md devices torn down, all
/// mountpoints of every partition on the target disk are unmounted, and the kernel's
/// btrfs scan cache is forgotten. None of these touches the *bytes* on the disk, so a
/// `protect`-style preserved partition is safe — only the OS-level claims on it are
/// dropped (and they need to be: a mounted partition keeps the kernel from seeing
/// partition-table updates around it).
///
/// Failures are logged at WARN and ignored. The next step (`partx --update`) is what
/// actually decides whether the new layout took, and surfaces the precise per-entry
/// reason if it didn't.
pub async fn quiesce_disk(disk_path: &Path) -> Result<(), Error> {
    let partitions = list_partitions(disk_path).await?;

    // Unmount any swap that might be backed by these partitions. `swapoff -a` is broad
    // but on the install medium nothing else legitimately uses swap.
    if let Err(e) = Command::new("swapoff")
        .arg("-a")
        .invoke(ErrorKind::DiskManagement)
        .await
    {
        tracing::warn!("swapoff -a failed during quiesce: {e}");
    }

    // Unmount every mountpoint of every partition on the target disk. `umount -A`
    // unmounts all mountpoints that have this source; fall back to lazy unmount if the
    // kernel says it's busy (we're about to overwrite the partition anyway).
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

    // Deactivate any LVM VG, then any remaining device-mapper holders.
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

    // Drop the kernel's btrfs device-scan cache. Without this, mounting a freshly
    // mkfs'd btrfs over a partition the kernel previously scanned can race with the
    // pending re-scan and surface as `mount` exit 32.
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

/// Tell the kernel about the new partition table on `disk_path` without requiring an
/// exclusive open of the whole disk.
///
/// `partx --update` issues per-entry `BLKPG_*` ioctls (add/delete/resize) and tolerates
/// other partitions on the same disk being in use, so it works even when a protected
/// data partition is still claimed by something we couldn't tear down. This replaces
/// the old `blockdev --rereadpt` (BLKRRPART) path which fails with EBUSY whenever any
/// partition on the disk is still held.
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
    // udev re-runs blkid/btrfs scan against the freshly-announced partitions. Forget
    // any scan refs it may have just installed, so the next mount of the new rootfs
    // partition isn't racing the kernel's btrfs cache.
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
