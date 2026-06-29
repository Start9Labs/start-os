use std::collections::{BTreeMap, BTreeSet};
use std::path::{Path, PathBuf};

use color_eyre::eyre::eyre;
use tokio::process::Command;

use crate::disk::util::pvscan;
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

/// Canonical `/dev` node for `path`, falling back to the input on error.
async fn canonical(path: &Path) -> PathBuf {
    tokio::fs::canonicalize(path)
        .await
        .unwrap_or_else(|_| path.to_path_buf())
}

/// Block device backing the running live/install medium. Ventoy exposes the
/// booted ISO as a device-mapper device (`/dev/mapper/ventoy`); teardown must
/// never touch it or the OS we're running from disappears mid-install. Returns
/// `None` when there's no such mount (a directly-written USB boots off a plain
/// loop/partition that isn't a teardown target anyway).
async fn live_medium_device() -> Option<PathBuf> {
    let out = Command::new("findmnt")
        .arg("-n")
        .arg("-o")
        .arg("SOURCE")
        .arg("/run/live/medium")
        .invoke(ErrorKind::Filesystem)
        .await
        .ok()?;
    let src = String::from_utf8(out).ok()?;
    let src = src.trim().split('[').next().unwrap_or("").trim();
    if src.is_empty() {
        return None;
    }
    Some(canonical(Path::new(src)).await)
}

/// Direct holders of `dev` (`/sys/class/block/<dev>/holders/*`) as `/dev/<name>`.
async fn holders_of(dev: &Path) -> Vec<PathBuf> {
    let Some(name) = dev.file_name() else {
        return Vec::new();
    };
    let dir = Path::new("/sys/class/block").join(name).join("holders");
    let mut out = Vec::new();
    let Ok(mut entries) = tokio::fs::read_dir(&dir).await else {
        return out;
    };
    while let Ok(Some(entry)) = entries.next_entry().await {
        out.push(Path::new("/dev").join(entry.file_name()));
    }
    out
}

/// device-mapper name (`/sys/class/block/dm-N/dm/name`) for `dev`, or `None`
/// when `dev` isn't a device-mapper device.
async fn dm_name(dev: &Path) -> Option<String> {
    let name = dev.file_name()?;
    let s = tokio::fs::read_to_string(Path::new("/sys/class/block").join(name).join("dm/name"))
        .await
        .ok()?;
    let s = s.trim().to_owned();
    (!s.is_empty()).then_some(s)
}

/// Every device-mapper device stacked on `partitions`, ordered top-of-stack
/// first (deepest holder first) so `dmsetup remove` never hits a still-held
/// parent. Walks the holder graph in sysfs; anything in `exclude` (and devices
/// reached only through it) is skipped.
async fn stacked_dm_devices(partitions: &[PathBuf], exclude: &BTreeSet<PathBuf>) -> Vec<PathBuf> {
    let mut depth: BTreeMap<PathBuf, usize> = BTreeMap::new();
    let mut frontier: Vec<PathBuf> = partitions.to_vec();
    let mut d = 0usize;
    while !frontier.is_empty() && d < 64 {
        let mut next = Vec::new();
        for dev in &frontier {
            if exclude.contains(&canonical(dev).await) {
                continue;
            }
            for holder in holders_of(dev).await {
                let ch = canonical(&holder).await;
                if exclude.contains(&ch) {
                    continue;
                }
                let slot = depth.entry(ch.clone()).or_insert(0);
                *slot = (*slot).max(d + 1);
                next.push(ch);
            }
        }
        frontier = next;
        d += 1;
    }
    let mut devs: Vec<(PathBuf, usize)> = depth.into_iter().collect();
    devs.sort_by(|a, b| b.1.cmp(&a.1));
    devs.into_iter().map(|(dev, _)| dev).collect()
}

/// VG names whose PVs sit on one of `target_devs`.
async fn target_vgs(target_devs: &BTreeSet<PathBuf>) -> Vec<String> {
    let pvs = match pvscan().await {
        Ok(p) => p,
        Err(e) => {
            tracing::warn!("pvscan during quiesce failed: {e}");
            return Vec::new();
        }
    };
    let mut vgs = BTreeSet::new();
    for (pv, vg) in pvs {
        if let Some(vg) = vg {
            if target_devs.contains(&canonical(&pv).await) {
                vgs.insert(vg.to_string());
            }
        }
    }
    vgs.into_iter().collect()
}

/// Best-effort teardown of OS-level claims (mounts, swap, LVM, dm, btrfs scan
/// cache) on `disk_path` **and only `disk_path`**. Every step is scoped to the
/// target disk's own partitions and the devices stacked on them, so a live boot
/// medium on another disk — notably a Ventoy `/dev/mapper` device the running OS
/// reads from — is never torn down. Doesn't touch on-disk bytes, so `protect`ed
/// partitions stay intact. Failures are logged and ignored — the subsequent
/// `partx --update` is the source of truth.
pub async fn quiesce_disk(disk_path: &Path) -> Result<(), Error> {
    let partitions = list_partitions(disk_path).await?;

    let mut exclude = BTreeSet::new();
    if let Some(medium) = live_medium_device().await {
        exclude.insert(medium);
    }

    let dm_devices = stacked_dm_devices(&partitions, &exclude).await;

    // Canonical set of every block device that belongs to the target disk.
    let mut target_devs: BTreeSet<PathBuf> = BTreeSet::new();
    for part in &partitions {
        target_devs.insert(canonical(part).await);
    }
    target_devs.extend(dm_devices.iter().cloned());

    // swapoff only the target's own devices.
    for dev in &target_devs {
        if let Err(e) = Command::new("swapoff")
            .arg(dev)
            .invoke(ErrorKind::DiskManagement)
            .await
        {
            tracing::debug!("swapoff {} skipped: {e}", dev.display());
        }
    }

    // umount -A every mountpoint on the target's devices; lazy-unmount on busy.
    for dev in &target_devs {
        if Command::new("umount")
            .arg("-A")
            .arg(dev)
            .invoke(ErrorKind::Filesystem)
            .await
            .is_err()
        {
            if let Err(e) = Command::new("umount")
                .arg("-A")
                .arg("-l")
                .arg(dev)
                .invoke(ErrorKind::Filesystem)
                .await
            {
                tracing::warn!("lazy umount of {} failed: {e}", dev.display());
            }
        }
    }

    // Deactivate only VGs whose PVs sit on the target disk.
    for vg in target_vgs(&target_devs).await {
        if let Err(e) = Command::new("vgchange")
            .arg("-an")
            .arg(&vg)
            .invoke(ErrorKind::DiskManagement)
            .await
        {
            tracing::warn!("vgchange -an {vg} during quiesce failed: {e}");
        }
    }

    // Remove only the dm devices stacked on the target, top-of-stack first.
    for dev in &dm_devices {
        let Some(name) = dm_name(dev).await else {
            continue; // already gone (e.g. cleared by vgchange) or not a dm device
        };
        if let Err(e) = Command::new("dmsetup")
            .arg("remove")
            .arg("--force")
            .arg(&name)
            .invoke(ErrorKind::DiskManagement)
            .await
        {
            tracing::warn!("dmsetup remove {name} during quiesce failed: {e}");
        }
    }

    // Drop btrfs scan cache for the target's partitions only, so a later mount
    // doesn't race with a pending re-scan — without forgetting the live medium.
    for part in &partitions {
        if let Err(e) = Command::new("btrfs")
            .arg("device")
            .arg("scan")
            .arg("--forget")
            .arg(part)
            .invoke(ErrorKind::DiskManagement)
            .await
        {
            tracing::debug!("btrfs forget {} skipped: {e}", part.display());
        }
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
    // Forget any btrfs scan refs udev just re-installed for the target's
    // partitions only, for the same reason as in quiesce_disk.
    for part in list_partitions(disk_path).await.unwrap_or_default() {
        if let Err(e) = Command::new("btrfs")
            .arg("device")
            .arg("scan")
            .arg("--forget")
            .arg(&part)
            .invoke(ErrorKind::DiskManagement)
            .await
        {
            tracing::warn!(
                "btrfs device scan --forget {} after partx failed: {e}",
                part.display()
            );
        }
    }
    Ok(())
}
