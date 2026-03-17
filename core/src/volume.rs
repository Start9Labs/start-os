use std::path::{Path, PathBuf};

use tokio::process::Command;

use crate::PackageId;
pub use crate::VolumeId;
use crate::prelude::*;
use crate::util::Invoke;
use crate::util::VersionString;
use crate::DATA_DIR;

pub const PKG_VOLUME_DIR: &str = "package-data/volumes";
pub const BACKUP_DIR: &str = "/media/startos/backups";

const INSTALL_BACKUP_SUFFIX: &str = ".install-backup";

pub fn data_dir<P: AsRef<Path>>(datadir: P, pkg_id: &PackageId, volume_id: &VolumeId) -> PathBuf {
    datadir
        .as_ref()
        .join(PKG_VOLUME_DIR)
        .join(pkg_id)
        .join("data")
        .join(volume_id)
}

pub fn asset_dir<P: AsRef<Path>>(
    datadir: P,
    pkg_id: &PackageId,
    version: &VersionString,
) -> PathBuf {
    datadir
        .as_ref()
        .join(PKG_VOLUME_DIR)
        .join(pkg_id)
        .join("assets")
        .join(version.as_str())
}

pub fn backup_dir(pkg_id: &PackageId) -> PathBuf {
    Path::new(BACKUP_DIR).join(pkg_id).join("data")
}

fn pkg_volume_dir(pkg_id: &PackageId) -> PathBuf {
    Path::new(DATA_DIR).join(PKG_VOLUME_DIR).join(pkg_id)
}

fn install_backup_path(pkg_id: &PackageId) -> PathBuf {
    Path::new(DATA_DIR)
        .join(PKG_VOLUME_DIR)
        .join(format!("{pkg_id}{INSTALL_BACKUP_SUFFIX}"))
}

/// Creates a COW snapshot of the package volume directory before install.
/// Uses `cp --reflink=always` so it's instant on btrfs and fails gracefully
/// on ext4 (no backup, current behavior preserved).
/// Returns `true` if a backup was created, `false` if no data existed or
/// the filesystem doesn't support reflinks.
pub async fn snapshot_volumes_for_install(pkg_id: &PackageId) -> Result<bool, Error> {
    let src = pkg_volume_dir(pkg_id);
    if tokio::fs::metadata(&src).await.is_err() {
        return Ok(false);
    }
    let dst = install_backup_path(pkg_id);
    // Remove any stale backup from a previous failed attempt
    crate::util::io::delete_dir(&dst).await?;
    match Command::new("cp")
        .arg("-a")
        .arg("--reflink=always")
        .arg(&src)
        .arg(&dst)
        .invoke(ErrorKind::Filesystem)
        .await
    {
        Ok(_) => {
            tracing::info!("Created install backup for {pkg_id} at {dst:?}");
            Ok(true)
        }
        Err(e) => {
            tracing::warn!(
                "Could not create install backup for {pkg_id} \
                 (filesystem may not support reflinks): {e}"
            );
            // Clean up partial copy if any
            crate::util::io::delete_dir(&dst).await?;
            Ok(false)
        }
    }
}

/// Restores the package volume directory from a COW snapshot after a failed
/// install. The current (possibly corrupted) volume dir is deleted first.
/// No-op if no backup exists.
pub async fn restore_volumes_from_install_backup(pkg_id: &PackageId) -> Result<(), Error> {
    let backup = install_backup_path(pkg_id);
    if tokio::fs::metadata(&backup).await.is_err() {
        return Ok(());
    }
    let dst = pkg_volume_dir(pkg_id);
    crate::util::io::delete_dir(&dst).await?;
    crate::util::io::rename(&backup, &dst).await?;
    tracing::info!("Restored volumes from install backup for {pkg_id}");
    Ok(())
}

/// Removes the install backup after a successful install.
pub async fn remove_install_backup(pkg_id: &PackageId) -> Result<(), Error> {
    crate::util::io::delete_dir(&install_backup_path(pkg_id)).await
}
