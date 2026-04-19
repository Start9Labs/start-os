use std::fs;
use std::path::Path;

use crate::invoke::Invoke;
use crate::prelude::*;

pub const PERSISTENT_MOUNT: &str = "/key_backup";
pub const WIFI_PASSWORD_PATH: &str = "/key_backup/wifi_password";

/// Check if /key_backup is already mounted by scanning /proc/mounts.
pub fn is_persistent_mounted() -> bool {
    fs::read_to_string("/proc/mounts")
        .map(|contents| {
            contents
                .lines()
                .any(|line| line.split_whitespace().nth(1) == Some(PERSISTENT_MOUNT))
        })
        .unwrap_or(false)
}

/// Find the device node for the GPT partition named "key_backup" on eMMC.
///
/// Scans `/sys/block/mmcblk*` partition uevent files for `PARTNAME=key_backup`,
/// equivalent to OpenWrt's `find_mmc_part` shell function.
fn find_persistent_device() -> Result<String, Error> {
    let sys_block = Path::new("/sys/block");
    for block_entry in fs::read_dir(sys_block)
        .map_err(|e| Error::new(eyre!("failed to read /sys/block: {e}"), ErrorKind::Filesystem))?
    {
        let block_entry =
            block_entry.map_err(|e| Error::new(eyre!("readdir /sys/block: {e}"), ErrorKind::Filesystem))?;
        let block_name = block_entry.file_name();
        let block_name = block_name.to_string_lossy();
        if !block_name.starts_with("mmcblk") {
            continue;
        }
        for part_entry in fs::read_dir(block_entry.path())
            .map_err(|e| Error::new(eyre!("readdir {}: {e}", block_entry.path().display()), ErrorKind::Filesystem))?
        {
            let part_entry = part_entry.map_err(|e| Error::new(eyre!("readdir: {e}"), ErrorKind::Filesystem))?;
            let part_name = part_entry.file_name();
            let part_name_str = part_name.to_string_lossy();
            if !part_name_str.starts_with(&*block_name) {
                continue;
            }
            let uevent_path = part_entry.path().join("uevent");
            if let Ok(contents) = fs::read_to_string(&uevent_path) {
                if contents.lines().any(|l| l == "PARTNAME=key_backup") {
                    return Ok(format!("/dev/{part_name_str}"));
                }
            }
        }
    }
    Err(Error::new(
        eyre!("no partition named 'key_backup' found on any eMMC device"),
        ErrorKind::NotFound,
    ))
}

/// Mount /key_backup if not already mounted.
///
/// Discovers the key_backup partition device dynamically from sysfs rather than
/// relying on `/etc/fstab` (OpenWrt uses UCI fstab which BusyBox mount doesn't read).
pub async fn ensure_persistent_mounted() -> Result<(), Error> {
    if is_persistent_mounted() {
        return Ok(());
    }
    let dev = find_persistent_device()?;
    tokio::process::Command::new("mount")
        .args(["-t", "ext4", &dev, PERSISTENT_MOUNT])
        .invoke(ErrorKind::Filesystem.into())
        .await?;
    Ok(())
}

/// Check whether manufacturing init has already been performed.
pub fn is_initialized() -> bool {
    Path::new(WIFI_PASSWORD_PATH).exists()
}

/// Read the plaintext WiFi password from the key_backup partition.
pub fn read_password() -> Result<String, Error> {
    let password = fs::read_to_string(WIFI_PASSWORD_PATH)
        .map_err(|e| Error::new(eyre!("failed to read {WIFI_PASSWORD_PATH}: {e}"), ErrorKind::Filesystem))?;
    Ok(password.trim().to_string())
}

/// Write a plaintext WiFi password to the key_backup partition atomically.
pub async fn write_password(password: &str) -> Result<(), Error> {
    use std::os::unix::fs::PermissionsExt;
    use tokio::io::AsyncWriteExt;

    let path = Path::new(WIFI_PASSWORD_PATH);
    let mut file = startos::util::io::AtomicFile::new(path, None::<&Path>)
        .await
        .map_err(Error::from)?;
    file.set_permissions(std::fs::Permissions::from_mode(0o600))
        .await
        .map_err(|e| Error::new(eyre!("failed to set password file permissions: {e}"), ErrorKind::Filesystem))?;
    file.write_all(password.as_bytes())
        .await
        .map_err(|e| Error::new(eyre!("failed to write password: {e}"), ErrorKind::Filesystem))?;
    file.save().await.map_err(Error::from)?;

    Ok(())
}
