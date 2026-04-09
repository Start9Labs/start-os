use std::fs::{self, OpenOptions};
use std::io::Write;
use std::os::unix::fs::OpenOptionsExt;
use std::path::Path;

use crate::Error;

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
        .map_err(|e| Error::other(format!("failed to read /sys/block: {e}")))?
    {
        let block_entry =
            block_entry.map_err(|e| Error::other(format!("readdir /sys/block: {e}")))?;
        let block_name = block_entry.file_name();
        let block_name = block_name.to_string_lossy();
        if !block_name.starts_with("mmcblk") {
            continue;
        }
        for part_entry in fs::read_dir(block_entry.path())
            .map_err(|e| Error::other(format!("readdir {}: {e}", block_entry.path().display())))?
        {
            let part_entry = part_entry.map_err(|e| Error::other(format!("readdir: {e}")))?;
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
    Err(Error::other(
        "no partition named 'key_backup' found on any eMMC device",
    ))
}

/// Mount /key_backup if not already mounted.
///
/// Discovers the key_backup partition device dynamically from sysfs rather than
/// relying on `/etc/fstab` (OpenWrt uses UCI fstab which BusyBox mount doesn't read).
pub fn ensure_persistent_mounted() -> Result<(), Error> {
    if is_persistent_mounted() {
        return Ok(());
    }
    let dev = find_persistent_device()?;
    let status = std::process::Command::new("mount")
        .args(["-t", "ext4", &dev, PERSISTENT_MOUNT])
        .status()
        .map_err(|e| Error::other(format!("failed to run mount: {e}")))?;
    if !status.success() {
        return Err(Error::other(format!(
            "mount {dev} {PERSISTENT_MOUNT} failed (exit {})",
            status.code().unwrap_or(-1)
        )));
    }
    Ok(())
}

/// Check whether manufacturing init has already been performed.
pub fn is_initialized() -> bool {
    Path::new(WIFI_PASSWORD_PATH).exists()
}

/// Read the plaintext WiFi password from the key_backup partition.
pub fn read_password() -> Result<String, Error> {
    let password = fs::read_to_string(WIFI_PASSWORD_PATH)
        .map_err(|e| Error::other(format!("failed to read {WIFI_PASSWORD_PATH}: {e}")))?;
    Ok(password.trim().to_string())
}

/// Write a plaintext WiFi password to the key_backup partition atomically.
///
/// Uses the temp-file → mode 0o600 → write → flush → sync → rename pattern
/// from auth.rs to avoid partial writes.
pub fn write_password(password: &str) -> Result<(), Error> {
    let path = Path::new(WIFI_PASSWORD_PATH);
    let tmp_path = path
        .parent()
        .unwrap_or(Path::new("/key_backup"))
        .join(".wifi_password.tmp");

    let mut file = OpenOptions::new()
        .write(true)
        .create(true)
        .truncate(true)
        .mode(0o600)
        .open(&tmp_path)
        .map_err(|e| Error::other(format!("failed to create temp password file: {e}")))?;

    file.write_all(password.as_bytes())
        .map_err(|e| Error::other(format!("failed to write password: {e}")))?;

    file.flush()
        .map_err(|e| Error::other(format!("failed to flush password file: {e}")))?;

    file.sync_all()
        .map_err(|e| Error::other(format!("failed to sync password file: {e}")))?;

    fs::rename(&tmp_path, path)
        .map_err(|e| Error::other(format!("failed to persist password file: {e}")))?;

    // fsync the directory to ensure the rename's directory entry is durable.
    // Without this, the rename is only in the journal's page cache and may be
    // lost on a crash/reboot before the next journal commit (up to 5 seconds).
    let dir = std::fs::File::open(path.parent().unwrap_or(Path::new(PERSISTENT_MOUNT)))
        .map_err(|e| Error::other(format!("failed to open directory for fsync: {e}")))?;
    dir.sync_all()
        .map_err(|e| Error::other(format!("failed to fsync directory: {e}")))?;

    Ok(())
}
