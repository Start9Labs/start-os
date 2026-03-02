use std::fs::{self, File, OpenOptions};
use std::io::{self, BufRead, Read, Seek, SeekFrom, Write};
use std::path::Path;
use std::process::Command;

use serde::{Deserialize, Serialize};

use crate::{emmc, init, Error};

pub(crate) const SECTOR_SIZE: u64 = 512;
const LINUX_FS_GUID: &str = "0FC63DAF-8483-4772-8E79-3D69D8477DE4";

/// SquashFS magic number ("hsqs" in little-endian).
pub(crate) const SQUASHFS_MAGIC: u32 = 0x73717368;

/// Read `bytes_used` from the squashfs superblock at a given byte offset on a device.
///
/// Returns `None` if the device can't be read or the magic doesn't match.
pub(crate) fn squashfs_bytes_used(dev_path: &str, offset: u64) -> Option<u64> {
    let mut f = File::open(dev_path).ok()?;
    f.seek(SeekFrom::Start(offset)).ok()?;
    let mut sb = [0u8; 48];
    f.read_exact(&mut sb).ok()?;
    let magic = u32::from_le_bytes(sb[0..4].try_into().unwrap());
    if magic != SQUASHFS_MAGIC {
        return None;
    }
    Some(u64::from_le_bytes(sb[40..48].try_into().unwrap()))
}

/// Partition entry from sfdisk --json output.
#[derive(Debug, Deserialize)]
pub(crate) struct SfdiskPartition {
    pub node: String,
    pub start: u64,
    pub size: u64,
    #[serde(default)]
    pub name: Option<String>,
}

#[derive(Debug, Deserialize)]
pub(crate) struct SfdiskTable {
    pub partitions: Vec<SfdiskPartition>,
}

#[derive(Debug, Deserialize)]
pub(crate) struct SfdiskOutput {
    #[serde(rename = "partitiontable")]
    pub partition_table: SfdiskTable,
}

/// Size of the key_backup partition in 512-byte sectors (64 MB).
const PERSISTENT_SECTORS: u64 = 131072;

/// Progress/phase events emitted during flash operations.
#[derive(Debug, Clone, Serialize)]
#[serde(tag = "phase", rename_all = "camelCase")]
pub enum FlashEvent {
    /// Raw byte copy in progress.
    Copying { copied: u64, total: u64 },
    /// A status message (partition manipulation, formatting, etc.).
    Status { message: String },
}

/// Information about the flashed eMMC, returned after a successful flash.
pub struct FlashResult {
    /// eMMC block device name (e.g. "mmcblk2").
    pub emmc_dev: String,
    /// eMMC rootfs partition device path (e.g. "/dev/mmcblk2p7") — SquashFS, read-only.
    pub rootfs_dev: String,
    /// eMMC rootfs_data partition device path (e.g. "/dev/mmcblk2p8") — ext4 overlay.
    pub rootfs_data_dev: String,
    /// eMMC key_backup partition device path (e.g. "/dev/mmcblk2p9").
    pub persistent_dev: String,
}

/// Extract the GPT partition number from a device node path.
///
/// `/dev/mmcblk2p6` → `6`, `/dev/sda1` → `1`.
pub(crate) fn node_partition_number(node: &str) -> Result<u64, Error> {
    let base = node.rsplit('/').next().unwrap_or(node);
    if base.starts_with("mmcblk") {
        if let Some(i) = base.rfind('p') {
            if let Ok(n) = base[i + 1..].parse::<u64>() {
                return Ok(n);
            }
        }
    }
    // sd-style: trailing digits
    let start = base
        .rfind(|c: char| !c.is_ascii_digit())
        .map(|i| i + 1)
        .unwrap_or(0);
    if start < base.len() {
        if let Ok(n) = base[start..].parse::<u64>() {
            return Ok(n);
        }
    }
    Err(Error::other(format!(
        "can't parse partition number from {node}"
    )))
}

/// Run sfdisk with the given arguments, piping `script` to stdin.
pub(crate) fn run_sfdisk(args: &[&str], script: &str) -> Result<(), Error> {
    let mut child = Command::new("sfdisk")
        .args(args)
        .stdin(std::process::Stdio::piped())
        .spawn()
        .map_err(|e| Error::other(format!("failed to spawn sfdisk: {e}")))?;

    {
        let stdin = child.stdin.as_mut().ok_or_else(|| {
            Error::other("failed to open sfdisk stdin pipe")
        })?;
        stdin
            .write_all(script.as_bytes())
            .map_err(|e| Error::other(format!("failed to write sfdisk script: {e}")))?;
    }

    let status = child
        .wait()
        .map_err(|e| Error::other(format!("sfdisk wait failed: {e}")))?;
    if !status.success() {
        return Err(Error::other(format!(
            "sfdisk {} failed (exit {})",
            args.join(" "),
            status.code().unwrap_or(-1)
        )));
    }
    Ok(())
}

/// Strip partition suffix to get the base block device name.
///
/// - `mmcblk0p2` → `mmcblk0`  (mmcblk: partition separated by `pN` after device number)
/// - `sda1`      → `sda`      (sd-style: trailing digits after letters are partition)
/// - `mmcblk0`   → `mmcblk0`  (no partition suffix)
pub(crate) fn strip_partition(dev: &str) -> &str {
    // mmcblk-style: find last occurrence of 'p' preceded by a digit
    if dev.starts_with("mmcblk") {
        if let Some(i) = dev.rfind('p') {
            if i > 0
                && dev.as_bytes()[i - 1].is_ascii_digit()
                && !dev[i + 1..].is_empty()
                && dev[i + 1..].bytes().all(|b| b.is_ascii_digit())
            {
                return &dev[..i];
            }
        }
        return dev;
    }
    // sd-style: strip trailing digits (sda1 -> sda)
    let end = dev.trim_end_matches(|c: char| c.is_ascii_digit()).len();
    if end < dev.len() {
        return &dev[..end];
    }
    dev
}

/// Read MMC device type from sysfs.
///
/// Returns `Some("MMC")` for eMMC, `Some("SD")` for SD cards, or `None` if
/// the sysfs type file doesn't exist (e.g. USB mass-storage).
pub(crate) fn mmc_device_type(dev: &str) -> Option<String> {
    let path = format!("/sys/block/{dev}/device/type");
    fs::read_to_string(&path).ok().map(|s| s.trim().to_string())
}

/// Parse /proc/cmdline to find the root device.
pub(crate) fn boot_device() -> Result<String, Error> {
    let cmdline = fs::read_to_string("/proc/cmdline")
        .map_err(|e| Error::other(format!("failed to read /proc/cmdline: {e}")))?;

    for token in cmdline.split_whitespace() {
        if let Some(root) = token.strip_prefix("root=") {
            if let Some(dev) = root.strip_prefix("/dev/") {
                return Ok(strip_partition(dev).to_string());
            }
        }
    }

    Err(Error::other(
        "could not determine boot device from /proc/cmdline",
    ))
}

/// Find the eMMC block device by enumerating /sys/block/mmcblk* and checking
/// the sysfs device type. Only devices reporting "MMC" are considered eMMC.
/// The boot device, boot partitions (mmcblkNbootM), and RPMB are excluded.
pub(crate) fn find_emmc(boot_dev: &str) -> Result<String, Error> {
    let mut candidates = Vec::new();

    for entry in fs::read_dir("/sys/block")
        .map_err(|e| Error::other(format!("failed to read /sys/block: {e}")))?
    {
        let entry = entry.map_err(|e| Error::other(format!("readdir error: {e}")))?;
        let name = entry.file_name().to_string_lossy().to_string();

        // Only consider mmcblk* devices, skip boot partitions and RPMB
        if !name.starts_with("mmcblk") || name.contains("boot") || name.contains("rpmb") {
            continue;
        }

        // Skip the boot device
        if name == boot_dev {
            continue;
        }

        // Only accept devices whose sysfs type is "MMC" (eMMC).
        // SD cards report "SD" and are excluded.
        if mmc_device_type(&name).as_deref() != Some("MMC") {
            continue;
        }

        candidates.push(name);
    }

    match candidates.len() {
        0 => Err(Error::other("no eMMC device found")),
        1 => Ok(candidates.into_iter().next().unwrap()),
        _ => Err(Error::other(format!(
            "multiple eMMC candidates found: {}",
            candidates.join(", ")
        ))),
    }
}

/// Read block device size in sectors from sysfs.
pub(crate) fn device_size_sectors(dev: &str) -> Result<u64, Error> {
    let path = format!("/sys/block/{dev}/size");
    let content = fs::read_to_string(&path)
        .map_err(|e| Error::other(format!("failed to read {path}: {e}")))?;
    content
        .trim()
        .parse::<u64>()
        .map_err(|e| Error::other(format!("failed to parse device size: {e}")))
}

/// Run sfdisk --json on a device and parse the partition table.
pub(crate) fn read_partition_table(dev_path: &str) -> Result<SfdiskOutput, Error> {
    let output = Command::new("sfdisk")
        .args(["--json", dev_path])
        .output()
        .map_err(|e| Error::other(format!("failed to run sfdisk: {e}")))?;

    if !output.status.success() {
        return Err(Error::other(format!(
            "sfdisk --json failed: {}",
            String::from_utf8_lossy(&output.stderr)
        )));
    }

    serde_json::from_slice(&output.stdout)
        .map_err(|e| Error::other(format!("failed to parse sfdisk JSON: {e}")))
}

/// Find the end of actual firmware data and return its byte offset.
///
/// Reads the squashfs superblock's `bytes_used` field to determine exactly
/// where the real data ends inside the rootfs partition, avoiding copying
/// empty space. Falls back to the partition boundary if the superblock
/// can't be read or the value is nonsensical.
pub(crate) fn find_copy_end(dev_path: &str, partitions: &[SfdiskPartition]) -> Result<u64, Error> {
    let rootfs = partitions
        .iter()
        .find(|p| p.name.as_deref() == Some("rootfs"))
        .ok_or_else(|| {
            Error::other("no partition named 'rootfs' found on source device")
        })?;
    let rootfs_start = rootfs.start * SECTOR_SIZE;
    let rootfs_end = (rootfs.start + rootfs.size) * SECTOR_SIZE;

    // Try reading squashfs bytes_used; fall back to partition boundary
    if let Some(bytes_used) = squashfs_bytes_used(dev_path, rootfs_start) {
        let data_end = rootfs_start + bytes_used;
        if bytes_used > 0 && data_end <= rootfs_end {
            return Ok(data_end);
        }
    }

    Ok(rootfs_end)
}

/// Find the "key_backup" partition and return its 1-based index.
pub(crate) fn find_persistent_index(partitions: &[SfdiskPartition]) -> Option<usize> {
    partitions.iter().position(|p| {
        p.name.as_deref() == Some("key_backup")
    }).map(|i| i + 1)
}

/// Prompt user for Y/n confirmation. Enter or "y"/"yes" → true, "n"/"no" → false.
fn confirm(prompt: &str) -> Result<bool, Error> {
    loop {
        print!("{prompt} [Y/n] ");
        io::stdout()
            .flush()
            .map_err(|e| Error::other(format!("flush failed: {e}")))?;

        let mut input = String::new();
        io::stdin()
            .lock()
            .read_line(&mut input)
            .map_err(|e| Error::other(format!("failed to read input: {e}")))?;

        let trimmed = input.trim();
        if trimmed.is_empty()
            || trimmed.eq_ignore_ascii_case("y")
            || trimmed.eq_ignore_ascii_case("yes")
        {
            return Ok(true);
        }
        if trimmed.eq_ignore_ascii_case("n") || trimmed.eq_ignore_ascii_case("no") {
            return Ok(false);
        }
        println!("Please answer y or n.");
    }
}

/// Run a command, returning an error with context on failure.
pub(crate) fn run_cmd(cmd: &str, args: &[&str]) -> Result<(), Error> {
    let status = Command::new(cmd)
        .args(args)
        .status()
        .map_err(|e| Error::other(format!("failed to run {cmd}: {e}")))?;

    if !status.success() {
        return Err(Error::other(format!(
            "{cmd} {} failed (exit {})",
            args.join(" "),
            status.code().unwrap_or(-1)
        )));
    }
    Ok(())
}

/// Copy `total_bytes` from one block device to another with progress reporting.
///
/// If `on_progress` is `Some`, progress events are sent through the callback
/// instead of printing to stdout.
fn copy_raw(
    src: &str,
    dst: &str,
    total_bytes: u64,
    on_progress: Option<&dyn Fn(FlashEvent)>,
) -> Result<(), Error> {
    let mut src_file = File::open(src)
        .map_err(|e| Error::other(format!("failed to open {src}: {e}")))?;
    let mut dst_file = OpenOptions::new()
        .write(true)
        .open(dst)
        .map_err(|e| Error::other(format!("failed to open {dst}: {e}")))?;

    let buf_size = 1024 * 1024; // 1 MB
    let mut buf = vec![0u8; buf_size];
    let mut copied: u64 = 0;
    let mut last_report: u64 = 0;
    let total_mb = total_bytes / 1_000_000;

    while copied < total_bytes {
        let to_read = std::cmp::min(buf_size as u64, total_bytes - copied) as usize;
        let n = src_file
            .read(&mut buf[..to_read])
            .map_err(|e| Error::other(format!("read error at offset {copied}: {e}")))?;
        if n == 0 {
            return Err(Error::other(format!(
                "unexpected EOF at offset {copied} (expected {total_bytes} bytes)"
            )));
        }
        dst_file
            .write_all(&buf[..n])
            .map_err(|e| Error::other(format!("write error at offset {copied}: {e}")))?;
        copied += n as u64;

        if copied - last_report >= 10 * 1024 * 1024 || copied >= total_bytes {
            if let Some(cb) = on_progress {
                cb(FlashEvent::Copying { copied, total: total_bytes });
            } else {
                let pct = (copied as f64 / total_bytes as f64) * 100.0;
                let mb = copied / 1_000_000;
                print!("\r  {mb} / {total_mb} MB ({pct:.0}%)");
                io::stdout().flush().ok();
            }
            last_report = copied;
        }
    }

    if on_progress.is_none() {
        println!();
    }
    dst_file
        .sync_all()
        .map_err(|e| Error::other(format!("sync failed: {e}")))?;

    Ok(())
}

/// Core flash logic shared by interactive and unattended paths.
///
/// Copies firmware from microSD to eMMC, recreates the key_backup partition.
/// Returns device info needed for post-flash configuration.
fn run_flash_core(
    interactive: bool,
    on_progress: Option<&dyn Fn(FlashEvent)>,
) -> Result<Option<FlashResult>, Error> {
    let report = |msg: &str| {
        if let Some(cb) = on_progress {
            cb(FlashEvent::Status { message: msg.to_string() });
        }
        if interactive {
            println!("{msg}");
        }
    };

    // 1. Detect boot device — refuse to run if booted from eMMC.
    let boot_dev = boot_device()?;
    if mmc_device_type(&boot_dev).as_deref() == Some("MMC") {
        return Err(Error::other(
            "flash must be run from removable media (microSD or USB), not eMMC",
        ));
    }

    // 2. Find eMMC
    let emmc_dev = find_emmc(&boot_dev)?;
    let emmc_path = format!("/dev/{emmc_dev}");
    let sd_path = format!("/dev/{boot_dev}");

    // 3. Read source partition table
    let sfdisk = read_partition_table(&sd_path)?;
    let partitions = &sfdisk.partition_table.partitions;

    // 4. Find copy end offset (through end of squashfs data)
    let copy_end_bytes = find_copy_end(&sd_path, partitions)?;

    // 5. Read device sizes
    let emmc_sectors = device_size_sectors(&emmc_dev)?;
    let emmc_bytes = emmc_sectors * SECTOR_SIZE;
    let sd_sectors = device_size_sectors(&boot_dev)?;

    if copy_end_bytes > emmc_bytes {
        return Err(Error::other(format!(
            "source image ({} bytes) exceeds eMMC capacity ({emmc_bytes} bytes)",
            copy_end_bytes
        )));
    }

    // 6. Display summary and confirm (interactive only)
    if interactive {
        println!();
        println!("========================================");
        println!("   StartWRT eMMC Flash");
        println!("========================================");
        println!();
        println!("Source:  {sd_path} ({} MB)", sd_sectors * SECTOR_SIZE / 1_000_000);
        println!("Target:  {emmc_path} ({} MB)", emmc_bytes / 1_000_000);
        println!("Copy:    {} MB (GPT through squashfs data)", copy_end_bytes / 1_000_000);
        println!();
        println!("Partitions on source:");
        for p in partitions {
            let name = p.name.as_deref().unwrap_or("<unnamed>");
            println!(
                "  {}: {} ({} MB)",
                name,
                p.node,
                p.size * SECTOR_SIZE / 1_000_000
            );
        }
        println!();
        println!("WARNING: This will ERASE all data on {emmc_path}!");
        println!();

        if !confirm("Proceed?")? {
            println!("Aborted.");
            return Ok(None);
        }
    }

    // 7. Unmount any eMMC partitions
    let mounts = fs::read_to_string("/proc/mounts")
        .map_err(|e| Error::other(format!("failed to read /proc/mounts: {e}")))?;
    for line in mounts.lines() {
        let mut fields = line.split_whitespace();
        if let (Some(dev), Some(mount_point)) = (fields.next(), fields.next()) {
            if dev.starts_with(&emmc_path) {
                report(&format!("Unmounting {mount_point} (on eMMC)..."));
                run_cmd("umount", &[mount_point])?;
            }
        }
    }

    // 8. Copy raw bytes: byte 0 through end of rootfs
    report("Copying firmware to eMMC...");
    copy_raw(&sd_path, &emmc_path, copy_end_bytes, on_progress)?;

    // 9. Remove the copied key_backup partition (if present).
    if let Some(persistent_idx) = find_persistent_index(partitions) {
        report("Removing copied key_backup partition...");
        let part_num = persistent_idx.to_string();
        run_cmd("sfdisk", &["--no-reread", "--force", "--delete", &emmc_path, &part_num])?;
    }

    // 10. Read eMMC partition table and find rootfs_data
    let emmc_sfdisk = read_partition_table(&emmc_path)?;
    let emmc_parts = &emmc_sfdisk.partition_table.partitions;

    let rootfs_part = emmc_parts
        .iter()
        .find(|p| p.name.as_deref() == Some("rootfs"))
        .ok_or_else(|| Error::other("rootfs partition not found on eMMC"))?;
    let rootfs_part_num = node_partition_number(&rootfs_part.node)?;
    let rootfs_dev = format!("{emmc_path}p{rootfs_part_num}");

    let rootfs_data_part = emmc_parts
        .iter()
        .find(|p| p.name.as_deref() == Some("rootfs_data"))
        .ok_or_else(|| Error::other("rootfs_data partition not found on eMMC"))?;
    let rootfs_data_start = rootfs_data_part.start;
    let rootfs_data_part_num = node_partition_number(&rootfs_data_part.node)?;

    // 11. Calculate new rootfs_data size: fill all space except 64 MB for key_backup
    let last_usable = emmc_sectors - 34;
    let persistent_start = ((last_usable - PERSISTENT_SECTORS) / 2048) * 2048;
    let new_rootfs_data_size = persistent_start - rootfs_data_start;

    // 12. Expand rootfs_data partition
    report(&format!(
        "Expanding rootfs_data partition to {new_rootfs_data_size} sectors..."
    ));
    let part_num_str = rootfs_data_part_num.to_string();
    run_sfdisk(
        &["--no-reread", "--force", "-N", &part_num_str, &emmc_path],
        &format!("size={new_rootfs_data_size}\n"),
    )?;

    // 13. Append key_backup partition (fills the remaining ~64 MB gap)
    report("Creating key_backup partition (64 MB)...");
    run_sfdisk(
        &["--no-reread", "--force", "--append", &emmc_path],
        &format!("type={LINUX_FS_GUID}, name=\"key_backup\"\n"),
    )?;

    // 14. Refresh kernel partition table.
    //     partx -d + -a is the cleanest (wipe stale entries, re-read GPT),
    //     but -a fails if entries already exist and -d fails if partitions
    //     are busy.  Fall back to -u (updates existing entries) which is
    //     enough because the on-disk GPT is already correct at this point.
    report("Refreshing partition table...");
    let _ = Command::new("partx")
        .args(["-d", &emmc_path])
        .status();
    if Command::new("partx")
        .args(["-a", &emmc_path])
        .status()
        .map_or(true, |s| !s.success())
    {
        run_cmd("partx", &["-u", &emmc_path])?;
    }

    // 15. Format rootfs_data as ext4 (clean overlay)
    let rootfs_data_dev = format!("{emmc_path}p{rootfs_data_part_num}");
    report(&format!(
        "Formatting rootfs_data overlay ({rootfs_data_dev})..."
    ));
    run_cmd("mkfs.ext4", &["-L", "rootfs_data", "-F", &rootfs_data_dev])?;

    // 16. Find and format the new key_backup partition.
    let new_sfdisk = read_partition_table(&emmc_path)?;
    let new_persistent = new_sfdisk
        .partition_table
        .partitions
        .iter()
        .find(|p| p.name.as_deref() == Some("key_backup"))
        .ok_or_else(|| Error::other("key_backup partition not found after creation"))?;
    let persistent_dev = &new_persistent.node;

    // Unmount if hotplug auto-mounted it
    if emmc::is_persistent_mounted() {
        report("Unmounting auto-mounted key_backup partition...");
        run_cmd("umount", &[emmc::PERSISTENT_MOUNT])?;
    }

    report(&format!("Formatting key_backup partition ({persistent_dev})..."));
    run_cmd("mkfs.ext4", &["-L", "key_backup", "-F", persistent_dev])?;

    // 17. Mount key_backup
    let mount_point = Path::new(emmc::PERSISTENT_MOUNT);
    if !mount_point.exists() {
        fs::create_dir_all(mount_point)
            .map_err(|e| Error::other(format!("failed to create {}: {e}", emmc::PERSISTENT_MOUNT)))?;
    }
    run_cmd("mount", &[persistent_dev, emmc::PERSISTENT_MOUNT])?;

    // 18. Success
    report("Flash complete.");

    Ok(Some(FlashResult {
        emmc_dev: emmc_dev.clone(),
        rootfs_dev,
        rootfs_data_dev,
        persistent_dev: persistent_dev.to_string(),
    }))
}

/// Interactive manufacturing flash entry point.
///
/// Copies firmware from microSD to eMMC, then recreates the key_backup
/// partition to fill remaining eMMC space. Prompts for confirmation
/// and prints progress to stdout.
///
/// Returns `Ok(true)` if the flash completed successfully, `Ok(false)` if the
/// operator aborted at the confirmation prompt.
pub fn run_flash() -> Result<bool, Error> {
    match run_flash_core(true, None)? {
        Some(_) => {
            println!();
            println!("========================================");
            println!("   Flash complete!");
            println!("========================================");
            println!();
            println!("The key_backup partition has been created and mounted.");
            Ok(true)
        }
        None => Ok(false),
    }
}

/// Non-interactive flash for the setup wizard.
///
/// Same as `run_flash` but skips confirmation and reports progress through
/// the callback. Returns device info needed for post-flash configuration.
pub fn run_flash_unattended(
    on_progress: &dyn Fn(FlashEvent),
) -> Result<FlashResult, Error> {
    run_flash_core(false, Some(on_progress))?
        .ok_or_else(|| Error::other("flash aborted unexpectedly in unattended mode"))
}

/// Combined manufacturing flow: password → flash → persist PMK.
///
/// Intended for use when booted from removable media. The operator enters
/// the sticker password (pure computation, no disk needed), the eMMC is
/// flashed (creating the key_backup partition), and the PMK is written to
/// key_backup. On the subsequent eMMC boot, `restore_wifi_if_needed()`
/// recovers WiFi and the captive portal activates.
pub fn run_manufacture() -> Result<(), Error> {
    // 1. Banner
    println!();
    println!("========================================");
    println!("   StartWRT Manufacturing Setup");
    println!("========================================");
    println!();

    // 2. Prompt for password and derive PMK (no disk access)
    let pmk_hex = init::prompt_and_derive_pmk()?;

    // 3. Flash eMMC (creates key_backup partition and mounts it)
    if !run_flash()? {
        return Ok(()); // operator aborted
    }

    // 4. Write PMK to key_backup (now mounted from step 3)
    println!("Writing WiFi credentials to key_backup partition...");
    emmc::write_pmk(&pmk_hex)?;

    // 5. Success
    println!();
    println!("========================================");
    println!("   Manufacturing complete!");
    println!("========================================");
    println!();
    println!("Remove the microSD card, then power off.");
    println!("WiFi will activate automatically on the next boot.");

    // Wait for operator acknowledgement, then drop into a login shell.
    println!();
    print!("Press Enter for console login...");
    io::stdout().flush().ok();
    let _ = io::stdin().lock().read_line(&mut String::new());

    Ok(())
}

#[cfg(test)]
mod tests {
    use super::*;
    use std::path::Path;

    /// Write a minimal squashfs superblock at the given sector offset in a sparse file.
    fn make_fake_device(path: &Path, rootfs_start_sectors: u64, bytes_used: u64) {
        let mut f = File::create(path).unwrap();
        let offset = rootfs_start_sectors * 512;
        f.seek(SeekFrom::Start(offset)).unwrap();
        let mut sb = [0u8; 48];
        sb[0..4].copy_from_slice(&SQUASHFS_MAGIC.to_le_bytes());
        sb[40..48].copy_from_slice(&bytes_used.to_le_bytes());
        f.write_all(&sb).unwrap();
    }

    /// Test that sfdisk JSON output parses correctly (new layout with rootfs_data).
    #[test]
    fn parse_sfdisk_json() {
        let json = r#"{
            "partitiontable": {
                "label": "gpt",
                "id": "12345678-1234-1234-1234-123456789ABC",
                "device": "/dev/mmcblk1",
                "unit": "sectors",
                "firstlba": 34,
                "lastlba": 61071326,
                "sectorsize": 512,
                "partitions": [
                    {"node": "/dev/mmcblk1p1", "start": 256, "size": 512, "name": "bootinfo"},
                    {"node": "/dev/mmcblk1p2", "start": 768, "size": 256, "name": "fsbl"},
                    {"node": "/dev/mmcblk1p3", "start": 2048, "size": 4096, "name": "env"},
                    {"node": "/dev/mmcblk1p4", "start": 6144, "size": 2048, "name": "opensbi"},
                    {"node": "/dev/mmcblk1p5", "start": 8192, "size": 12288, "name": "uboot"},
                    {"node": "/dev/mmcblk1p6", "start": 20480, "size": 262144, "name": "bootfs"},
                    {"node": "/dev/mmcblk1p7", "start": 282624, "size": 524288, "name": "rootfs"},
                    {"node": "/dev/mmcblk1p8", "start": 806912, "size": 524288, "name": "rootfs_data"},
                    {"node": "/dev/mmcblk1p9", "start": 1331200, "size": 65536, "name": "key_backup"}
                ]
            }
        }"#;

        let parsed: SfdiskOutput = serde_json::from_str(json).expect("parse failed");
        assert_eq!(parsed.partition_table.partitions.len(), 9);

        let rootfs = &parsed.partition_table.partitions[6];
        assert_eq!(rootfs.name.as_deref(), Some("rootfs"));
        assert_eq!(rootfs.start, 282624);
        assert_eq!(rootfs.size, 524288);

        let rootfs_data = &parsed.partition_table.partitions[7];
        assert_eq!(rootfs_data.name.as_deref(), Some("rootfs_data"));

        // Create sparse temp file with squashfs superblock at rootfs offset
        let dir = tempfile::tempdir().unwrap();
        let dev = dir.path().join("fake_dev");
        let bytes_used: u64 = 150_000_000;
        make_fake_device(&dev, 282624, bytes_used);

        // find_copy_end should return rootfs_start + squashfs bytes_used
        let end = find_copy_end(dev.to_str().unwrap(), &parsed.partition_table.partitions).unwrap();
        assert_eq!(end, 282624 * 512 + bytes_used);

        let persistent_idx = find_persistent_index(&parsed.partition_table.partitions);
        assert_eq!(persistent_idx, Some(9));
    }

    /// Test find_copy_end with squashfs data when rootfs_data is absent.
    #[test]
    fn find_copy_end_rootfs_only() {
        let json = r#"{
            "partitiontable": {
                "partitions": [
                    {"node": "/dev/mmcblk1p1", "start": 256, "size": 512, "name": "bootfs"},
                    {"node": "/dev/mmcblk1p2", "start": 1024, "size": 2048, "name": "rootfs"}
                ]
            }
        }"#;
        let parsed: SfdiskOutput = serde_json::from_str(json).expect("parse failed");

        let dir = tempfile::tempdir().unwrap();
        let dev = dir.path().join("fake_dev");
        let bytes_used: u64 = 500_000;
        make_fake_device(&dev, 1024, bytes_used);

        let end = find_copy_end(dev.to_str().unwrap(), &parsed.partition_table.partitions).unwrap();
        assert_eq!(end, 1024 * 512 + bytes_used);
    }

    /// Test find_copy_end falls back to partition boundary on bad magic.
    #[test]
    fn find_copy_end_fallback_on_bad_magic() {
        let json = r#"{
            "partitiontable": {
                "partitions": [
                    {"node": "/dev/mmcblk1p1", "start": 256, "size": 512, "name": "bootfs"},
                    {"node": "/dev/mmcblk1p2", "start": 1024, "size": 2048, "name": "rootfs"}
                ]
            }
        }"#;
        let parsed: SfdiskOutput = serde_json::from_str(json).expect("parse failed");

        // Create temp file with zeros (no squashfs magic) at rootfs offset
        let dir = tempfile::tempdir().unwrap();
        let dev = dir.path().join("fake_dev");
        {
            let mut f = File::create(&dev).unwrap();
            let offset = 1024u64 * 512;
            f.seek(SeekFrom::Start(offset)).unwrap();
            f.write_all(&[0u8; 48]).unwrap();
        }

        // Should fall back to partition boundary
        let end = find_copy_end(dev.to_str().unwrap(), &parsed.partition_table.partitions).unwrap();
        assert_eq!(end, (1024 + 2048) * 512);
    }

    #[test]
    fn strip_partition_mmcblk() {
        assert_eq!(strip_partition("mmcblk0p2"), "mmcblk0");
        assert_eq!(strip_partition("mmcblk1p7"), "mmcblk1");
        assert_eq!(strip_partition("mmcblk0"), "mmcblk0");
    }

    #[test]
    fn strip_partition_sd() {
        assert_eq!(strip_partition("sda1"), "sda");
        assert_eq!(strip_partition("sdb3"), "sdb");
        assert_eq!(strip_partition("sda"), "sda");
    }

    #[test]
    fn node_partition_number_mmcblk() {
        assert_eq!(node_partition_number("/dev/mmcblk2p6").unwrap(), 6);
        assert_eq!(node_partition_number("/dev/mmcblk0p1").unwrap(), 1);
        assert_eq!(node_partition_number("/dev/mmcblk2p128").unwrap(), 128);
    }

    #[test]
    fn node_partition_number_sd() {
        assert_eq!(node_partition_number("/dev/sda1").unwrap(), 1);
        assert_eq!(node_partition_number("/dev/sdb3").unwrap(), 3);
    }
}
