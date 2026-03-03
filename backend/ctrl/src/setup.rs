use std::collections::HashSet;
use std::fs::{self, File, OpenOptions};
use std::io::{Read, Seek, SeekFrom, Write};
use std::os::unix::fs::{MetadataExt, OpenOptionsExt};
use std::path::Path;

use imbl_value::Value;
use rpc_toolkit::{from_fn_async, Context, HandlerExt, ParentHandler};
use serde::{Deserialize, Serialize};
use tokio::sync::mpsc;

use crate::{emmc, flash, init, Error, ServerContext};

const EMMC_ROOTFS_MOUNT: &str = "/mnt/emmc_rootfs";
const EMMC_OVERLAY_MOUNT: &str = "/mnt/emmc_overlay";
const EMMC_MERGED_MOUNT: &str = "/mnt/emmc_merged";

/// Files excluded from conffiles backup — the wizard writes its own versions.
const CONFFILES_EXCLUDE: &[&str] = &["/etc/shadow", "/etc/config/wireless"];

// ---------------------------------------------------------------------------
// Boot / device detection
// ---------------------------------------------------------------------------

/// Check if the system booted from removable media (setup mode).
pub fn is_setup_mode() -> bool {
    match flash::boot_device() {
        Ok(dev) => flash::mmc_device_type(&dev).as_deref() != Some("MMC"),
        Err(_) => false,
    }
}

// ---------------------------------------------------------------------------
// PMK resolution
// ---------------------------------------------------------------------------

/// Result of PMK resolution at daemon startup.
#[derive(Clone)]
pub struct ResolvedPmk {
    pub pmk_hex: String,
    /// True if the PMK came from the SD card's rootfs partition (custom image).
    pub baked_in: bool,
}

/// Magic header written by the startwrt-bake-password tool.
/// Format: 8-byte magic "SWRTPMK\0" + 64 ASCII hex chars of the PMK.
const BAKE_MAGIC: &[u8; 8] = b"SWRTPMK\0";
const BAKE_PMK_LEN: usize = 64;

/// Find a partition named `name` on block device `dev_path` and return its node.
fn find_partition_by_name(dev_path: &str, name: &str) -> Result<Option<String>, Error> {
    let sfdisk = flash::read_partition_table(dev_path)?;
    Ok(sfdisk
        .partition_table
        .partitions
        .iter()
        .find(|p| p.name.as_deref() == Some(name))
        .map(|p| p.node.clone()))
}

/// Round up to 4096-byte alignment (squashfs pad_bytes boundary).
fn align_up_4k(n: u64) -> u64 {
    (n + 4095) & !4095
}

/// Try reading a baked-in PMK from a rootfs partition device.
///
/// Reads the squashfs superblock at offset 0 to get `bytes_used`, then looks
/// for the `SWRTPMK\0` magic + 64 hex chars at the next 4096-byte aligned
/// offset. Returns `Some(pmk_hex)` if found and valid, `None` otherwise.
fn read_raw_baked_pmk(dev: &str) -> Option<String> {
    let mut f = File::open(dev).ok()?;

    // Read squashfs superblock (first 48 bytes)
    let mut sb = [0u8; 48];
    f.read_exact(&mut sb).ok()?;

    // Validate squashfs magic
    let magic = u32::from_le_bytes(sb[0..4].try_into().unwrap());
    if magic != flash::SQUASHFS_MAGIC {
        return None;
    }

    // bytes_used is at offset 40, 8 bytes LE
    let bytes_used = u64::from_le_bytes(sb[40..48].try_into().unwrap());

    // Seek to aligned offset after squashfs data
    let pmk_offset = align_up_4k(bytes_used);
    f.seek(SeekFrom::Start(pmk_offset)).ok()?;

    // Read magic + PMK
    let mut buf = [0u8; 8 + BAKE_PMK_LEN];
    f.read_exact(&mut buf).ok()?;

    if &buf[..8] != BAKE_MAGIC {
        return None;
    }

    let pmk = std::str::from_utf8(&buf[8..]).ok()?;
    if pmk.len() == BAKE_PMK_LEN && pmk.chars().all(|c| c.is_ascii_hexdigit()) {
        Some(pmk.to_string())
    } else {
        None
    }
}

/// Mount a partition at a given mount point (read-only by default).
fn mount_ro(dev: &str, mount_point: &str) -> Result<(), Error> {
    let mp = Path::new(mount_point);
    if !mp.exists() {
        fs::create_dir_all(mp)
            .map_err(|e| Error::other(format!("failed to create {mount_point}: {e}")))?;
    }
    flash::run_cmd("mount", &["-o", "ro", dev, mount_point])
}

/// Mount the three-layer SquashFS + ext4 overlay stack.
///
/// 1. squashfs rootfs → EMMC_ROOTFS_MOUNT (read-only base)
/// 2. ext4 rootfs_data → EMMC_OVERLAY_MOUNT (writable overlay)
/// 3. overlayfs → EMMC_MERGED_MOUNT (merged view)
///
/// On failure, any layers already mounted are cleaned up before returning.
fn mount_emmc_overlayfs(rootfs_dev: &str, rootfs_data_dev: &str) -> Result<(), Error> {
    // 1. Mount squashfs (read-only)
    fs::create_dir_all(EMMC_ROOTFS_MOUNT)
        .map_err(|e| Error::other(format!("mkdir {EMMC_ROOTFS_MOUNT}: {e}")))?;
    flash::run_cmd("mount", &["-t", "squashfs", rootfs_dev, EMMC_ROOTFS_MOUNT])?;

    // 2. Mount ext4 overlay (read-write)
    fs::create_dir_all(EMMC_OVERLAY_MOUNT)
        .map_err(|e| Error::other(format!("mkdir {EMMC_OVERLAY_MOUNT}: {e}")))?;
    if let Err(e) = flash::run_cmd("mount", &["-t", "ext4", rootfs_data_dev, EMMC_OVERLAY_MOUNT]) {
        let _ = flash::run_cmd("umount", &[EMMC_ROOTFS_MOUNT]);
        return Err(e);
    }

    // 3. Create upper/work dirs on the overlay
    let upper = format!("{EMMC_OVERLAY_MOUNT}/upper");
    let work = format!("{EMMC_OVERLAY_MOUNT}/work");
    if let Err(e) = fs::create_dir_all(&upper)
        .and_then(|_| fs::create_dir_all(&work))
    {
        let _ = flash::run_cmd("umount", &[EMMC_OVERLAY_MOUNT]);
        let _ = flash::run_cmd("umount", &[EMMC_ROOTFS_MOUNT]);
        return Err(Error::other(format!("mkdir overlay dirs: {e}")));
    }

    // 4. Mount overlayfs
    fs::create_dir_all(EMMC_MERGED_MOUNT)
        .map_err(|e| Error::other(format!("mkdir {EMMC_MERGED_MOUNT}: {e}")))?;
    let overlay_opts = format!(
        "lowerdir={EMMC_ROOTFS_MOUNT},upperdir={upper},workdir={work}"
    );
    if let Err(e) = flash::run_cmd("mount", &[
        "-t", "overlay", "overlay",
        "-o", &overlay_opts,
        EMMC_MERGED_MOUNT,
    ]) {
        let _ = flash::run_cmd("umount", &[EMMC_OVERLAY_MOUNT]);
        let _ = flash::run_cmd("umount", &[EMMC_ROOTFS_MOUNT]);
        return Err(e);
    }

    Ok(())
}

/// Unmount the three-layer overlayfs stack in reverse order.
///
/// After unmounting overlayfs, the kernel's dentry/inode cache may still hold
/// references to the underlying mounts, keeping them temporarily busy. A sync
/// ensures all data is on disk, then lazy unmount is used as a fallback for the
/// underlying layers.
fn umount_emmc_overlayfs() -> Result<(), Error> {
    flash::run_cmd("sync", &[])?;

    let mut errors = Vec::new();
    if let Err(e) = flash::run_cmd("umount", &[EMMC_MERGED_MOUNT]) {
        errors.push(format!("{EMMC_MERGED_MOUNT}: {e}"));
    }
    for mount in [EMMC_OVERLAY_MOUNT, EMMC_ROOTFS_MOUNT] {
        if flash::run_cmd("umount", &[mount]).is_err() {
            if let Err(e) = flash::run_cmd("umount", &["-l", mount]) {
                errors.push(format!("{mount}: {e}"));
            }
        }
    }
    if errors.is_empty() {
        Ok(())
    } else {
        Err(Error::other(format!("umount errors: {}", errors.join("; "))))
    }
}

/// Mark the overlay as FS_STATE_READY by creating a `.fs_state` symlink.
///
/// OpenWrt's `mount_root` / `fstools` checks for this symlink at the root of
/// the overlay partition. The symlink target is the state number: "2" means
/// FS_STATE_READY. Without this, `mount_root` on first boot sees
/// FS_STATE_UNKNOWN and wipes the overlay.
fn mark_overlay_ready(overlay_mount: &str) -> Result<(), Error> {
    let fs_state_path = format!("{overlay_mount}/.fs_state");
    // Remove if it already exists (e.g. from a previous flash attempt)
    let _ = fs::remove_file(&fs_state_path);
    std::os::unix::fs::symlink("2", &fs_state_path)
        .map_err(|e| Error::other(format!("create .fs_state symlink: {e}")))?;
    Ok(())
}

/// Try reading a PMK from an ext4 partition (mount, read file, unmount).
fn read_ext4_pmk(dev: &str, mount_point: &str) -> Option<String> {
    // If block mount already mounted the device (e.g., via fstab label match),
    // unmount it first to avoid "Resource busy" when we mount at our own path.
    let _ = flash::run_cmd("umount", &[dev]);

    if mount_ro(dev, mount_point).is_err() {
        return None;
    }
    let pmk_path = format!("{mount_point}/wifi_pmk");
    let result = if Path::new(&pmk_path).exists() {
        fs::read_to_string(&pmk_path).ok().map(|s| s.trim().to_string())
    } else {
        None
    };
    let _ = flash::run_cmd("umount", &[mount_point]);
    result
}

/// Resolve the WiFi PMK.
///
/// Precedence: SD baked-in (rootfs) → eMMC key_backup (ext4) → None.
/// Must be called from setup mode (booted from SD).
///
/// For the SD card, reads the squashfs superblock in the rootfs partition to
/// find `bytes_used`, then checks for the `SWRTPMK` magic at the next
/// 4096-aligned offset (written by `startwrt-bake-password`).
/// The eMMC key_backup partition always uses ext4.
pub fn resolve_pmk() -> Result<Option<ResolvedPmk>, Error> {
    let boot_dev = flash::boot_device()?;
    let sd_path = format!("/dev/{boot_dev}");

    // 1. Check SD card's rootfs partition for a baked-in PMK
    if let Ok(Some(sd_rootfs_dev)) = find_partition_by_name(&sd_path, "rootfs") {
        if let Some(pmk) = read_raw_baked_pmk(&sd_rootfs_dev) {
            return Ok(Some(ResolvedPmk {
                pmk_hex: pmk,
                baked_in: true,
            }));
        }
    }

    // 2. Check eMMC's key_backup partition (always ext4)
    let emmc_dev = match flash::find_emmc(&boot_dev) {
        Ok(dev) => dev,
        Err(_) => return Ok(None),
    };
    let emmc_path = format!("/dev/{emmc_dev}");

    if let Ok(Some(emmc_persistent_dev)) = find_partition_by_name(&emmc_path, "key_backup") {
        if let Some(pmk) = read_ext4_pmk(&emmc_persistent_dev, "/mnt/emmc_persistent") {
            return Ok(Some(ResolvedPmk {
                pmk_hex: pmk,
                baked_in: false,
            }));
        }
    }

    Ok(None)
}

// ---------------------------------------------------------------------------
// Disk state detection
// ---------------------------------------------------------------------------

#[derive(Debug, Clone, Serialize, Deserialize)]
#[serde(rename_all = "camelCase")]
pub struct DiskState {
    /// An eMMC device was found.
    pub emmc_found: bool,
    /// The eMMC has existing firmware (rootfs partition present).
    pub has_firmware: bool,
}

pub fn detect_disk_state() -> Result<DiskState, Error> {
    let boot_dev = match flash::boot_device() {
        Ok(dev) => dev,
        Err(_) => {
            return Ok(DiskState {
                emmc_found: false,
                has_firmware: false,
            })
        }
    };

    let emmc_dev = match flash::find_emmc(&boot_dev) {
        Ok(dev) => dev,
        Err(_) => {
            return Ok(DiskState {
                emmc_found: false,
                has_firmware: false,
            })
        }
    };

    let emmc_path = format!("/dev/{emmc_dev}");

    let has_firmware = match flash::read_partition_table(&emmc_path) {
        Ok(sfdisk) => sfdisk
            .partition_table
            .partitions
            .iter()
            .any(|p| p.name.as_deref() == Some("rootfs")),
        Err(_) => false,
    };

    Ok(DiskState {
        emmc_found: true,
        has_firmware,
    })
}

// ---------------------------------------------------------------------------
// Conffiles backup / restore
// ---------------------------------------------------------------------------

struct BackedUpFile {
    /// Absolute path on the target rootfs (e.g. "/etc/config/network").
    path: String,
    /// File mode bits.
    mode: u32,
    /// File contents.
    contents: Vec<u8>,
}

/// Build the list of conffiles to preserve from a mounted rootfs.
fn list_conffiles(rootfs_mount: &str) -> HashSet<String> {
    let mut files = HashSet::new();

    // /lib/upgrade/keep.d/*
    let keep_dir = format!("{rootfs_mount}/lib/upgrade/keep.d");
    if let Ok(entries) = fs::read_dir(&keep_dir) {
        for entry in entries.flatten() {
            if let Ok(content) = fs::read_to_string(entry.path()) {
                for line in content.lines() {
                    let line = line.trim();
                    if !line.is_empty() && !line.starts_with('#') {
                        files.insert(line.to_string());
                    }
                }
            }
        }
    }

    // /etc/sysupgrade.conf
    let sysupgrade_conf = format!("{rootfs_mount}/etc/sysupgrade.conf");
    if let Ok(content) = fs::read_to_string(&sysupgrade_conf) {
        for line in content.lines() {
            let line = line.trim();
            if !line.is_empty() && !line.starts_with('#') {
                files.insert(line.to_string());
            }
        }
    }

    // opkg conffiles: /usr/lib/opkg/status
    let opkg_status = format!("{rootfs_mount}/usr/lib/opkg/status");
    if let Ok(content) = fs::read_to_string(&opkg_status) {
        let mut in_conffiles = false;
        for line in content.lines() {
            if line.starts_with("Conffiles:") {
                in_conffiles = true;
                continue;
            }
            if in_conffiles {
                if line.starts_with(' ') {
                    if let Some(path) = line.trim().split_whitespace().next() {
                        if !path.is_empty() {
                            files.insert(path.to_string());
                        }
                    }
                } else {
                    in_conffiles = false;
                }
            }
        }
    }

    // apk conffiles: /lib/apk/packages/*.conffiles_static
    let apk_dir = format!("{rootfs_mount}/lib/apk/packages");
    if let Ok(entries) = fs::read_dir(&apk_dir) {
        for entry in entries.flatten() {
            let name = entry.file_name();
            if name.to_string_lossy().ends_with(".conffiles_static") {
                if let Ok(content) = fs::read_to_string(entry.path()) {
                    for line in content.lines() {
                        let line = line.trim();
                        if !line.is_empty() {
                            files.insert(line.to_string());
                        }
                    }
                }
            }
        }
    }

    // Remove excluded files
    for exclude in CONFFILES_EXCLUDE {
        files.remove(*exclude);
    }

    files
}

/// Back up conffiles from a mounted rootfs.
fn backup_conffiles(rootfs_mount: &str) -> Vec<BackedUpFile> {
    let conffiles = list_conffiles(rootfs_mount);
    let mut backed_up = Vec::new();

    for path in conffiles {
        let full_path = format!("{rootfs_mount}{path}");
        let p = Path::new(&full_path);
        if !p.exists() || !p.is_file() {
            continue;
        }
        let Ok(metadata) = fs::metadata(&full_path) else {
            continue;
        };
        let Ok(contents) = fs::read(&full_path) else {
            continue;
        };
        backed_up.push(BackedUpFile {
            path,
            mode: metadata.mode(),
            contents,
        });
    }

    backed_up
}

/// Restore backed-up conffiles to a mounted rootfs.
fn restore_conffiles(rootfs_mount: &str, files: &[BackedUpFile]) -> Result<(), Error> {
    for file in files {
        let full_path = format!("{rootfs_mount}{}", file.path);
        if let Some(parent) = Path::new(&full_path).parent() {
            fs::create_dir_all(parent)
                .map_err(|e| Error::other(format!("mkdir {}: {e}", parent.display())))?;
        }

        // Atomic write: tmp file → sync → rename to avoid partial writes on
        // power loss between truncate and write completion.
        let tmp_path = format!("{full_path}.tmp");
        let mut f = OpenOptions::new()
            .write(true)
            .create(true)
            .truncate(true)
            .mode(file.mode)
            .open(&tmp_path)
            .map_err(|e| Error::other(format!("create {tmp_path}: {e}")))?;
        f.write_all(&file.contents)
            .map_err(|e| Error::other(format!("write {tmp_path}: {e}")))?;
        f.sync_all()
            .map_err(|e| Error::other(format!("sync {tmp_path}: {e}")))?;
        fs::rename(&tmp_path, &full_path)
            .map_err(|e| Error::other(format!("rename {tmp_path} -> {full_path}: {e}")))?;
    }
    Ok(())
}

// ---------------------------------------------------------------------------
// Post-flash configuration
// ---------------------------------------------------------------------------

/// Write the admin password hash to /etc/shadow on a mounted rootfs.
fn write_admin_password(rootfs_mount: &str, password: &str) -> Result<(), Error> {
    let shadow_path = format!("{rootfs_mount}/etc/shadow");
    let shadow = fs::read_to_string(&shadow_path)
        .map_err(|e| Error::other(format!("read {shadow_path}: {e}")))?;

    let new_hash = pwhash::sha512_crypt::hash(password)
        .map_err(|e| Error::other(format!("hash password: {e}")))?;

    let mut found = false;
    let new_content: String = shadow
        .lines()
        .map(|line| {
            if line.starts_with("root:") {
                found = true;
                let parts: Vec<&str> = line.split(':').collect();
                if parts.len() >= 2 {
                    let mut owned: Vec<String> =
                        parts.iter().map(|s| s.to_string()).collect();
                    owned[1] = new_hash.clone();
                    return owned.join(":");
                }
            }
            line.to_string()
        })
        .collect::<Vec<_>>()
        .join("\n");

    if !found {
        return Err(Error::other("root user not found in /etc/shadow"));
    }

    let new_content = if new_content.ends_with('\n') {
        new_content
    } else {
        format!("{new_content}\n")
    };

    // Atomic write
    let tmp_path = format!("{shadow_path}.tmp");
    let mut f = OpenOptions::new()
        .write(true)
        .create(true)
        .truncate(true)
        .mode(0o600)
        .open(&tmp_path)
        .map_err(|e| Error::other(format!("create {tmp_path}: {e}")))?;
    f.write_all(new_content.as_bytes())
        .map_err(|e| Error::other(format!("write {tmp_path}: {e}")))?;
    f.sync_all()
        .map_err(|e| Error::other(format!("sync {tmp_path}: {e}")))?;
    fs::rename(&tmp_path, &shadow_path)
        .map_err(|e| Error::other(format!("rename {tmp_path}: {e}")))?;

    Ok(())
}

// ---------------------------------------------------------------------------
// Flash orchestration
// ---------------------------------------------------------------------------

#[derive(Debug, Clone, Copy, PartialEq, Eq, Deserialize)]
#[serde(rename_all = "kebab-case")]
pub enum FlashMode {
    Update,
    FreshStart,
}

/// Streaming event sent to the frontend during flash.
#[derive(Debug, Clone, Serialize)]
#[serde(tag = "phase", rename_all = "camelCase", rename_all_fields = "camelCase")]
pub enum SetupEvent {
    Copying {
        copied: u64,
        total: u64,
        step: u32,
        total_steps: u32,
    },
    Status {
        message: String,
        step: u32,
        total_steps: u32,
    },
    Complete,
    Error {
        message: String,
    },
}

/// Run the full setup flash sequence (blocking).
///
/// Sends progress events through `tx`. On error, sends an Error event
/// before returning.
pub fn run_setup_flash(
    mode: FlashMode,
    password: &str,
    pmk_hex: &str,
    tx: &mpsc::Sender<SetupEvent>,
) {
    if let Err(e) = run_setup_flash_inner(mode, password, pmk_hex, tx) {
        let _ = tx.blocking_send(SetupEvent::Error {
            message: e.to_string(),
        });
    }
}

fn run_setup_flash_inner(
    mode: FlashMode,
    password: &str,
    pmk_hex: &str,
    tx: &mpsc::Sender<SetupEvent>,
) -> Result<(), Error> {
    let total_steps: u32 = 3;

    // ── Step 1: Preparing ──────────────────────────────────────────────
    let _ = tx.blocking_send(SetupEvent::Status {
        message: "Validating password...".into(),
        step: 1,
        total_steps,
    });

    if password.len() < 12 {
        return Err(Error::other("password must be at least 12 characters"));
    }

    // Update: backup conffiles before flash (mount old eMMC squashfs + overlay)
    let conffiles_backup = if mode == FlashMode::Update {
        let _ = tx.blocking_send(SetupEvent::Status {
            message: "Backing up configuration...".into(),
            step: 1,
            total_steps,
        });

        let boot_dev = flash::boot_device()?;
        let emmc_dev = flash::find_emmc(&boot_dev)?;
        let emmc_path = format!("/dev/{emmc_dev}");

        // Find eMMC rootfs and rootfs_data
        let rootfs_dev = find_partition_by_name(&emmc_path, "rootfs")?
            .ok_or_else(|| Error::other("rootfs partition not found on eMMC"))?;
        let rootfs_data_dev = find_partition_by_name(&emmc_path, "rootfs_data")?
            .ok_or_else(|| Error::other("rootfs_data partition not found on eMMC"))?;

        // Mount squashfs + overlay to get merged view of old config
        mount_emmc_overlayfs(&rootfs_dev, &rootfs_data_dev)?;
        let backup = backup_conffiles(EMMC_MERGED_MOUNT);
        let _ = umount_emmc_overlayfs();

        Some(backup)
    } else {
        None
    };

    // ── Step 2: Flashing eMMC ──────────────────────────────────────────
    let on_progress = |event: flash::FlashEvent| {
        let setup_event = match event {
            flash::FlashEvent::Copying { copied, total } => SetupEvent::Copying {
                copied,
                total,
                step: 2,
                total_steps,
            },
            flash::FlashEvent::Status { message } => SetupEvent::Status {
                message,
                step: 2,
                total_steps,
            },
        };
        let _ = tx.blocking_send(setup_event);
    };
    let result = flash::run_flash_unattended(&on_progress)?;

    // ── Step 3: Applying settings ──────────────────────────────────────
    let _ = tx.blocking_send(SetupEvent::Status {
        message: "Configuring system...".into(),
        step: 3,
        total_steps,
    });

    // Mount SquashFS + ext4 overlay to get a writable merged view
    mount_emmc_overlayfs(&result.rootfs_dev, &result.rootfs_data_dev)?;

    // Restore conffiles (Update only)
    if let Some(ref files) = conffiles_backup {
        let _ = tx.blocking_send(SetupEvent::Status {
            message: "Restoring configuration...".into(),
            step: 3,
            total_steps,
        });
        restore_conffiles(EMMC_MERGED_MOUNT, files)?;
    }

    // Write admin password
    let _ = tx.blocking_send(SetupEvent::Status {
        message: "Setting admin password...".into(),
        step: 3,
        total_steps,
    });
    write_admin_password(EMMC_MERGED_MOUNT, password)?;

    // Write WiFi config
    // TODO: uciedit::dump_all() truncates files in-place (set_len(0) + write)
    // rather than using write-to-tmp + rename. A crash here leaves a corrupted
    // /etc/config/wireless on the eMMC. Fix in the uciedit crate.
    let _ = tx.blocking_send(SetupEvent::Status {
        message: "Configuring WiFi...".into(),
        step: 3,
        total_steps,
    });
    let uci_root = format!("{EMMC_MERGED_MOUNT}/etc/config");
    init::configure_wifi(&uci_root, pmk_hex, None)?;

    // Mark overlay as FS_STATE_READY so mount_root doesn't wipe it on first boot
    mark_overlay_ready(EMMC_OVERLAY_MOUNT)?;

    // Unmount the overlayfs stack
    umount_emmc_overlayfs()?;

    // Write PMK to eMMC key_backup partition.
    //
    // run_flash_core mounted /key_backup, but the hotplug/block-mount handler
    // (triggered asynchronously by partx -u) may have unmounted it in the
    // meantime. Verify the mount is still active; if not, remount explicitly
    // using the known eMMC device so we don't write to the rootfs directory.
    if !emmc::is_persistent_mounted() {
        eprintln!("WARNING: /key_backup was unmounted (likely by hotplug); remounting");
        flash::run_cmd("mount", &[&result.persistent_dev, emmc::PERSISTENT_MOUNT])?;
    }

    let _ = tx.blocking_send(SetupEvent::Status {
        message: "Writing WiFi credentials...".into(),
        step: 3,
        total_steps,
    });
    emmc::write_pmk(pmk_hex)?;

    // Unmount key_backup to flush all data to disk and guarantee durability
    // before the Complete event tells the client it's safe to reboot.
    flash::run_cmd("umount", &[emmc::PERSISTENT_MOUNT])?;

    let _ = tx.blocking_send(SetupEvent::Complete);

    Ok(())
}

// ---------------------------------------------------------------------------
// RPC endpoints
// ---------------------------------------------------------------------------

#[derive(Debug, Clone, Serialize, Deserialize)]
#[serde(rename_all = "camelCase")]
pub struct SetupStatusRes {
    pub setup_mode: bool,
    pub disk: DiskState,
}

async fn setup_status_impl(_ctx: ServerContext) -> Result<SetupStatusRes, Error> {
    tokio::task::spawn_blocking(|| {
        let disk = detect_disk_state()?;
        Ok(SetupStatusRes {
            setup_mode: is_setup_mode(),
            disk,
        })
    })
    .await
    .map_err(|e| Error::other(format!("setup status task panicked: {e}")))?
}

pub fn setup<C: Context>() -> ParentHandler<C> {
    ParentHandler::new().subcommand(
        "status",
        from_fn_async(setup_status_impl)
            .with_metadata("no_auth", Value::Bool(true))
            .no_cli(),
    )
}

#[cfg(test)]
mod tests {
    use super::*;
    use std::io::Write;
    use std::os::unix::fs::PermissionsExt;
    use tempfile::TempDir;
    use tokio::sync::mpsc;

    // ── SetupEvent serialization ─────────────────────────────────────

    #[test]
    fn setup_event_copying_json() {
        let event = SetupEvent::Copying {
            copied: 100,
            total: 1000,
            step: 2,
            total_steps: 3,
        };
        let json: serde_json::Value = serde_json::to_value(&event).unwrap();
        assert_eq!(json["phase"], "copying");
        assert_eq!(json["copied"], 100);
        assert_eq!(json["total"], 1000);
        assert_eq!(json["step"], 2);
        assert_eq!(json["totalSteps"], 3);
    }

    #[test]
    fn setup_event_status_json() {
        let event = SetupEvent::Status {
            message: "Flashing...".into(),
            step: 1,
            total_steps: 3,
        };
        let json: serde_json::Value = serde_json::to_value(&event).unwrap();
        assert_eq!(json["phase"], "status");
        assert_eq!(json["message"], "Flashing...");
        assert_eq!(json["step"], 1);
        assert_eq!(json["totalSteps"], 3);
    }

    #[test]
    fn setup_event_complete_json() {
        let event = SetupEvent::Complete;
        let parsed: serde_json::Value = serde_json::to_value(&event).unwrap();
        assert_eq!(parsed, serde_json::json!({"phase": "complete"}));
    }

    #[test]
    fn setup_event_error_json() {
        let event = SetupEvent::Error {
            message: "disk full".into(),
        };
        let json: serde_json::Value = serde_json::to_value(&event).unwrap();
        assert_eq!(json["phase"], "error");
        assert_eq!(json["message"], "disk full");
    }

    // ── FlashMode deserialization ────────────────────────────────────

    #[test]
    fn flash_mode_from_kebab_case() {
        let update: FlashMode = serde_json::from_str("\"update\"").unwrap();
        assert_eq!(update, FlashMode::Update);
        let fresh: FlashMode = serde_json::from_str("\"fresh-start\"").unwrap();
        assert_eq!(fresh, FlashMode::FreshStart);
    }

    #[test]
    fn flash_mode_invalid() {
        let result = serde_json::from_str::<FlashMode>("\"invalid\"");
        assert!(result.is_err());
    }

    // ── read_raw_baked_pmk ───────────────────────────────────────────

    /// Create a fake rootfs file with squashfs superblock + optional baked PMK.
    fn make_fake_rootfs(
        path: &std::path::Path,
        bytes_used: u64,
        pmk_magic: &[u8],
        pmk_data: &[u8],
    ) {
        let mut f = fs::File::create(path).unwrap();

        // Write squashfs superblock (48 bytes)
        let mut sb = [0u8; 48];
        sb[0..4].copy_from_slice(&flash::SQUASHFS_MAGIC.to_le_bytes());
        sb[40..48].copy_from_slice(&bytes_used.to_le_bytes());
        f.write_all(&sb).unwrap();

        // Pad to aligned offset: align_up_4k(bytes_used)
        let pmk_offset = align_up_4k(bytes_used) as usize;
        if pmk_offset > 48 {
            f.write_all(&vec![0u8; pmk_offset - 48]).unwrap();
        }

        // Write magic + PMK data
        f.write_all(pmk_magic).unwrap();
        f.write_all(pmk_data).unwrap();
    }

    #[test]
    fn raw_baked_pmk_valid() {
        let dir = TempDir::new().unwrap();
        let path = dir.path().join("rootfs");
        let pmk_hex = format!("{:0>64}", "a1b2c3d4e5f6");

        make_fake_rootfs(&path, 1000, BAKE_MAGIC, pmk_hex.as_bytes());

        let result = read_raw_baked_pmk(path.to_str().unwrap());
        assert_eq!(result, Some(pmk_hex));
    }

    #[test]
    fn raw_baked_pmk_wrong_magic() {
        let dir = TempDir::new().unwrap();
        let path = dir.path().join("rootfs");
        let pmk_hex = "a".repeat(64);

        make_fake_rootfs(&path, 1000, b"WRONGMAG", pmk_hex.as_bytes());

        assert_eq!(read_raw_baked_pmk(path.to_str().unwrap()), None);
    }

    #[test]
    fn raw_baked_pmk_short_file() {
        let dir = TempDir::new().unwrap();
        let path = dir.path().join("rootfs");
        fs::write(&path, b"SHORT").unwrap();
        assert_eq!(read_raw_baked_pmk(path.to_str().unwrap()), None);
    }

    #[test]
    fn raw_baked_pmk_invalid_hex() {
        let dir = TempDir::new().unwrap();
        let path = dir.path().join("rootfs");

        make_fake_rootfs(&path, 1000, BAKE_MAGIC, "z".repeat(64).as_bytes());

        assert_eq!(read_raw_baked_pmk(path.to_str().unwrap()), None);
    }

    #[test]
    fn raw_baked_pmk_nonexistent_file() {
        assert_eq!(read_raw_baked_pmk("/nonexistent/path/rootfs"), None);
    }

    #[test]
    fn raw_baked_pmk_no_squashfs_magic() {
        let dir = TempDir::new().unwrap();
        let path = dir.path().join("rootfs");

        // Write 48 bytes of zeros (no squashfs magic)
        let mut f = fs::File::create(&path).unwrap();
        f.write_all(&[0u8; 48]).unwrap();
        drop(f);

        assert_eq!(read_raw_baked_pmk(path.to_str().unwrap()), None);
    }

    #[test]
    fn raw_baked_pmk_no_pmk_written() {
        let dir = TempDir::new().unwrap();
        let path = dir.path().join("rootfs");

        // Valid squashfs header but no PMK written after it
        let mut f = fs::File::create(&path).unwrap();
        let mut sb = [0u8; 48];
        sb[0..4].copy_from_slice(&flash::SQUASHFS_MAGIC.to_le_bytes());
        let bytes_used: u64 = 1000;
        sb[40..48].copy_from_slice(&bytes_used.to_le_bytes());
        f.write_all(&sb).unwrap();
        // Pad to alignment but leave all zeros (no SWRTPMK magic)
        f.write_all(&vec![0u8; 4096 - 48 + 72]).unwrap();
        drop(f);

        assert_eq!(read_raw_baked_pmk(path.to_str().unwrap()), None);
    }

    // ── list_conffiles ───────────────────────────────────────────────

    #[test]
    fn list_conffiles_keep_d() {
        let dir = TempDir::new().unwrap();
        let root = dir.path();
        let keep_d = root.join("lib/upgrade/keep.d");
        fs::create_dir_all(&keep_d).unwrap();
        fs::write(keep_d.join("base"), "/etc/config/network\n/etc/hosts\n").unwrap();

        let files = list_conffiles(root.to_str().unwrap());
        assert!(files.contains("/etc/config/network"));
        assert!(files.contains("/etc/hosts"));
    }

    #[test]
    fn list_conffiles_sysupgrade_conf() {
        let dir = TempDir::new().unwrap();
        let root = dir.path();
        fs::create_dir_all(root.join("etc")).unwrap();
        fs::write(root.join("etc/sysupgrade.conf"), "/etc/foo.conf\n").unwrap();

        let files = list_conffiles(root.to_str().unwrap());
        assert!(files.contains("/etc/foo.conf"));
    }

    #[test]
    fn list_conffiles_opkg_status() {
        let dir = TempDir::new().unwrap();
        let root = dir.path();
        fs::create_dir_all(root.join("usr/lib/opkg")).unwrap();
        let status_content = "\
Package: base-files
Status: install ok installed
Conffiles:
 /etc/profile abc123
 /etc/shells def456
Architecture: riscv64

Package: dropbear
Status: install ok installed
Conffiles:
 /etc/dropbear/authorized_keys xyz789
";
        fs::write(root.join("usr/lib/opkg/status"), status_content).unwrap();

        let files = list_conffiles(root.to_str().unwrap());
        assert!(files.contains("/etc/profile"));
        assert!(files.contains("/etc/shells"));
        assert!(files.contains("/etc/dropbear/authorized_keys"));
    }

    #[test]
    fn list_conffiles_apk() {
        let dir = TempDir::new().unwrap();
        let root = dir.path();
        fs::create_dir_all(root.join("lib/apk/packages")).unwrap();
        fs::write(
            root.join("lib/apk/packages/base-files.conffiles_static"),
            "/etc/apk.conf\n/etc/apk2.conf\n",
        )
        .unwrap();

        let files = list_conffiles(root.to_str().unwrap());
        assert!(files.contains("/etc/apk.conf"));
        assert!(files.contains("/etc/apk2.conf"));
    }

    #[test]
    fn list_conffiles_excludes() {
        let dir = TempDir::new().unwrap();
        let root = dir.path();
        let keep_d = root.join("lib/upgrade/keep.d");
        fs::create_dir_all(&keep_d).unwrap();
        fs::write(
            keep_d.join("base"),
            "/etc/shadow\n/etc/config/wireless\n/etc/config/network\n",
        )
        .unwrap();

        let files = list_conffiles(root.to_str().unwrap());
        assert!(!files.contains("/etc/shadow"), "shadow should be excluded");
        assert!(
            !files.contains("/etc/config/wireless"),
            "wireless should be excluded"
        );
        assert!(files.contains("/etc/config/network"));
    }

    #[test]
    fn list_conffiles_comments_and_blanks() {
        let dir = TempDir::new().unwrap();
        let root = dir.path();
        let keep_d = root.join("lib/upgrade/keep.d");
        fs::create_dir_all(&keep_d).unwrap();
        fs::write(
            keep_d.join("base"),
            "# this is a comment\n\n/etc/real\n  \n# another comment\n",
        )
        .unwrap();

        let files = list_conffiles(root.to_str().unwrap());
        assert!(files.contains("/etc/real"));
        assert_eq!(files.len(), 1);
    }

    #[test]
    fn list_conffiles_empty_rootfs() {
        let dir = TempDir::new().unwrap();
        let files = list_conffiles(dir.path().to_str().unwrap());
        assert!(files.is_empty());
    }

    // ── backup / restore round-trip ──────────────────────────────────

    #[test]
    fn backup_restore_round_trip() {
        let dir = TempDir::new().unwrap();
        let root = dir.path();

        // Create conffiles listing
        let keep_d = root.join("lib/upgrade/keep.d");
        fs::create_dir_all(&keep_d).unwrap();
        fs::write(keep_d.join("base"), "/etc/config/network\n/etc/hosts\n").unwrap();

        // Create actual files with content and specific modes
        fs::create_dir_all(root.join("etc/config")).unwrap();
        fs::write(root.join("etc/config/network"), "config interface 'lan'\n").unwrap();
        fs::set_permissions(
            root.join("etc/config/network"),
            fs::Permissions::from_mode(0o644),
        )
        .unwrap();

        fs::write(root.join("etc/hosts"), "127.0.0.1 localhost\n").unwrap();
        fs::set_permissions(root.join("etc/hosts"), fs::Permissions::from_mode(0o644))
            .unwrap();

        // Backup
        let rootfs = root.to_str().unwrap();
        let backup = backup_conffiles(rootfs);
        assert_eq!(backup.len(), 2);

        // Wipe originals
        fs::remove_file(root.join("etc/config/network")).unwrap();
        fs::remove_file(root.join("etc/hosts")).unwrap();

        // Restore to a fresh location
        let restore_dir = TempDir::new().unwrap();
        let restore_root = restore_dir.path();
        restore_conffiles(restore_root.to_str().unwrap(), &backup).unwrap();

        // Verify content
        let network = fs::read_to_string(restore_root.join("etc/config/network")).unwrap();
        assert_eq!(network, "config interface 'lan'\n");

        let hosts = fs::read_to_string(restore_root.join("etc/hosts")).unwrap();
        assert_eq!(hosts, "127.0.0.1 localhost\n");

        // Verify mode bits (mask to permission bits only)
        let network_mode =
            fs::metadata(restore_root.join("etc/config/network")).unwrap().mode() & 0o7777;
        assert_eq!(network_mode, 0o644);
    }

    #[test]
    fn backup_skips_missing_files() {
        let dir = TempDir::new().unwrap();
        let root = dir.path();

        // List a file that doesn't exist on disk
        let keep_d = root.join("lib/upgrade/keep.d");
        fs::create_dir_all(&keep_d).unwrap();
        fs::write(keep_d.join("base"), "/etc/nonexistent\n").unwrap();

        let backup = backup_conffiles(root.to_str().unwrap());
        assert!(backup.is_empty());
    }

    #[test]
    fn restore_creates_parent_dirs() {
        let dir = TempDir::new().unwrap();
        let root = dir.path();

        let files = vec![BackedUpFile {
            path: "/etc/config/deep/nested/file.conf".to_string(),
            mode: 0o644,
            contents: b"content".to_vec(),
        }];

        restore_conffiles(root.to_str().unwrap(), &files).unwrap();

        let restored =
            fs::read_to_string(root.join("etc/config/deep/nested/file.conf")).unwrap();
        assert_eq!(restored, "content");
    }

    // ── write_admin_password ─────────────────────────────────────────

    #[test]
    fn write_password_replaces_root_hash() {
        let dir = TempDir::new().unwrap();
        let root = dir.path();
        fs::create_dir_all(root.join("etc")).unwrap();
        fs::write(
            root.join("etc/shadow"),
            "root:*:0:0:99999:7:::\nnobody:!:0:0:99999:7:::\n",
        )
        .unwrap();

        write_admin_password(root.to_str().unwrap(), "mysecurepassword").unwrap();

        let shadow = fs::read_to_string(root.join("etc/shadow")).unwrap();
        let root_line = shadow.lines().find(|l| l.starts_with("root:")).unwrap();
        let hash = root_line.split(':').nth(1).unwrap();
        assert!(hash.starts_with("$6$"), "hash should be sha512_crypt, got: {hash}");
        assert_ne!(hash, "*", "hash should have been replaced");
    }

    #[test]
    fn write_password_preserves_other_fields() {
        let dir = TempDir::new().unwrap();
        let root = dir.path();
        fs::create_dir_all(root.join("etc")).unwrap();
        fs::write(
            root.join("etc/shadow"),
            "root:*:19000:0:99999:7:::\nnobody:!:19000:0:99999:7:::\n",
        )
        .unwrap();

        write_admin_password(root.to_str().unwrap(), "mysecurepassword").unwrap();

        let shadow = fs::read_to_string(root.join("etc/shadow")).unwrap();

        // Check root's other fields are preserved
        let root_line = shadow.lines().find(|l| l.starts_with("root:")).unwrap();
        let parts: Vec<&str> = root_line.split(':').collect();
        assert_eq!(parts[0], "root");
        // parts[1] is the new hash — skip
        assert_eq!(parts[2], "19000");
        assert_eq!(parts[3], "0");
        assert_eq!(parts[4], "99999");
        assert_eq!(parts[5], "7");

        // Check nobody line is completely unchanged
        let nobody_line = shadow.lines().find(|l| l.starts_with("nobody:")).unwrap();
        assert_eq!(nobody_line, "nobody:!:19000:0:99999:7:::");
    }

    #[test]
    fn write_password_no_root_entry() {
        let dir = TempDir::new().unwrap();
        let root = dir.path();
        fs::create_dir_all(root.join("etc")).unwrap();
        fs::write(root.join("etc/shadow"), "nobody:!:0:0:99999:7:::\n").unwrap();

        let result = write_admin_password(root.to_str().unwrap(), "mysecurepassword");
        assert!(result.is_err());
        let err = result.unwrap_err().to_string();
        assert!(err.contains("root"), "error should mention root: {err}");
    }

    #[test]
    fn write_password_trailing_newline() {
        let dir = TempDir::new().unwrap();
        let root = dir.path();
        fs::create_dir_all(root.join("etc")).unwrap();
        // Input without trailing newline
        fs::write(root.join("etc/shadow"), "root:*:0:0:99999:7:::").unwrap();

        write_admin_password(root.to_str().unwrap(), "mysecurepassword").unwrap();

        let shadow = fs::read_to_string(root.join("etc/shadow")).unwrap();
        assert!(shadow.ends_with('\n'), "output should end with newline");
    }

    // ── run_setup_flash event protocol ───────────────────────────────

    #[test]
    fn flash_rejects_short_password() {
        let (tx, mut rx) = mpsc::channel(16);
        run_setup_flash(FlashMode::FreshStart, "short", &"ab".repeat(32), &tx);
        drop(tx);

        let mut events = Vec::new();
        while let Ok(event) = rx.try_recv() {
            events.push(event);
        }

        // First event: Status with step 1 ("Validating password...")
        assert!(
            events.len() >= 2,
            "expected at least 2 events, got {}",
            events.len()
        );
        match &events[0] {
            SetupEvent::Status { step, message, .. } => {
                assert_eq!(*step, 1);
                assert!(
                    message.contains("Validating"),
                    "expected validating message, got: {message}"
                );
            }
            other => panic!("expected Status event, got: {other:?}"),
        }

        // Last event: Error about password length
        match events.last().unwrap() {
            SetupEvent::Error { message } => {
                assert!(message.contains("12"), "error should mention 12 chars: {message}");
            }
            other => panic!("expected Error event, got: {other:?}"),
        }
    }
}
