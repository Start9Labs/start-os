use std::io::{self, Write};
use std::process::Command;
use std::thread;
use std::time::Duration;

use uciedit::openwrt::{WifiInterface, WifiMode};
use uciedit::{parse_all, Arena};

use crate::{flash, Error};

/// Run all factory QC verification checks.
pub fn run_verify() -> Result<(), Error> {
    println!();
    println!("========================================");
    println!("   StartWRT Verification");
    println!("========================================");
    println!();

    let mut passed = true;

    // Step 1: Firmware integrity
    if !verify_firmware_integrity()? {
        passed = false;
    }

    // Step 2: WiFi SSID broadcast
    if !verify_wifi_broadcast()? {
        passed = false;
    }

    // Firmware version
    let version = env!("CARGO_PKG_VERSION");
    let git_hash = env!("STARTWRT_GIT_HASH");
    println!();
    println!("Firmware version: {version} ({git_hash})");

    println!();
    if passed {
        println!("========================================");
        println!("   VERIFICATION PASSED");
        println!("========================================");
    } else {
        println!("========================================");
        println!("   VERIFICATION FAILED");
        println!("========================================");
        return Err(Error::other("one or more checks failed"));
    }
    println!();

    Ok(())
}

/// Verify firmware integrity by checking the squashfs superblock on eMMC.
///
/// Validates that the squashfs magic is present and `bytes_used` is sane,
/// confirming the image was not truncated during flash.
fn verify_firmware_integrity() -> Result<bool, Error> {
    print!("[1/2] Firmware integrity... ");
    io::stdout().flush().ok();

    // Find the eMMC device — works whether booted from eMMC or SD.
    let emmc_dev = find_emmc_device()?;
    let emmc_path = format!("/dev/{emmc_dev}");

    let sfdisk = flash::read_partition_table(&emmc_path)?;
    let partitions = &sfdisk.partition_table.partitions;

    let rootfs = partitions
        .iter()
        .find(|p| p.name.as_deref() == Some("rootfs"))
        .ok_or_else(|| Error::other("no rootfs partition found on eMMC"))?;

    let rootfs_start_bytes = rootfs.start * flash::SECTOR_SIZE;
    let rootfs_size_bytes = rootfs.size * flash::SECTOR_SIZE;

    match flash::squashfs_bytes_used(&emmc_path, rootfs_start_bytes) {
        Some(bytes_used) if bytes_used > 0 && bytes_used <= rootfs_size_bytes => {
            let mb = bytes_used / 1_000_000;
            println!("PASS");
            println!("  squashfs valid, {mb} MB");
            Ok(true)
        }
        Some(bytes_used) => {
            println!("FAIL");
            println!("  squashfs bytes_used out of range: {bytes_used}");
            Ok(false)
        }
        None => {
            println!("FAIL");
            println!("  squashfs magic not found on eMMC rootfs partition");
            Ok(false)
        }
    }
}

/// Verify that the StartWRT WiFi SSID is actively broadcasting.
///
/// Only runs when booted from eMMC — the WiFi stack on the SD card is not
/// representative of the flashed image. When booted from SD, the check is
/// skipped with a message directing the operator to reboot from eMMC.
///
/// Checks for hostapd interfaces in ubus and confirms the SSID from UCI.
/// Retries up to 3 times with a 2-second delay since hostapd may still be
/// starting after init/manufacture.
fn verify_wifi_broadcast() -> Result<bool, Error> {
    print!("[2/2] WiFi SSID broadcast... ");
    io::stdout().flush().ok();

    // Only meaningful when booted from eMMC
    let boot_dev = flash::boot_device()?;
    if flash::mmc_device_type(&boot_dev).as_deref() != Some("MMC") {
        println!("SKIPPED");
        println!("  reboot from eMMC to verify WiFi broadcast");
        return Ok(true);
    }

    let mut hostapd_ifaces = Vec::new();
    for attempt in 0..3 {
        if attempt > 0 {
            thread::sleep(Duration::from_secs(2));
        }
        let output = Command::new("ubus")
            .arg("list")
            .output()
            .map_err(|e| Error::other(format!("failed to run ubus list: {e}")))?;

        if output.status.success() {
            let stdout = String::from_utf8_lossy(&output.stdout);
            hostapd_ifaces = stdout
                .lines()
                .filter(|l| l.starts_with("hostapd."))
                .map(|l| l.strip_prefix("hostapd.").unwrap_or(l).to_string())
                .collect();
            if !hostapd_ifaces.is_empty() {
                break;
            }
        }
    }

    if hostapd_ifaces.is_empty() {
        println!("FAIL");
        println!("  no hostapd interfaces found (WiFi not broadcasting)");
        return Ok(false);
    }

    // Read SSID from UCI to confirm it's "StartWRT"
    let ssid = read_configured_ssid().unwrap_or_default();

    if ssid != "StartWRT" {
        println!("FAIL");
        println!("  hostapd interfaces: {}", hostapd_ifaces.join(", "));
        if ssid.is_empty() {
            println!("  SSID: not configured");
        } else {
            println!("  SSID: {ssid} (expected StartWRT)");
        }
        return Ok(false);
    }

    println!("PASS");
    println!("  hostapd interfaces: {}", hostapd_ifaces.join(", "));
    println!("  SSID: {ssid}");
    Ok(true)
}

/// Read the configured SSID from the first AP wifi-iface in UCI.
fn read_configured_ssid() -> Option<String> {
    let arena = Arena::new();
    let cfgs = parse_all("/etc/config", &arena, &["wireless"]).ok()?;

    for section in &cfgs["wireless"].sections {
        if let Ok(Some(iface)) = section.get_typed::<WifiInterface>() {
            if iface.mode == WifiMode::AP {
                return Some(iface.ssid.to_string());
            }
        }
    }
    None
}

/// Find the eMMC block device, whether booted from eMMC or SD.
fn find_emmc_device() -> Result<String, Error> {
    let boot_dev = flash::boot_device()?;
    if flash::mmc_device_type(&boot_dev).as_deref() == Some("MMC") {
        // Booted from eMMC — the boot device is the eMMC
        Ok(boot_dev)
    } else {
        // Booted from SD — find the eMMC
        flash::find_emmc(&boot_dev)
    }
}
