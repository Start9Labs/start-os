use std::io::{self, BufRead, Write};

use uciedit::openwrt::{WifiDevice, WifiDynamicVlan, WifiInterface, WifiMode};
use uciedit::{parse_all, Arena};

use crate::emmc;
use crate::prelude::*;

use crate::PASSWORD_CHARS;
const PASSWORD_LEN: usize = 12;

/// Validate that a password meets the sticker password requirements.
fn validate_password(password: &str) -> Result<(), String> {
    if password.len() != PASSWORD_LEN {
        return Err(format!(
            "password must be exactly {PASSWORD_LEN} characters (got {})",
            password.len()
        ));
    }
    for ch in password.chars() {
        if !PASSWORD_CHARS.contains(ch) {
            return Err(format!("invalid character '{ch}' in password"));
        }
    }
    Ok(())
}

/// Configure WiFi: set SSID, encryption, password, and enable all radios.
///
/// Follows the UCI parse/modify/dump pattern from wifi.rs with conflict retry.
/// `uci_root` is typically "/etc/config" but can be a mounted eMMC path for
/// post-flash configuration.
///
/// `max_stations` optionally limits the number of associated stations per
/// interface (UCI `maxassoc`). Pass `Some(1)` in setup mode to restrict the
/// AP to a single client during reflash. Pass `None` for normal operation.
pub async fn configure_wifi(uci_root: &str, password: &str, max_stations: Option<u32>) -> Result<(), Error> {
    let mut retries = 4;
    loop {
        // Read raw bytes (Send future) — no arena yet
        let bytes = uciedit::read_all(uci_root, &["wireless"]).await?;
        // Parse + mutate + freeze in a sync scope that never crosses an await.
        // The arena is created, used, and dropped here — so the result
        // (FrozenConfigs) is fully Send and the enclosing future stays Send.
        let frozen = {
            let arena = Arena::new();
            let mut cfgs = bytes.parse(&arena)?;
            for s in &mut cfgs["wireless"].sections {
                if let Some(mut device) = s.get_typed::<WifiDevice>()? {
                    device.disabled = false;
                    s.set(&device)?;
                }
                if let Some(mut iface) = s.get_typed::<WifiInterface>()? {
                    if iface.mode != WifiMode::AP {
                        continue;
                    }
                    iface.ssid = "StartWRT".into();
                    iface.encryption = "psk2".into();
                    iface.key = Some(password.to_string());
                    iface.dynamic_vlan = WifiDynamicVlan::ALLOWED;
                    if let Some(max) = max_stations {
                        iface.maxassoc = Some(max);
                    }
                    s.set(&iface)?;
                }
            }
            cfgs.freeze()
        };
        match uciedit::write_all(uci_root, frozen).await {
            Err(uciedit::Error::Conflict { .. }) if retries > 0 => {
                retries -= 1;
                continue;
            }
            Err(err) => return Err(err.into()),
            Ok(()) => return Ok(()),
        }
    }
}

/// Read a line from stdin with a visible prompt.
///
/// Uses stdout (not stderr) for the prompt and leaves echo enabled so the
/// operator can see what they type. This is intentional — these commands run
/// on a physical serial console in a manufacturing environment where hiding
/// the password provides no security benefit, and rpassword's termios
/// manipulation is unreliable on serial consoles through procd's askconsole.
fn read_line(prompt: &str) -> Result<String, Error> {
    print!("{prompt}");
    io::stdout()
        .flush()
        .map_err(|e| Error::new(eyre!("flush failed: {e}"), ErrorKind::Filesystem))?;
    let mut line = String::new();
    io::stdin()
        .lock()
        .read_line(&mut line)
        .map_err(|e| Error::new(eyre!("failed to read input: {e}"), ErrorKind::Filesystem))?;
    Ok(line.trim_end().to_string())
}

/// Prompt for the sticker password, validate, and confirm.
///
/// Returns the plaintext password. This performs no disk I/O — it's pure
/// terminal interaction.
pub fn prompt_password() -> Result<String, Error> {
    println!("Enter the password from the device sticker.");
    println!();

    let password = loop {
        let pw = read_line("Password: ")?;

        if let Err(msg) = validate_password(&pw) {
            println!("Invalid: {msg}");
            continue;
        }

        let confirm = read_line("Confirm password: ")?;

        if pw != confirm {
            println!("Passwords do not match. Try again.");
            continue;
        }

        break pw;
    };

    Ok(password)
}

/// Manufacturing initialization entry point.
///
/// Called directly from startwrt-cli when "init" is the first argument.
/// This is a blocking, synchronous function that creates a minimal tokio
/// runtime for async command execution.
pub fn run_init() -> Result<(), Error> {
    let rt = tokio::runtime::Runtime::new()
        .map_err(|e| Error::new(eyre!("failed to create tokio runtime: {e}"), ErrorKind::Filesystem))?;

    rt.block_on(async {
        // 1. Mount key_backup partition
        emmc::ensure_persistent_mounted().await?;

        // 2. Check if already initialized
        if emmc::is_initialized() {
            println!("This device has already been initialized.");
            return Ok(());
        }

        // 3. Banner
        println!();
        println!("========================================");
        println!("   StartWRT Device Initialization");
        println!("========================================");
        println!();

        // 4. Prompt, validate, confirm
        let password = prompt_password()?;

        // 5. Write password to key_backup partition
        emmc::write_password(&password).await?;

        // 6. Configure WiFi
        println!("Configuring WiFi...");
        configure_wifi("/etc/config", &password, None).await?;

        // 6b. Bootstrap Admin profile
        crate::profiles::bootstrap_admin_profile("/etc/config").await?;

        // 7. Reload WiFi
        let _ = crate::run_quiet_async(
            tokio::process::Command::new("wifi").arg("reload"),
        )
        .await;

        // 8. Success
        println!();
        println!("Initialization complete. WiFi SSID \"StartWRT\" is now active.");

        Ok(())
    })
}

/// Restore WiFi credentials from key_backup password at boot time.
///
/// Called by the daemon during startup to recover WiFi after a factory reset
/// (overlay wipe). Returns `Ok(true)` if WiFi was restored, `Ok(false)` if
/// no restore was needed.
pub async fn restore_wifi_if_needed() -> Result<bool, Error> {
    // 1. Mount key_backup if needed
    emmc::ensure_persistent_mounted().await?;

    // 2. Check if password exists on key_backup
    if !emmc::is_initialized() {
        return Ok(false);
    }

    // 3. Check if WiFi already has a key configured
    let arena = Arena::new();
    let cfgs = parse_all("/etc/config", &arena, &["wireless"]).await?;

    for s in &cfgs["wireless"].sections {
        if let Some(iface) = s.get_typed::<WifiInterface>()? {
            if iface.mode == WifiMode::AP && iface.key.is_some() {
                // WiFi already configured, no restore needed
                return Ok(false);
            }
        }
    }

    // 4. Read password and configure WiFi
    let password = emmc::read_password()?;
    configure_wifi("/etc/config", &password, None).await?;

    // 5. Reload WiFi
    let _ = crate::run_quiet_async(
        tokio::process::Command::new("wifi").arg("reload"),
    )
    .await;

    Ok(true)
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn valid_password_accepted() {
        // 12 chars from allowed set
        assert!(validate_password("AbCdEf234567").is_ok());
        assert!(validate_password("!@#$%^&*=+?2").is_ok());
        assert!(validate_password("HJKMNPQRSTUz").is_ok());
    }

    #[test]
    fn wrong_length_rejected() {
        assert!(validate_password("short").is_err());
        assert!(validate_password("waytoolongpassword").is_err());
        assert!(validate_password("").is_err());
    }

    #[test]
    fn ambiguous_chars_rejected() {
        // I, O, l, o, 0, 1 are not in the allowed charset
        assert!(validate_password("ABCDEFGHIJKL").is_err()); // I at position 8
        assert!(validate_password("ABCDEFGH0JKL").is_err()); // 0 (zero)
        assert!(validate_password("ABCDEFGHlJKL").is_err()); // l
        assert!(validate_password("ABCDEFGHJKLO").is_err()); // O at end
        assert!(validate_password("0BCDEFGHJKLM").is_err()); // 0
        assert!(validate_password("1BCDEFGHJKLM").is_err()); // 1
    }
}
