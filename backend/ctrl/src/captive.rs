use std::fs;
use std::path::Path;

use crate::Error;

const CAPTIVE_CONF_NAME: &str = "captive-portal.conf";
const CAPTIVE_CONTENT: &str = "\
    address=/#/192.168.0.1\n\
    address=/#/::\n\
    dhcp-option=114,http://192.168.0.1/\n";

/// Find the dnsmasq instance conf-dir from the generated config.
///
/// OpenWrt generates per-instance conf-dirs like `/tmp/dnsmasq.cfg01411c.d/`
/// (named after the UCI section hash) rather than a plain `/tmp/dnsmasq.d/`.
/// Parse the generated config in `/var/etc/` to discover the actual path.
fn find_dnsmasq_conf_dir() -> Result<String, Error> {
    let var_etc = Path::new("/var/etc");
    for entry in fs::read_dir(var_etc)
        .map_err(|e| Error::other(format!("failed to read /var/etc: {e}")))?
    {
        let entry = entry.map_err(|e| Error::other(format!("readdir: {e}")))?;
        let name = entry.file_name();
        if !name.to_string_lossy().starts_with("dnsmasq.conf.") {
            continue;
        }
        let contents = fs::read_to_string(entry.path())
            .map_err(|e| Error::other(format!("read {}: {e}", entry.path().display())))?;
        for line in contents.lines() {
            if let Some(dir) = line.strip_prefix("conf-dir=") {
                return Ok(dir.to_string());
            }
        }
    }
    Err(Error::other(
        "no dnsmasq conf-dir found in /var/etc/dnsmasq.conf.*",
    ))
}

/// Check if the captive portal DNS hijack is currently active.
pub fn is_captive_portal_active() -> bool {
    find_dnsmasq_conf_dir()
        .map(|dir| Path::new(&dir).join(CAPTIVE_CONF_NAME).exists())
        .unwrap_or(false)
}

/// Enable captive portal DNS hijacking.
///
/// Drops a dnsmasq conf file that redirects all DNS queries to the router's
/// IP, then restarts dnsmasq to pick up the change.
pub fn enable_captive_portal() -> Result<(), Error> {
    let dir = find_dnsmasq_conf_dir()?;
    let conf = Path::new(&dir).join(CAPTIVE_CONF_NAME);

    if !Path::new(&dir).exists() {
        fs::create_dir_all(&dir)
            .map_err(|e| Error::other(format!("failed to create {dir}: {e}")))?;
    }

    fs::write(&conf, CAPTIVE_CONTENT)
        .map_err(|e| Error::other(format!("failed to write {}: {e}", conf.display())))?;

    restart_dnsmasq()
}

/// Disable captive portal DNS hijacking.
///
/// Removes the dnsmasq conf file and restarts dnsmasq to resume normal DNS.
pub fn disable_captive_portal() -> Result<(), Error> {
    let dir = find_dnsmasq_conf_dir()?;
    let conf = Path::new(&dir).join(CAPTIVE_CONF_NAME);

    if conf.exists() {
        fs::remove_file(&conf)
            .map_err(|e| Error::other(format!("failed to remove {}: {e}", conf.display())))?;
    }

    restart_dnsmasq()
}

/// Check if the admin (root) password has been set in /etc/shadow.
///
/// Synchronous version of the check in auth.rs — reads /etc/shadow directly.
pub fn is_admin_password_set() -> Result<bool, Error> {
    let shadow = fs::read_to_string("/etc/shadow")
        .map_err(|e| Error::other(format!("failed to read /etc/shadow: {e}")))?;

    for line in shadow.lines() {
        let parts: Vec<&str> = line.split(':').collect();
        if parts.len() >= 2 && parts[0] == "root" {
            let hash = parts[1];
            // Empty, *, !, or x means no password set
            if hash.is_empty() || hash == "*" || hash == "!" || hash == "x" {
                return Ok(false);
            }
            return Ok(true);
        }
    }

    Ok(false)
}

/// Evaluate current state and enable/disable captive portal accordingly.
///
/// - If no admin password is set → enable captive portal
/// - If admin password is set → disable captive portal
///
/// This is an async wrapper for use from the daemon's boot sequence.
pub async fn ensure_captive_portal_state() -> Result<(), Error> {
    tokio::task::spawn_blocking(|| {
        let password_set = is_admin_password_set()?;

        if password_set {
            if is_captive_portal_active() {
                tracing::info!("admin password set, disabling captive portal");
                disable_captive_portal()?;
            }
        } else {
            if !is_captive_portal_active() {
                tracing::info!("no admin password set, enabling captive portal");
                enable_captive_portal()?;
            }
        }

        Ok(())
    })
    .await
    .map_err(|e| Error::other(format!("captive portal task panicked: {e}")))?
}

fn restart_dnsmasq() -> Result<(), Error> {
    let status = crate::run_quiet(std::process::Command::new("/etc/init.d/dnsmasq").arg("restart"))
        .map_err(|e| Error::other(format!("failed to restart dnsmasq: {e}")))?;

    if !status.success() {
        return Err(Error::other(format!(
            "dnsmasq restart failed (exit {})",
            status.code().unwrap_or(-1)
        )));
    }
    Ok(())
}
