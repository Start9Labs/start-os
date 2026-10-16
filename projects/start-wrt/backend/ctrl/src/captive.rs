use std::fs;
use std::path::Path;
use std::process::Stdio;

use tokio::io::AsyncWriteExt;

use crate::prelude::*;

const CAPTIVE_CONF_NAME: &str = "captive-portal.conf";
const CAPTIVE_CONTENT: &str = "\
    address=/#/192.168.0.1\n\
    address=/#/::\n\
    dhcp-option=114,http://192.168.0.1/\n";

/// Outside `inet fw4`, which fw4 reloads replace.
const BLOCK_RULESET: &str = "\
table inet startwrt_captive
delete table inet startwrt_captive
table inet startwrt_captive {
\tchain forward {
\t\ttype filter hook forward priority filter - 10; policy accept;
\t\tmeta l4proto tcp reject with tcp reset
\t\treject with icmpx type admin-prohibited
\t}
\tchain dstnat {
\t\ttype nat hook prerouting priority dstnat - 10; policy accept;
\t\tmeta l4proto { tcp, udp } th dport 53 redirect to :53
\t}
}
";

const UNBLOCK_RULESET: &str = "\
table inet startwrt_captive
delete table inet startwrt_captive
";

/// Find the dnsmasq instance conf-dir from the generated config.
///
/// OpenWrt generates per-instance conf-dirs like `/tmp/dnsmasq.cfg01411c.d/`
/// (named after the UCI section hash) rather than a plain `/tmp/dnsmasq.d/`.
/// Parse the generated config in `/var/etc/` to discover the actual path.
fn find_dnsmasq_conf_dir() -> Result<String, Error> {
    let var_etc = Path::new("/var/etc");
    for entry in fs::read_dir(var_etc)
        .map_err(|e| Error::new(eyre!("failed to read /var/etc: {e}"), ErrorKind::Filesystem))?
    {
        let entry = entry.map_err(|e| Error::new(eyre!("readdir: {e}"), ErrorKind::Filesystem))?;
        let name = entry.file_name();
        if !name.to_string_lossy().starts_with("dnsmasq.conf.") {
            continue;
        }
        let contents = fs::read_to_string(entry.path()).map_err(|e| {
            Error::new(
                eyre!("read {}: {e}", entry.path().display()),
                ErrorKind::Filesystem,
            )
        })?;
        for line in contents.lines() {
            if let Some(dir) = line.strip_prefix("conf-dir=") {
                return Ok(dir.to_string());
            }
        }
    }
    Err(Error::new(
        eyre!("no dnsmasq conf-dir found in /var/etc/dnsmasq.conf.*"),
        ErrorKind::NotFound,
    ))
}

/// Check if the captive portal DNS hijack is currently active.
pub fn is_captive_portal_active() -> bool {
    find_dnsmasq_conf_dir()
        .map(|dir| Path::new(&dir).join(CAPTIVE_CONF_NAME).exists())
        .unwrap_or(false)
}

pub async fn enable_captive_portal() -> Result<(), Error> {
    apply_ruleset(BLOCK_RULESET).await?;
    enable_dns_hijack().await
}

/// Unblocks forwarding even when restoring DNS fails.
pub async fn disable_captive_portal() -> Result<(), Error> {
    let unblocked = apply_ruleset(UNBLOCK_RULESET).await;
    disable_dns_hijack().await?;
    unblocked
}

async fn enable_dns_hijack() -> Result<(), Error> {
    let dir = find_dnsmasq_conf_dir()?;
    let conf = Path::new(&dir).join(CAPTIVE_CONF_NAME);

    if !Path::new(&dir).exists() {
        fs::create_dir_all(&dir)
            .map_err(|e| Error::new(eyre!("failed to create {dir}: {e}"), ErrorKind::Filesystem))?;
    }

    fs::write(&conf, CAPTIVE_CONTENT).map_err(|e| {
        Error::new(
            eyre!("failed to write {}: {e}", conf.display()),
            ErrorKind::Filesystem,
        )
    })?;

    restart_dnsmasq().await
}

async fn disable_dns_hijack() -> Result<(), Error> {
    let dir = find_dnsmasq_conf_dir()?;
    let conf = Path::new(&dir).join(CAPTIVE_CONF_NAME);

    if conf.exists() {
        fs::remove_file(&conf).map_err(|e| {
            Error::new(
                eyre!("failed to remove {}: {e}", conf.display()),
                ErrorKind::Filesystem,
            )
        })?;
    }

    restart_dnsmasq().await
}

/// Check if the admin (root) password has been set in /etc/shadow.
///
/// Synchronous version of the check in auth.rs — reads /etc/shadow directly.
pub fn is_admin_password_set() -> Result<bool, Error> {
    let shadow = fs::read_to_string("/etc/shadow").map_err(|e| {
        Error::new(
            eyre!("failed to read /etc/shadow: {e}"),
            ErrorKind::Filesystem,
        )
    })?;

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
    let password_set = is_admin_password_set()?;

    if password_set {
        apply_ruleset(UNBLOCK_RULESET).await?;
        if is_captive_portal_active() {
            tracing::info!("admin password set, disabling captive portal");
            disable_dns_hijack().await?;
        }
    } else {
        apply_ruleset(BLOCK_RULESET).await?;
        if !is_captive_portal_active() {
            tracing::info!("no admin password set, enabling captive portal");
            enable_dns_hijack().await?;
        }
    }

    Ok(())
}

async fn apply_ruleset(ruleset: &str) -> Result<(), Error> {
    let nft_err = |e: std::io::Error| Error::new(eyre!("nft: {e}"), ErrorKind::Network);
    let mut child = tokio::process::Command::new("nft")
        .args(["-f", "-"])
        .stdin(Stdio::piped())
        .stdout(Stdio::null())
        .stderr(Stdio::piped())
        .spawn()
        .map_err(nft_err)?;
    if let Some(mut stdin) = child.stdin.take() {
        stdin.write_all(ruleset.as_bytes()).await.map_err(nft_err)?;
    }
    let output = child.wait_with_output().await.map_err(nft_err)?;
    if !output.status.success() {
        return Err(Error::new(
            eyre!(
                "nft exited {}: {}",
                output.status,
                String::from_utf8_lossy(&output.stderr).trim()
            ),
            ErrorKind::Network,
        ));
    }
    Ok(())
}

async fn restart_dnsmasq() -> Result<(), Error> {
    let status =
        crate::run_quiet_async(tokio::process::Command::new("/etc/init.d/dnsmasq").arg("restart"))
            .await
            .map_err(|e| Error::new(eyre!("failed to restart dnsmasq: {e}"), ErrorKind::Network))?;

    if !status.success() {
        return Err(Error::new(
            eyre!(
                "dnsmasq restart failed (exit {})",
                status.code().unwrap_or(-1)
            ),
            ErrorKind::Network,
        ));
    }
    Ok(())
}
