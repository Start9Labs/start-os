use crate::error::{Error, ErrorKind};
use crate::utils::{DeserializeStdin, HandlerExtSerde as _};
use crate::{CliContext, CtrlContext, ServerContext};
use chrono::Utc;
use imbl_value::Value;
use nix::sys::signal::{self, Signal};
use nix::unistd::Pid;
use rpc_toolkit::{from_fn, from_fn_async, HandlerExt, ParentHandler};
use serde::{Deserialize, Serialize};
use std::collections::HashSet;
use std::io;
use std::net::{Ipv4Addr, Ipv6Addr};
use std::process::Command as StdCommand;
use std::{fs, path::Path};
use tokio::process::Command;
use tracing::instrument;
use uciedit::openwrt::{FirewallRule, FirewallTarget};
use uciedit::{dump_all, parse_all, Arena, Configs, TypedSection};

#[derive(Debug, TypedSection, Default)]
#[uci(ty = "preferences")]
pub(crate) struct UciPreferences {
    #[uci(default_value = "\"en_US\".to_string()")]
    pub language: String,
    #[uci(default_value = "\"system\".to_string()")]
    pub theme: String,
    #[uci(default_value = "\"default\".to_string()")]
    pub remote_access: String,
}

#[derive(Debug, Serialize, Deserialize)]
#[serde(rename_all = "camelCase")]
struct SystemInfoResponse {
    version: String,
    language: String,
    date: String,
    theme: String,
    remote_access: String,
}

#[derive(Debug, Serialize, Deserialize)]
#[serde(rename_all = "camelCase")]
struct VersionInfo {
    version: String,
    release_notes: String,
}

#[derive(Debug, Serialize, Deserialize)]
#[serde(rename_all = "camelCase")]
struct SetPreferencesReq {
    language: Option<String>,
    theme: Option<String>,
    remote_access: Option<String>,
}

fn info<C: CtrlContext>(ctx: C) -> Result<SystemInfoResponse, Error> {
    let arena = Arena::new();
    let cfgs = parse_all(ctx.uci_root(), &arena, &["startwrt"])?;

    let mut prefs = UciPreferences::default();
    cfgs["startwrt"].try_each(|name, p: UciPreferences| {
        if name == Some("preferences") {
            prefs = p;
        }
        Ok::<_, Error>(())
    })?;

    Ok(SystemInfoResponse {
        version: env!("CARGO_PKG_VERSION").to_string(),
        language: prefs.language,
        date: Utc::now().to_rfc3339(),
        theme: prefs.theme,
        remote_access: prefs.remote_access,
    })
}

fn newer_versions<C: CtrlContext>(_ctx: C) -> Result<Vec<VersionInfo>, Error> {
    // TODO: implement update check against remote server
    Ok(Vec::new())
}

const VALID_LANGUAGES: &[&str] = &["en_US", "es_ES", "de_DE", "fr_FR", "pl_PL"];
const VALID_THEMES: &[&str] = &["dark", "light", "system"];
const VALID_REMOTE_ACCESS: &[&str] = &["default", "never", "always"];

const REMOTE_RULE_PREFIX: &str = "startwrt_remote_";
const REMOTE_ACCESS_PORTS: &[(&str, &str)] = &[
    ("startwrt_remote_80", "80"),
    ("startwrt_remote_22", "22"),
    ("startwrt_remote_8080", "8080"),
    ("startwrt_remote_8443", "8443"),
];

fn is_private_ipv4(ip: &Ipv4Addr) -> bool {
    let octets = ip.octets();
    // RFC1918 only — excludes CGNAT 100.64.0.0/10
    octets[0] == 10
        || (octets[0] == 172 && (16..=31).contains(&octets[1]))
        || (octets[0] == 192 && octets[1] == 168)
}

fn has_global_ipv6(addrs: &[Ipv6Addr]) -> bool {
    addrs.iter().any(|addr| {
        // 2000::/3 — first 3 bits are 001
        let first = addr.segments()[0];
        (0x2000..=0x3fff).contains(&first)
    })
}

fn get_wan_ipv4() -> Result<Option<Ipv4Addr>, Error> {
    let output = StdCommand::new("ubus")
        .args(["call", "network.interface.wan", "status"])
        .output()
        .map_err(|e| Error::other(format!("failed to call ubus for wan: {e}")))?;
    if !output.status.success() {
        return Ok(None);
    }
    let json: serde_json::Value = match serde_json::from_slice(&output.stdout) {
        Ok(v) => v,
        Err(_) => return Ok(None),
    };
    let addr_str = json
        .pointer("/ipv4-address/0/address")
        .and_then(|v| v.as_str());
    match addr_str {
        Some(s) => Ok(s.parse().ok()),
        None => Ok(None),
    }
}

pub fn get_wan_ipv6s() -> Result<Vec<Ipv6Addr>, Error> {
    let output = StdCommand::new("ubus")
        .args(["call", "network.interface.wan6", "status"])
        .output()
        .map_err(|e| Error::other(format!("failed to call ubus for wan6: {e}")))?;
    if !output.status.success() {
        return Ok(Vec::new());
    }
    let json: serde_json::Value = match serde_json::from_slice(&output.stdout) {
        Ok(v) => v,
        Err(_) => return Ok(Vec::new()),
    };
    let mut addrs = Vec::new();
    if let Some(arr) = json.get("ipv6-address").and_then(|v| v.as_array()) {
        for entry in arr {
            if let Some(s) = entry.get("address").and_then(|v| v.as_str()) {
                if let Ok(addr) = s.parse() {
                    addrs.push(addr);
                }
            }
        }
    }
    // Also check prefix-assignment local addresses (DHCPv6-PD)
    if let Some(arr) = json.get("ipv6-prefix-assignment").and_then(|v| v.as_array()) {
        for entry in arr {
            if let Some(s) = entry
                .pointer("/local-address/address")
                .and_then(|v| v.as_str())
            {
                if let Ok(addr) = s.parse() {
                    addrs.push(addr);
                }
            }
        }
    }
    Ok(addrs)
}

fn apply_remote_access_config(
    cfgs: &mut Configs,
    mode: &str,
    wan_ipv4: Option<Ipv4Addr>,
    wan_ipv6s: &[Ipv6Addr],
) {
    // Remove all existing remote access rules
    cfgs["firewall"].sections.retain(|section| {
        let Ok(rule) = section.get::<FirewallRule>() else {
            return true;
        };
        !rule.name.starts_with(REMOTE_RULE_PREFIX)
    });

    if mode == "never" {
        return;
    }

    let family: Option<Option<String>> = match mode {
        "always" => Some(None), // both families
        "default" => {
            let v4_private = wan_ipv4.map_or(false, |ip| is_private_ipv4(&ip));
            let v6_private = !wan_ipv6s.is_empty() && !has_global_ipv6(wan_ipv6s);
            match (v4_private, v6_private) {
                (true, true) => Some(None),               // both families
                (true, false) => Some(Some("ipv4".into())), // IPv4 only
                (false, true) => Some(Some("ipv6".into())), // IPv6 only
                (false, false) => None,                    // no rules
            }
        }
        _ => None,
    };

    let Some(family) = family else { return };

    for &(name, port) in REMOTE_ACCESS_PORTS {
        let rule = FirewallRule {
            name: name.to_string(),
            src: "wan".to_string(),
            dest_port: Some(port.to_string()),
            proto: vec!["tcp".to_string()],
            target: FirewallTarget::ACCEPT,
            family: family.clone(),
            ..Default::default()
        };
        cfgs["firewall"].append(&rule, Some(name)).ok();
    }
}

/// Convert an IPv6 address to the hex format used in `/proc/net/tcp6`.
/// Each 32-bit group is stored in little-endian byte order.
fn ipv6_to_proc_hex(addr: Ipv6Addr) -> String {
    let octets = addr.octets();
    let mut hex = String::with_capacity(32);
    // /proc/net/tcp6 stores the address as 4 little-endian 32-bit words
    for chunk in octets.chunks(4) {
        for &byte in chunk.iter().rev() {
            hex.push_str(&format!("{byte:02X}"));
        }
    }
    hex
}

/// Parse `/proc/net/tcp` and collect socket inodes for ESTABLISHED connections
/// on port 22 where the local address matches the given WAN IPv4.
fn collect_tcp4_ssh_inodes(wan_ip: Ipv4Addr, inodes: &mut HashSet<u64>) -> io::Result<()> {
    let content = fs::read_to_string("/proc/net/tcp")?;
    let wan_hex = {
        let octets = wan_ip.octets();
        format!(
            "{:02X}{:02X}{:02X}{:02X}",
            octets[3], octets[2], octets[1], octets[0]
        )
    };
    let local_match = format!("{wan_hex}:0016"); // port 22 in hex

    for line in content.lines().skip(1) {
        let fields: Vec<&str> = line.split_whitespace().collect();
        if fields.len() < 10 {
            continue;
        }
        // fields[1] = local_address:port, fields[3] = connection state
        // state "01" = ESTABLISHED
        if fields[3] != "01" {
            continue;
        }
        if fields[1].eq_ignore_ascii_case(&local_match) {
            if let Ok(inode) = fields[9].parse::<u64>() {
                if inode != 0 {
                    inodes.insert(inode);
                }
            }
        }
    }
    Ok(())
}

/// Parse `/proc/net/tcp6` and collect socket inodes for ESTABLISHED connections
/// on port 22 where the local address matches the given WAN IPv6.
fn collect_tcp6_ssh_inodes(wan_ip: Ipv6Addr, inodes: &mut HashSet<u64>) -> io::Result<()> {
    let content = fs::read_to_string("/proc/net/tcp6")?;
    let wan_hex = ipv6_to_proc_hex(wan_ip);
    let local_match = format!("{wan_hex}:0016"); // port 22 in hex

    for line in content.lines().skip(1) {
        let fields: Vec<&str> = line.split_whitespace().collect();
        if fields.len() < 10 {
            continue;
        }
        if fields[3] != "01" {
            continue;
        }
        if fields[1].eq_ignore_ascii_case(&local_match) {
            if let Ok(inode) = fields[9].parse::<u64>() {
                if inode != 0 {
                    inodes.insert(inode);
                }
            }
        }
    }
    Ok(())
}

/// Scan all `/proc/<pid>/fd/` entries for socket inodes matching the given set.
/// If a match is found and the process is `dropbear`, send SIGTERM.
fn kill_dropbear_by_inodes(inodes: &HashSet<u64>) {
    let proc = Path::new("/proc");
    let Ok(entries) = fs::read_dir(proc) else {
        return;
    };
    for entry in entries.flatten() {
        let name = entry.file_name();
        let Some(pid_str) = name.to_str() else {
            continue;
        };
        let Ok(pid_num) = pid_str.parse::<i32>() else {
            continue;
        };

        let fd_dir = proc.join(pid_str).join("fd");
        let Ok(fds) = fs::read_dir(&fd_dir) else {
            continue;
        };

        let mut matched = false;
        for fd in fds.flatten() {
            let Ok(link) = fs::read_link(fd.path()) else {
                continue;
            };
            let link_str = link.to_string_lossy();
            // symlink target looks like "socket:[12345]"
            if let Some(inode_str) = link_str.strip_prefix("socket:[").and_then(|s| s.strip_suffix(']')) {
                if let Ok(inode) = inode_str.parse::<u64>() {
                    if inodes.contains(&inode) {
                        matched = true;
                        break;
                    }
                }
            }
        }

        if matched {
            // Verify this is a dropbear process before killing
            let comm_path = proc.join(pid_str).join("comm");
            if let Ok(comm) = fs::read_to_string(&comm_path) {
                if comm.trim() == "dropbear" {
                    let _ = signal::kill(Pid::from_raw(pid_num), Signal::SIGTERM);
                }
            }
        }
    }
}

/// Kill WAN-side SSH (dropbear) sessions by finding their socket inodes in
/// `/proc/net/tcp{,6}` and matching them to process FDs.
fn kill_wan_ssh_sessions(wan_ipv4: Option<Ipv4Addr>, wan_ipv6s: &[Ipv6Addr]) {
    if wan_ipv4.is_none() && wan_ipv6s.is_empty() {
        return;
    }

    let mut inodes = HashSet::new();
    if let Some(ip) = wan_ipv4 {
        let _ = collect_tcp4_ssh_inodes(ip, &mut inodes);
    }
    for &ip in wan_ipv6s {
        let _ = collect_tcp6_ssh_inodes(ip, &mut inodes);
    }

    if !inodes.is_empty() {
        kill_dropbear_by_inodes(&inodes);
    }
}

fn reload_firewall(wan_ipv4: Option<Ipv4Addr>, wan_ipv6s: Vec<Ipv6Addr>) {
    std::thread::spawn(move || {
        let _ = StdCommand::new("/etc/init.d/firewall")
            .arg("reload")
            .spawn()
            .and_then(|mut c| c.wait());
        // Kill WAN-side SSH sessions so they don't survive firewall changes
        // via conntrack ESTABLISHED state. HTTP sessions disconnect naturally
        // on the next request. Only dropbear processes are killed.
        kill_wan_ssh_sessions(wan_ipv4, &wan_ipv6s);
    });
}

pub fn apply_remote_access<C: CtrlContext>(ctx: C) -> Result<Value, Error> {
    let mut retries = 4;
    loop {
        let arena = Arena::new();
        let mut cfgs = parse_all(ctx.uci_root(), &arena, &["startwrt", "firewall"])?;

        let mut prefs = UciPreferences::default();
        cfgs["startwrt"].try_each(|name, p: UciPreferences| {
            if name == Some("preferences") {
                prefs = p;
            }
            Ok::<_, Error>(())
        })?;

        let (wan_ipv4, wan_ipv6s) = if ctx.effectful() {
            (get_wan_ipv4()?, get_wan_ipv6s()?)
        } else {
            (None, Vec::new())
        };

        apply_remote_access_config(&mut cfgs, &prefs.remote_access, wan_ipv4, &wan_ipv6s);

        match dump_all(ctx.uci_root(), cfgs) {
            Err(uciedit::Error::Conflict { .. }) if retries > 0 => {
                retries -= 1;
                continue;
            }
            Err(err) => return Err(err.into()),
            Ok(()) => {
                if ctx.effectful() {
                    reload_firewall(wan_ipv4, wan_ipv6s);
                }
                return Ok(Value::Null);
            }
        }
    }
}

fn set_preferences<C: CtrlContext>(
    ctx: C,
    DeserializeStdin(req): DeserializeStdin<SetPreferencesReq>,
) -> Result<Value, Error> {
    if let Some(lang) = &req.language {
        if !VALID_LANGUAGES.contains(&lang.as_str()) {
            return Err(ErrorKind::InvalidValue {
                field: "language".to_string(),
                value: lang.clone(),
            }
            .into());
        }
    }
    if let Some(theme) = &req.theme {
        if !VALID_THEMES.contains(&theme.as_str()) {
            return Err(ErrorKind::InvalidValue {
                field: "theme".to_string(),
                value: theme.clone(),
            }
            .into());
        }
    }
    if let Some(ra) = &req.remote_access {
        if !VALID_REMOTE_ACCESS.contains(&ra.as_str()) {
            return Err(ErrorKind::InvalidValue {
                field: "remoteAccess".to_string(),
                value: ra.clone(),
            }
            .into());
        }
    }

    let needs_firewall = req.remote_access.is_some();
    let configs: &[&str] = if needs_firewall {
        &["startwrt", "firewall"]
    } else {
        &["startwrt"]
    };

    let mut retries = 4;
    loop {
        let arena = Arena::new();
        let mut cfgs = parse_all(ctx.uci_root(), &arena, configs)?;

        // Find existing preferences section or create one
        let mut found = false;
        let mut new_mode = None;
        for section in &mut cfgs["startwrt"].sections {
            if section.name().as_deref() == Some("preferences") {
                if let Some(mut prefs) = section.get_typed::<UciPreferences>()? {
                    if let Some(lang) = &req.language {
                        prefs.language = lang.clone();
                    }
                    if let Some(theme) = &req.theme {
                        prefs.theme = theme.clone();
                    }
                    if let Some(ra) = &req.remote_access {
                        new_mode = Some(ra.clone());
                        prefs.remote_access = ra.clone();
                    }
                    section.set(&prefs)?;
                    found = true;
                    break;
                }
            }
        }

        if !found {
            let ra = req
                .remote_access
                .clone()
                .unwrap_or_else(|| "default".to_string());
            if req.remote_access.is_some() {
                new_mode = Some(ra.clone());
            }
            let prefs = UciPreferences {
                language: req.language.clone().unwrap_or_else(|| "en_US".to_string()),
                theme: req.theme.clone().unwrap_or_else(|| "system".to_string()),
                remote_access: ra,
            };
            cfgs["startwrt"].append(&prefs, Some("preferences"))?;
        }

        // Apply firewall rules if remote_access changed
        if let Some(mode) = &new_mode {
            let (wan_ipv4, wan_ipv6s) = if ctx.effectful() {
                (get_wan_ipv4()?, get_wan_ipv6s()?)
            } else {
                (None, Vec::new())
            };
            apply_remote_access_config(&mut cfgs, mode, wan_ipv4, &wan_ipv6s);
        }

        match dump_all(ctx.uci_root(), cfgs) {
            Err(uciedit::Error::Conflict { .. }) if retries > 0 => {
                retries -= 1;
                continue;
            }
            Err(err) => return Err(err.into()),
            Ok(()) => {
                if new_mode.is_some() && ctx.effectful() {
                    let (wan_ipv4, wan_ipv6s) = (get_wan_ipv4()?, get_wan_ipv6s()?);
                    reload_firewall(wan_ipv4, wan_ipv6s);
                }
                return Ok(Value::Null);
            }
        }
    }
}

#[instrument(skip_all)]
async fn factory_reset(_ctx: ServerContext) -> Result<(), Error> {
    let output = Command::new("firstboot")
        .arg("-y")
        .output()
        .await
        .map_err(|e| Error::other(format!("failed to run firstboot: {e}")))?;

    if !output.status.success() {
        let stderr = String::from_utf8_lossy(&output.stderr);
        return Err(Error::other(format!("firstboot failed: {stderr}")));
    }

    // Spawn reboot after a short delay so the HTTP response can reach the client
    tokio::spawn(async {
        tokio::time::sleep(std::time::Duration::from_secs(1)).await;
        if let Err(e) = Command::new("reboot").status().await {
            tracing::error!("failed to reboot: {e}");
        }
    });

    Ok(())
}

pub fn system<C: CtrlContext>() -> ParentHandler<C> {
    ParentHandler::new()
        .subcommand(
            "info",
            from_fn(info::<C>)
                .with_metadata("no_auth", Value::Bool(true))
                .with_display_serializable(),
        )
        .subcommand(
            "newer-versions",
            from_fn(newer_versions::<C>)
                .with_metadata("no_auth", Value::Bool(true))
                .with_display_serializable(),
        )
        .subcommand(
            "set-preferences",
            from_fn(set_preferences::<C>).with_display_serializable(),
        )
        .subcommand(
            "apply-remote-access",
            from_fn(apply_remote_access::<C>)
                .with_metadata("no_auth", Value::Bool(true))
                .no_display(),
        )
        .subcommand(
            "factory-reset",
            from_fn_async(factory_reset)
                .no_display()
                .with_about("Wipe overlay and reboot (factory reset)")
                .with_call_remote::<CliContext>(),
        )
        .subcommand(
            "logs",
            from_fn_async(crate::logs::get_logs)
                .with_display_serializable()
                .with_call_remote::<CliContext>(),
        )
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_is_private_ipv4() {
        // RFC1918 ranges
        assert!(is_private_ipv4(&"10.0.0.1".parse().unwrap()));
        assert!(is_private_ipv4(&"10.255.255.255".parse().unwrap()));
        assert!(is_private_ipv4(&"172.16.0.1".parse().unwrap()));
        assert!(is_private_ipv4(&"172.31.255.255".parse().unwrap()));
        assert!(is_private_ipv4(&"192.168.0.1".parse().unwrap()));
        assert!(is_private_ipv4(&"192.168.255.255".parse().unwrap()));

        // Not private
        assert!(!is_private_ipv4(&"8.8.8.8".parse().unwrap()));
        assert!(!is_private_ipv4(&"100.64.0.1".parse().unwrap())); // CGNAT
        assert!(!is_private_ipv4(&"172.32.0.1".parse().unwrap()));
        assert!(!is_private_ipv4(&"1.2.3.4".parse().unwrap()));
    }

    #[test]
    fn test_has_global_ipv6() {
        let gua: Ipv6Addr = "2001:db8::1".parse().unwrap();
        let ula: Ipv6Addr = "fd00::1".parse().unwrap();
        let link_local: Ipv6Addr = "fe80::1".parse().unwrap();

        assert!(has_global_ipv6(&[gua]));
        assert!(has_global_ipv6(&[ula, gua]));
        assert!(!has_global_ipv6(&[ula]));
        assert!(!has_global_ipv6(&[link_local]));
        assert!(!has_global_ipv6(&[]));
    }

    fn setup_firewall_config(dir: &std::path::Path, startwrt_content: &str) {
        std::fs::write(dir.join("startwrt"), startwrt_content).unwrap();
        std::fs::write(
            dir.join("firewall"),
            "\
config zone wan
\toption name 'wan'
\toption input 'REJECT'
\toption output 'ACCEPT'
\toption forward 'REJECT'
",
        )
        .unwrap();
    }

    fn count_remote_rules(dir: &std::path::Path) -> usize {
        let arena = Arena::new();
        let cfgs = parse_all(dir, &arena, &["firewall"]).unwrap();
        cfgs["firewall"]
            .sections
            .iter()
            .filter(|s| {
                s.get::<FirewallRule>()
                    .ok()
                    .map_or(false, |r| r.name.starts_with(REMOTE_RULE_PREFIX))
            })
            .count()
    }

    fn get_remote_rule_families(dir: &std::path::Path) -> Vec<Option<String>> {
        let arena = Arena::new();
        let cfgs = parse_all(dir, &arena, &["firewall"]).unwrap();
        cfgs["firewall"]
            .sections
            .iter()
            .filter_map(|s| {
                let rule = s.get::<FirewallRule>().ok()?;
                if rule.name.starts_with(REMOTE_RULE_PREFIX) {
                    Some(rule.family.clone())
                } else {
                    None
                }
            })
            .collect()
    }

    #[test]
    fn test_apply_always() {
        let dir = tempfile::tempdir().unwrap();
        setup_firewall_config(
            dir.path(),
            "config preferences preferences\n\toption remote_access 'always'\n",
        );

        let arena = Arena::new();
        let mut cfgs = parse_all(dir.path(), &arena, &["startwrt", "firewall"]).unwrap();
        apply_remote_access_config(&mut cfgs, "always", None, &[]);
        dump_all(dir.path(), cfgs).unwrap();

        assert_eq!(count_remote_rules(dir.path()), 4);
        // All rules should have no family restriction
        for family in get_remote_rule_families(dir.path()) {
            assert_eq!(family, None);
        }
    }

    #[test]
    fn test_apply_never() {
        let dir = tempfile::tempdir().unwrap();
        setup_firewall_config(
            dir.path(),
            "config preferences preferences\n\toption remote_access 'never'\n",
        );

        // First add some rules
        let arena = Arena::new();
        let mut cfgs = parse_all(dir.path(), &arena, &["startwrt", "firewall"]).unwrap();
        apply_remote_access_config(&mut cfgs, "always", None, &[]);
        dump_all(dir.path(), cfgs).unwrap();
        assert_eq!(count_remote_rules(dir.path()), 4);

        // Now apply "never" — should remove them all
        let arena = Arena::new();
        let mut cfgs = parse_all(dir.path(), &arena, &["startwrt", "firewall"]).unwrap();
        apply_remote_access_config(&mut cfgs, "never", None, &[]);
        dump_all(dir.path(), cfgs).unwrap();
        assert_eq!(count_remote_rules(dir.path()), 0);
    }

    #[test]
    fn test_apply_default_ipv4_private_ipv6_public() {
        let dir = tempfile::tempdir().unwrap();
        setup_firewall_config(
            dir.path(),
            "config preferences preferences\n\toption remote_access 'default'\n",
        );

        let wan_ipv4: Ipv4Addr = "192.168.1.1".parse().unwrap();
        let wan_ipv6: Ipv6Addr = "2001:db8::1".parse().unwrap();

        let arena = Arena::new();
        let mut cfgs = parse_all(dir.path(), &arena, &["startwrt", "firewall"]).unwrap();
        apply_remote_access_config(&mut cfgs, "default", Some(wan_ipv4), &[wan_ipv6]);
        dump_all(dir.path(), cfgs).unwrap();

        assert_eq!(count_remote_rules(dir.path()), 4);
        for family in get_remote_rule_families(dir.path()) {
            assert_eq!(family, Some("ipv4".to_string()));
        }
    }

    #[test]
    fn test_apply_default_both_private() {
        let dir = tempfile::tempdir().unwrap();
        setup_firewall_config(
            dir.path(),
            "config preferences preferences\n\toption remote_access 'default'\n",
        );

        let wan_ipv4: Ipv4Addr = "10.0.0.1".parse().unwrap();
        let wan_ipv6: Ipv6Addr = "fd00::1".parse().unwrap();

        let arena = Arena::new();
        let mut cfgs = parse_all(dir.path(), &arena, &["startwrt", "firewall"]).unwrap();
        apply_remote_access_config(&mut cfgs, "default", Some(wan_ipv4), &[wan_ipv6]);
        dump_all(dir.path(), cfgs).unwrap();

        assert_eq!(count_remote_rules(dir.path()), 4);
        for family in get_remote_rule_families(dir.path()) {
            assert_eq!(family, None);
        }
    }

    #[test]
    fn test_apply_default_both_public() {
        let dir = tempfile::tempdir().unwrap();
        setup_firewall_config(
            dir.path(),
            "config preferences preferences\n\toption remote_access 'default'\n",
        );

        let wan_ipv4: Ipv4Addr = "8.8.8.8".parse().unwrap();
        let wan_ipv6: Ipv6Addr = "2001:db8::1".parse().unwrap();

        let arena = Arena::new();
        let mut cfgs = parse_all(dir.path(), &arena, &["startwrt", "firewall"]).unwrap();
        apply_remote_access_config(&mut cfgs, "default", Some(wan_ipv4), &[wan_ipv6]);
        dump_all(dir.path(), cfgs).unwrap();

        // Both public — no rules should be created
        assert_eq!(count_remote_rules(dir.path()), 0);
    }

    #[test]
    fn test_apply_default_no_wan() {
        let dir = tempfile::tempdir().unwrap();
        setup_firewall_config(
            dir.path(),
            "config preferences preferences\n\toption remote_access 'default'\n",
        );

        let arena = Arena::new();
        let mut cfgs = parse_all(dir.path(), &arena, &["startwrt", "firewall"]).unwrap();
        apply_remote_access_config(&mut cfgs, "default", None, &[]);
        dump_all(dir.path(), cfgs).unwrap();

        // No WAN at all — no rules should be created
        assert_eq!(count_remote_rules(dir.path()), 0);
    }

    #[test]
    fn test_apply_default_ipv4_only() {
        let dir = tempfile::tempdir().unwrap();
        setup_firewall_config(
            dir.path(),
            "config preferences preferences\n\toption remote_access 'default'\n",
        );

        let wan_ipv4: Ipv4Addr = "192.168.1.1".parse().unwrap();

        let arena = Arena::new();
        let mut cfgs = parse_all(dir.path(), &arena, &["startwrt", "firewall"]).unwrap();
        apply_remote_access_config(&mut cfgs, "default", Some(wan_ipv4), &[]);
        dump_all(dir.path(), cfgs).unwrap();

        // Private IPv4, no IPv6 — IPv4-only rules
        assert_eq!(count_remote_rules(dir.path()), 4);
        for family in get_remote_rule_families(dir.path()) {
            assert_eq!(family, Some("ipv4".to_string()));
        }
    }

    #[test]
    fn test_apply_default_ipv6_ula_only() {
        let dir = tempfile::tempdir().unwrap();
        setup_firewall_config(
            dir.path(),
            "config preferences preferences\n\toption remote_access 'default'\n",
        );

        let wan_ipv6: Ipv6Addr = "fd00::1".parse().unwrap();

        let arena = Arena::new();
        let mut cfgs = parse_all(dir.path(), &arena, &["startwrt", "firewall"]).unwrap();
        apply_remote_access_config(&mut cfgs, "default", None, &[wan_ipv6]);
        dump_all(dir.path(), cfgs).unwrap();

        // No IPv4, ULA-only IPv6 — IPv6-only rules
        assert_eq!(count_remote_rules(dir.path()), 4);
        for family in get_remote_rule_families(dir.path()) {
            assert_eq!(family, Some("ipv6".to_string()));
        }
    }

    #[test]
    fn test_apply_idempotent() {
        let dir = tempfile::tempdir().unwrap();
        setup_firewall_config(
            dir.path(),
            "config preferences preferences\n\toption remote_access 'always'\n",
        );

        // Apply twice
        for _ in 0..2 {
            let arena = Arena::new();
            let mut cfgs = parse_all(dir.path(), &arena, &["startwrt", "firewall"]).unwrap();
            apply_remote_access_config(&mut cfgs, "always", None, &[]);
            dump_all(dir.path(), cfgs).unwrap();
        }

        // Should still have exactly 4 rules, not 8
        assert_eq!(count_remote_rules(dir.path()), 4);
    }
}
