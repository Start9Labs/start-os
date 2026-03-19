use crate::devices::{self, Device, DeviceStatus};
use crate::error::ErrorKind;
use crate::profiles::Lookup;
use crate::utils::{DeserializeStdin, HandlerExtSerde};
use crate::{CliContext, CtrlContext, Error, ServerContext};
use rpc_toolkit::{from_fn, from_fn_async, HandlerExt as _, ParentHandler};
use serde::{Deserialize, Serialize};
use std::collections::HashMap;
use uciedit::openwrt::{DhcpHost, FirewallRedirect, FirewallRule, FirewallTarget, FirewallZone};
use uciedit::{dump_all, parse_all, Arena};

pub fn published_ports<C: CtrlContext>() -> ParentHandler<C> {
    ParentHandler::new()
        .subcommand(
            "list",
            from_fn_async(list)
                .with_display_serializable()
                .with_call_remote::<CliContext>(),
        )
        .subcommand("set", from_fn(set::<C>).no_display())
}

// ── Types ──────────────────────────────────────────────

#[derive(Debug, Clone, Serialize, Deserialize)]
#[serde(rename_all = "lowercase")]
pub enum Protocol {
    Tcp,
    Udp,
    #[serde(rename = "tcp+udp")]
    TcpUdp,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
#[serde(rename_all = "lowercase")]
pub enum PublishedPortStatus {
    Active,
    Partial,
    Paused,
    Error,
    Disabled,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct PublishedPort {
    pub id: String,
    pub enabled: bool,
    pub label: String,
    pub device_mac: String,
    pub ports: String,
    pub protocol: Protocol,
    pub ipv4: bool,
    pub ipv6: bool,
    pub ipv4_public_port: Option<String>,
    pub source: String,
    pub status: PublishedPortStatus,
    pub status_reason: Option<String>,
    pub device_name: Option<String>,
    pub device_ipv4: Option<String>,
    pub device_ipv6: Option<String>,
}

#[derive(Debug, Serialize, Deserialize)]
pub struct PublishedPortInput {
    pub id: String,
    pub enabled: bool,
    pub label: String,
    pub device_mac: String,
    pub ports: String,
    pub protocol: Protocol,
    pub ipv4: bool,
    pub ipv6: bool,
    pub ipv4_public_port: Option<String>,
    pub source: String,
}

#[derive(Debug, Serialize, Deserialize)]
pub struct PublishedPortsSetRequest {
    pub ports: Vec<PublishedPortInput>,
}

// ── Helpers ──────────────────────────────────────────────

/// Convert our Protocol enum to UCI proto string
fn protocol_to_uci(proto: &Protocol) -> Vec<String> {
    match proto {
        Protocol::Tcp => vec!["tcp".into()],
        Protocol::Udp => vec!["udp".into()],
        Protocol::TcpUdp => vec!["tcp".into(), "udp".into()],
    }
}

/// Convert UCI proto list to our Protocol enum
fn uci_to_protocol(proto: &[String]) -> Protocol {
    let has_tcp = proto.iter().any(|p| p == "tcp");
    let has_udp = proto.iter().any(|p| p == "udp");
    match (has_tcp, has_udp) {
        (true, true) => Protocol::TcpUdp,
        (false, true) => Protocol::Udp,
        _ => Protocol::Tcp,
    }
}

// ── Validation ──────────────────────────────────────────

fn validate_port_or_range(s: &str) -> bool {
    if let Some((a, b)) = s.split_once('-') {
        let (Ok(a), Ok(b)) = (a.parse::<u16>(), b.parse::<u16>()) else {
            return false;
        };
        a >= 1 && b >= 1 && a <= b
    } else {
        matches!(s.parse::<u16>(), Ok(p) if p >= 1)
    }
}

fn validate_mac(s: &str) -> bool {
    let parts: Vec<&str> = s.split(':').collect();
    parts.len() == 6
        && parts
            .iter()
            .all(|p| p.len() == 2 && p.chars().all(|c| c.is_ascii_hexdigit()))
}

fn validate_source(s: &str) -> bool {
    if let Some((ip_str, prefix_str)) = s.split_once('/') {
        let Ok(prefix) = prefix_str.parse::<u8>() else {
            return false;
        };
        prefix <= 32 && ip_str.parse::<std::net::Ipv4Addr>().is_ok()
    } else {
        s.parse::<std::net::Ipv4Addr>().is_ok()
    }
}

fn validate_id(s: &str) -> bool {
    !s.is_empty() && s.chars().all(|c| c.is_ascii_alphanumeric() || c == '-')
}

fn validate_inputs(ports: &[PublishedPortInput]) -> Result<(), Error> {
    for port in ports {
        if !validate_id(&port.id) {
            return Err(ErrorKind::InvalidValue {
                field: "id".into(),
                value: port.id.clone(),
            }
            .into());
        }
        if port.label.trim().is_empty() {
            return Err(ErrorKind::InvalidValue {
                field: "label".into(),
                value: port.label.clone(),
            }
            .into());
        }
        if !port.device_mac.is_empty() && !validate_mac(&port.device_mac) {
            return Err(ErrorKind::InvalidValue {
                field: "device_mac".into(),
                value: port.device_mac.clone(),
            }
            .into());
        }
        if !validate_port_or_range(&port.ports) {
            return Err(ErrorKind::InvalidValue {
                field: "ports".into(),
                value: port.ports.clone(),
            }
            .into());
        }
        if let Some(ref pub_port) = port.ipv4_public_port {
            if !validate_port_or_range(pub_port) {
                return Err(ErrorKind::InvalidValue {
                    field: "ipv4_public_port".into(),
                    value: pub_port.clone(),
                }
                .into());
            }
        }
        if port.source != "any" && !validate_source(&port.source) {
            return Err(ErrorKind::InvalidValue {
                field: "source".into(),
                value: port.source.clone(),
            }
            .into());
        }
    }
    Ok(())
}

struct RawPort {
    id: String,
    enabled: bool,
    label: String,
    device_mac: String,
    ports: String,
    protocol: Protocol,
    ipv4: bool,
    ipv6: bool,
    ipv4_public_port: Option<String>,
    source: String,
}

/// Extract raw published ports from firewall config sections.
fn extract_ports(arena: &Arena, uci_root: &std::path::Path) -> Result<Vec<RawPort>, Error> {
    let cfgs = parse_all(uci_root, arena, &["firewall"])?;

    // Collect redirects and rules with _pp_id metadata
    let mut redirects_by_id: HashMap<String, FirewallRedirect> = HashMap::new();
    let mut rules_by_id: HashMap<String, FirewallRule> = HashMap::new();
    let mut unmatched_redirects: Vec<(Option<String>, FirewallRedirect)> = Vec::new();
    let mut unmatched_rules: Vec<(Option<String>, FirewallRule)> = Vec::new();

    for section in &cfgs["firewall"].sections {
        let sec_name = section.name().map(|s| s.to_string());

        // Try as redirect
        if let Ok(redirect) = section.get::<FirewallRedirect>() {
            if redirect.target == "DNAT" {
                if let Some(ref pp_id) = redirect._pp_id {
                    if redirect._pp_mac.as_ref().is_some_and(|m| !m.is_empty()) {
                        redirects_by_id.insert(pp_id.clone(), redirect);
                    }
                } else {
                    unmatched_redirects.push((sec_name, redirect));
                }
                continue;
            }
        }

        // Try as rule (IPv6 port forward)
        if let Ok(rule) = section.get::<FirewallRule>() {
            if rule.family.as_deref() == Some("ipv6")
                && matches!(rule.target, FirewallTarget::ACCEPT)
                && rule.src == "wan"
                && rule.dest_port.is_some()
            {
                if let Some(ref pp_id) = rule._pp_id {
                    if rule._pp_mac.as_ref().is_some_and(|m| !m.is_empty()) {
                        rules_by_id.insert(pp_id.clone(), rule);
                    }
                } else {
                    unmatched_rules.push((sec_name, rule));
                }
            }
        }
    }

    let mut ports = Vec::new();

    // Process matched pairs
    let mut processed = std::collections::HashSet::new();
    for (pp_id, redirect) in &redirects_by_id {
        let rule = rules_by_id.get(pp_id);
        ports.push(sections_to_raw_port(pp_id.clone(), Some(redirect), rule));
        processed.insert(pp_id.clone());
    }

    // IPv6-only rules with _pp_id but no matching redirect
    for (pp_id, rule) in &rules_by_id {
        if !processed.contains(pp_id) {
            ports.push(sections_to_raw_port(pp_id.clone(), None, Some(rule)));
        }
    }

    // Unmatched redirects/rules are only treated as legacy published ports
    // if their section name starts with "pp_". Otherwise they are standard
    // firewall entries (e.g. Allow_DHCPv6) that happen to match the shape.
    for (sec_name, redirect) in &unmatched_redirects {
        let name = sec_name.as_deref().unwrap_or("");
        if let Some(id) = name.strip_prefix("pp_") {
            ports.push(sections_to_raw_port(id.to_string(), Some(redirect), None));
        }
    }

    for (sec_name, rule) in &unmatched_rules {
        let name = sec_name.as_deref().unwrap_or("");
        if let Some(id) = name.strip_prefix("pp_").and_then(|s| s.strip_suffix("_v6")) {
            ports.push(sections_to_raw_port(id.to_string(), None, Some(rule)));
        }
    }

    Ok(ports)
}

fn sections_to_raw_port(
    id: String,
    redirect: Option<&FirewallRedirect>,
    rule: Option<&FirewallRule>,
) -> RawPort {
    // Determine enabled/protocol/label from whichever section exists
    let (enabled, label, protocol, ports, source, device_mac) = if let Some(r) = redirect {
        let enabled = r.enabled.as_deref() != Some("0");
        let protocol = uci_to_protocol(&r.proto);
        let ports = r.dest_port.clone().unwrap_or_default();
        let source = r.src_ip.clone().unwrap_or_else(|| "any".into());
        let mac = r._pp_mac.clone().unwrap_or_default();
        (enabled, r.name.clone(), protocol, ports, source, mac)
    } else if let Some(r) = rule {
        let enabled = r.enabled.as_deref() != Some("0");
        let protocol = uci_to_protocol(&r.proto);
        let ports = r.dest_port.clone().unwrap_or_default();
        let source = r.src_ip.clone().unwrap_or_else(|| "any".into());
        let mac = r._pp_mac.clone().unwrap_or_default();
        (enabled, r.name.clone(), protocol, ports, source, mac)
    } else {
        unreachable!("at least one section must exist")
    };

    let ipv4_public_port = redirect.and_then(|r| {
        let src_dport = r.src_dport.as_deref()?;
        let dest_port = r.dest_port.as_deref()?;
        if src_dport != dest_port {
            Some(src_dport.to_string())
        } else {
            None
        }
    });

    RawPort {
        id,
        enabled,
        label,
        device_mac,
        ports,
        protocol,
        ipv4: redirect.is_some(),
        ipv6: rule.is_some(),
        ipv4_public_port,
        source,
    }
}

fn compute_status(
    port: &RawPort,
    device: Option<&Device>,
) -> (PublishedPortStatus, Option<String>) {
    if !port.enabled {
        return (PublishedPortStatus::Disabled, None);
    }

    let device = match device {
        Some(d) => d,
        None => return (PublishedPortStatus::Paused, Some("Device not found".into())),
    };

    if matches!(device.status, DeviceStatus::Offline) {
        return (PublishedPortStatus::Paused, Some("Device offline".into()));
    }
    if matches!(device.status, DeviceStatus::Blocked) {
        return (PublishedPortStatus::Paused, Some("Device blocked".into()));
    }

    let has_ipv4 = device.ipv4.is_some();
    let has_ipv6 = device.ipv6.is_some();

    if port.ipv4 && !has_ipv4 {
        if port.ipv6 && has_ipv6 {
            return (
                PublishedPortStatus::Partial,
                Some("IPv4 unavailable".into()),
            );
        }
        return (
            PublishedPortStatus::Error,
            Some("No addresses available".into()),
        );
    }

    if port.ipv6 && !has_ipv6 {
        if port.ipv4 && has_ipv4 {
            return (
                PublishedPortStatus::Partial,
                Some("IPv6 unavailable".into()),
            );
        }
        return (
            PublishedPortStatus::Error,
            Some("No addresses available".into()),
        );
    }

    (PublishedPortStatus::Active, None)
}

/// Generate a v4-style UUID from random bytes
fn uuid_v4() -> String {
    use rand::Rng;
    let mut bytes = [0u8; 16];
    rand::rng().fill(&mut bytes);
    // Set version (4) and variant (RFC 4122)
    bytes[6] = (bytes[6] & 0x0f) | 0x40;
    bytes[8] = (bytes[8] & 0x3f) | 0x80;
    format!(
        "{:08x}-{:04x}-{:04x}-{:04x}-{:012x}",
        u32::from_be_bytes([bytes[0], bytes[1], bytes[2], bytes[3]]),
        u16::from_be_bytes([bytes[4], bytes[5]]),
        u16::from_be_bytes([bytes[6], bytes[7]]),
        u16::from_be_bytes([bytes[8], bytes[9]]),
        u64::from_be_bytes([0, 0, bytes[10], bytes[11], bytes[12], bytes[13], bytes[14], bytes[15]]),
    )
}

// ── Handlers ──────────────────────────────────────────────

pub async fn list(_ctx: ServerContext) -> Result<Vec<PublishedPort>, Error> {
    // Read firewall config and device list in parallel
    let (ports_result, devices_result) = tokio::join!(
        tokio::task::spawn_blocking(|| {
            let arena = Arena::new();
            extract_ports(&arena, std::path::Path::new("/etc/config"))
        }),
        devices::list(ServerContext::default()),
    );

    let raw_ports = ports_result
        .map_err(|e| Error::other(format!("firewall parse task panicked: {e}")))?
        ?;
    let devices = devices_result?;

    // Index devices by MAC (skip VPN devices which have no MAC)
    let devices_by_mac: HashMap<String, &Device> = devices
        .iter()
        .filter_map(|d| Some((d.mac.as_ref()?.to_uppercase(), d)))
        .collect();

    let ports = raw_ports
        .iter()
        .map(|raw| {
            let device = devices_by_mac.get(&raw.device_mac.to_uppercase()).copied();
            let (status, status_reason) = compute_status(raw, device);

            PublishedPort {
                id: raw.id.clone(),
                enabled: raw.enabled,
                label: raw.label.clone(),
                device_mac: raw.device_mac.clone(),
                ports: raw.ports.clone(),
                protocol: raw.protocol.clone(),
                ipv4: raw.ipv4,
                ipv6: raw.ipv6,
                ipv4_public_port: raw.ipv4_public_port.clone(),
                source: raw.source.clone(),
                status,
                status_reason,
                device_name: device.and_then(|d| {
                    d.name
                        .clone()
                        .or_else(|| d.hostname.clone())
                }),
                device_ipv4: device.and_then(|d| d.ipv4.clone()),
                device_ipv6: device.and_then(|d| d.ipv6.clone()),
            }
        })
        .collect();

    Ok(ports)
}

pub fn set<C: CtrlContext>(
    ctx: C,
    DeserializeStdin(req): DeserializeStdin<PublishedPortsSetRequest>,
) -> Result<(), Error> {
    validate_inputs(&req.ports)?;

    let mut retries = 4;
    loop {
        // Resolve device IPs and interfaces inside the retry loop so they stay fresh
        let device_info: HashMap<String, DeviceNetInfo> = if ctx.effectful() {
            resolve_device_info(&req.ports)
        } else {
            HashMap::new()
        };

        // Validate that enabled ports have the required device addresses
        if ctx.effectful() {
            for port in &req.ports {
                if !port.enabled || port.device_mac.is_empty() {
                    continue;
                }
                let info = device_info.get(&port.device_mac.to_uppercase());
                let (ipv4_addr, ipv6_addr) = info
                    .map(|i| (i.ipv4.as_ref(), i.ipv6.as_ref()))
                    .unwrap_or((None, None));
                if port.ipv4 && ipv4_addr.is_none() {
                    return Err(ErrorKind::MissingDeviceAddress {
                        mac: port.device_mac.clone(),
                        family: "IPv4".into(),
                        label: port.label.clone(),
                    }
                    .into());
                }
                if port.ipv6 && ipv6_addr.is_none() {
                    return Err(ErrorKind::MissingDeviceAddress {
                        mac: port.device_mac.clone(),
                        family: "IPv6".into(),
                        label: port.label.clone(),
                    }
                    .into());
                }
            }
        }

        let arena = Arena::new();
        let mut cfgs = parse_all(ctx.uci_root(), &arena, &["firewall"])?;

        // Build MAC → zone lookup from profiles and firewall zones
        let mac_zones = if ctx.effectful() {
            resolve_device_zones(ctx.uci_root().as_path(), &device_info)
        } else {
            HashMap::new()
        };

        // Remove all existing published-port sections (name starts with "pp_")
        cfgs["firewall"].sections.retain(|section| {
            if let Some(name) = section.name() {
                if name.starts_with("pp_") {
                    return false;
                }
            }
            // Also remove sections with _pp_id metadata (in case name doesn't match)
            if let Ok(redirect) = section.get::<FirewallRedirect>() {
                if redirect._pp_id.is_some() && redirect.target == "DNAT" {
                    return false;
                }
            }
            if let Ok(rule) = section.get::<FirewallRule>() {
                if rule._pp_id.is_some()
                    && rule.family.as_deref() == Some("ipv6")
                    && matches!(rule.target, FirewallTarget::ACCEPT)
                {
                    return false;
                }
            }
            true
        });

        // Add new sections for each published port
        for port in &req.ports {
            let proto = protocol_to_uci(&port.protocol);
            let safe_id: String = port
                .id
                .chars()
                .map(|c| if c == '-' { '_' } else { c })
                .filter(|c| c.is_ascii_alphanumeric() || *c == '_')
                .collect();
            let mac_upper = port.device_mac.to_uppercase();
            let info = device_info.get(&mac_upper);
            let ipv4_addr = info.and_then(|i| i.ipv4.clone());
            let ipv6_addr = info.and_then(|i| i.ipv6.clone());
            let dest_zone = mac_zones
                .get(&mac_upper)
                .cloned()
                .unwrap_or_else(|| "lan".into());

            // IPv4 redirect (DNAT)
            if port.ipv4 {
                let redirect = FirewallRedirect {
                    name: port.label.clone(),
                    src: "wan".into(),
                    dest: Some(dest_zone.clone()),
                    target: "DNAT".into(),
                    proto: proto.clone(),
                    src_dport: Some(
                        port.ipv4_public_port
                            .clone()
                            .unwrap_or_else(|| port.ports.clone()),
                    ),
                    dest_ip: ipv4_addr,
                    dest_port: Some(port.ports.clone()),
                    src_ip: if port.source != "any" {
                        Some(port.source.clone())
                    } else {
                        None
                    },
                    enabled: Some(if port.enabled { "1" } else { "0" }.into()),
                    _pp_id: Some(port.id.clone()),
                    _pp_mac: Some(port.device_mac.clone()),
                };
                let section_name = format!("pp_{}", safe_id);
                cfgs["firewall"].append(&redirect, Some(&section_name))?;
            }

            // IPv6 rule (ACCEPT)
            if port.ipv6 {
                let rule = FirewallRule {
                    name: port.label.clone(),
                    src: "wan".into(),
                    dest: Some(dest_zone.clone()),
                    target: FirewallTarget::ACCEPT,
                    proto: proto.clone(),
                    dest_ip: ipv6_addr,
                    dest_port: Some(port.ports.clone()),
                    family: Some("ipv6".into()),
                    src_ip: if port.source != "any" {
                        Some(port.source.clone())
                    } else {
                        None
                    },
                    enabled: Some(if port.enabled { "1" } else { "0" }.into()),
                    _pp_id: Some(port.id.clone()),
                    _pp_mac: Some(port.device_mac.clone()),
                    ..Default::default()
                };
                let section_name = format!("pp_{}_v6", safe_id);
                cfgs["firewall"].append(&rule, Some(&section_name))?;
            }
        }

        match dump_all(ctx.uci_root(), cfgs) {
            Err(uciedit::Error::Conflict { .. }) if retries > 0 => {
                retries -= 1;
                continue;
            }
            Err(err) => {
                crate::activity::log("published-ports", "updated", false, "Failed to update published ports", Some(&err.to_string()));
                return Err(err.into());
            }
            Ok(()) => {
                if ctx.effectful() {
                    restart_firewall();
                }
                crate::activity::log("published-ports", "updated", true, &format!("Updated published ports ({} rules)", req.ports.len()), None);
                return Ok(());
            }
        }
    }
}

struct DeviceNetInfo {
    ipv4: Option<String>,
    ipv6: Option<String>,
    /// ARP interface name (e.g. "br-lan.101") for zone resolution
    arp_interface: Option<String>,
}

/// Resolve IPv4/IPv6 addresses and ARP interface for devices referenced by published ports.
fn resolve_device_info(
    ports: &[PublishedPortInput],
) -> HashMap<String, DeviceNetInfo> {
    let mut result: HashMap<String, DeviceNetInfo> = HashMap::new();

    // Collect unique MACs we need to resolve
    let macs: std::collections::HashSet<String> = ports
        .iter()
        .map(|p| p.device_mac.to_uppercase())
        .collect();

    if macs.is_empty() {
        return result;
    }

    // Read DHCP hosts for static IPs
    let arena = Arena::new();
    let mut static_ips: HashMap<String, Option<String>> = HashMap::new();
    if let Ok(cfgs) = parse_all(std::path::Path::new("/etc/config"), &arena, &["dhcp"]) {
        cfgs["dhcp"]
            .each::<DhcpHost, Error>(|_, host| {
                let mac = host.mac.to_uppercase();
                if macs.contains(&mac) {
                    static_ips.insert(mac, host.ip.clone());
                }
            })
            .ok();
    }

    // Read ARP/neighbor table for dynamic IPs and interface names
    let neigh_output = std::process::Command::new("ip")
        .args(["neigh", "show"])
        .output()
        .ok()
        .filter(|o| o.status.success())
        .map(|o| String::from_utf8_lossy(&o.stdout).to_string())
        .unwrap_or_default();

    let mut arp_ipv4: HashMap<String, String> = HashMap::new();
    let mut arp_ipv6_candidates: HashMap<String, Vec<String>> = HashMap::new();
    let mut arp_iface: HashMap<String, String> = HashMap::new();

    for line in neigh_output.lines() {
        if let Some((ip, rest)) = line.split_once(" dev ") {
            if let Some((iface, rest)) = rest.split_once(" lladdr ") {
                if let Some(mac) = rest.split_whitespace().next() {
                    let mac_upper = mac.to_uppercase();
                    if macs.contains(&mac_upper) {
                        let ip = ip.trim();
                        // Capture interface (prefer br-lan* interfaces)
                        if iface.starts_with("br-lan") {
                            arp_iface
                                .entry(mac_upper.clone())
                                .or_insert_with(|| iface.to_string());
                        }
                        if ip.contains(':') {
                            arp_ipv6_candidates
                                .entry(mac_upper)
                                .or_default()
                                .push(ip.to_string());
                        } else {
                            arp_ipv4
                                .entry(mac_upper)
                                .or_insert_with(|| ip.to_string());
                        }
                    }
                }
            }
        }
    }

    // Pick best IPv6 per MAC: GUA preferred over ULA, link-local excluded
    let arp_ipv6: HashMap<String, String> = arp_ipv6_candidates
        .iter()
        .filter_map(|(mac, candidates)| {
            devices::pick_ipv6(candidates.iter().map(|s| s.as_str()))
                .map(|ip| (mac.clone(), ip))
        })
        .collect();

    // Read DHCP leases as fallback for IPv4
    let leases = std::fs::read_to_string("/tmp/dhcp.leases").unwrap_or_default();
    let mut lease_ipv4: HashMap<String, String> = HashMap::new();
    for line in leases.lines() {
        let parts: Vec<&str> = line.split_whitespace().collect();
        if parts.len() >= 3 {
            let mac = parts[1].to_uppercase();
            if macs.contains(&mac) {
                lease_ipv4.entry(mac).or_insert_with(|| parts[2].to_string());
            }
        }
    }

    // Merge: prefer static > ARP > lease
    for mac in &macs {
        let ipv4 = static_ips
            .get(mac)
            .and_then(|ip| ip.clone())
            .or_else(|| arp_ipv4.get(mac).cloned())
            .or_else(|| lease_ipv4.get(mac).cloned());
        let ipv6 = arp_ipv6.get(mac).cloned();
        let arp_interface = arp_iface.get(mac).cloned();
        result.insert(mac.clone(), DeviceNetInfo { ipv4, ipv6, arp_interface });
    }

    result
}

/// Resolve MAC addresses to firewall zone names via ARP interface → VLAN tag → profile → zone.
fn resolve_device_zones(
    uci_root: &std::path::Path,
    device_info: &HashMap<String, DeviceNetInfo>,
) -> HashMap<String, String> {
    let mut mac_zones: HashMap<String, String> = HashMap::new();

    let arena = Arena::new();
    let Ok(cfgs) = parse_all(uci_root, &arena, &["startwrt", "firewall"]) else {
        return mac_zones;
    };

    // Build VLAN tag → profile interface name
    let Ok(lookup) = Lookup::parse(ServerContext::default(), &cfgs) else {
        return mac_zones;
    };

    // Build interface name → zone name from firewall config
    let mut iface_to_zone: HashMap<String, String> = HashMap::new();
    cfgs["firewall"]
        .each::<FirewallZone, Error>(|_, zone| {
            for iface in &zone.network {
                iface_to_zone.insert(iface.clone(), zone.name.clone());
            }
        })
        .ok();

    // For each device, resolve: ARP interface → VLAN tag → profile → zone
    for (mac, info) in device_info {
        let Some(ref arp_iface) = info.arp_interface else {
            continue;
        };
        // Extract VLAN tag from interface name (e.g. "br-lan.101" → 101, "br-lan" → 1)
        let vlan_tag = arp_iface
            .split('.')
            .nth(1)
            .and_then(|s| s.parse::<u16>().ok())
            .unwrap_or(1);

        if let Some(profile) = lookup.from_vlan(vlan_tag) {
            if let Some(zone) = iface_to_zone.get(&profile.interface) {
                mac_zones.insert(mac.clone(), zone.clone());
            }
        }
    }

    mac_zones
}

fn restart_firewall() {
    std::thread::spawn(|| {
        let result = std::process::Command::new("/etc/init.d/firewall")
            .arg("restart")
            .spawn()
            .and_then(|mut c| c.wait());
        if let Err(e) = result {
            tracing::error!("failed to restart firewall: {e}");
        }
    });
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::utils::DeserializeStdin;
    use crate::CtrlContext;
    use rpc_toolkit::Context;
    use std::path::PathBuf;
    use std::sync::Arc;
    use tokio::runtime::Runtime;

    #[derive(Clone)]
    struct TestContext(PathBuf);

    impl Context for TestContext {
        fn runtime(&self) -> Option<Arc<Runtime>> {
            None
        }
    }

    impl CtrlContext for TestContext {
        fn uci_root(&self) -> PathBuf {
            self.0.clone()
        }
        fn effectful(&self) -> bool {
            false
        }
    }

    fn setup_firewall(dir: &std::path::Path, content: &str) {
        std::fs::write(dir.join("firewall"), content).unwrap();
    }

    // ── extract_ports tests ──

    #[test]
    fn extract_empty_firewall() {
        let dir = tempfile::tempdir().unwrap();
        setup_firewall(
            dir.path(),
            "\
config defaults
\toption input 'REJECT'
",
        );
        let arena = Arena::new();
        let ports = extract_ports(&arena, dir.path()).unwrap();
        assert!(ports.is_empty());
    }

    #[test]
    fn extract_ipv4_only() {
        let dir = tempfile::tempdir().unwrap();
        setup_firewall(
            dir.path(),
            "\
config redirect 'pp_abc123'
\toption name 'Web Server'
\toption src 'wan'
\toption dest 'lan'
\toption target 'DNAT'
\tlist proto 'tcp'
\toption src_dport '80'
\toption dest_port '80'
\toption enabled '1'
\toption _pp_id 'abc123'
\toption _pp_mac 'AA:BB:CC:DD:EE:FF'
",
        );
        let arena = Arena::new();
        let ports = extract_ports(&arena, dir.path()).unwrap();
        assert_eq!(ports.len(), 1);
        assert!(ports[0].ipv4);
        assert!(!ports[0].ipv6);
        assert_eq!(ports[0].id, "abc123");
        assert_eq!(ports[0].label, "Web Server");
        assert_eq!(ports[0].device_mac, "AA:BB:CC:DD:EE:FF");
    }

    #[test]
    fn extract_ipv6_only() {
        let dir = tempfile::tempdir().unwrap();
        setup_firewall(
            dir.path(),
            "\
config rule 'pp_abc123_v6'
\toption name 'Web Server v6'
\toption src 'wan'
\toption dest 'lan'
\toption target 'ACCEPT'
\tlist proto 'tcp'
\toption dest_port '80'
\toption family 'ipv6'
\toption enabled '1'
\toption _pp_id 'abc123'
\toption _pp_mac 'AA:BB:CC:DD:EE:FF'
",
        );
        let arena = Arena::new();
        let ports = extract_ports(&arena, dir.path()).unwrap();
        assert_eq!(ports.len(), 1);
        assert!(!ports[0].ipv4);
        assert!(ports[0].ipv6);
        assert_eq!(ports[0].id, "abc123");
    }

    #[test]
    fn extract_paired_ipv4_ipv6() {
        let dir = tempfile::tempdir().unwrap();
        setup_firewall(
            dir.path(),
            "\
config redirect 'pp_abc123'
\toption name 'Web Server'
\toption src 'wan'
\toption dest 'lan'
\toption target 'DNAT'
\tlist proto 'tcp'
\toption src_dport '80'
\toption dest_port '80'
\toption enabled '1'
\toption _pp_id 'abc123'
\toption _pp_mac 'AA:BB:CC:DD:EE:FF'

config rule 'pp_abc123_v6'
\toption name 'Web Server v6'
\toption src 'wan'
\toption dest 'lan'
\toption target 'ACCEPT'
\tlist proto 'tcp'
\toption dest_port '80'
\toption family 'ipv6'
\toption enabled '1'
\toption _pp_id 'abc123'
\toption _pp_mac 'AA:BB:CC:DD:EE:FF'
",
        );
        let arena = Arena::new();
        let ports = extract_ports(&arena, dir.path()).unwrap();
        assert_eq!(ports.len(), 1);
        assert!(ports[0].ipv4);
        assert!(ports[0].ipv6);
        assert_eq!(ports[0].id, "abc123");
    }

    #[test]
    fn extract_legacy_no_pp_id() {
        let dir = tempfile::tempdir().unwrap();
        setup_firewall(
            dir.path(),
            "\
config redirect 'pp_foo'
\toption name 'Legacy Port'
\toption src 'wan'
\toption dest 'lan'
\toption target 'DNAT'
\tlist proto 'tcp'
\toption src_dport '443'
\toption dest_port '443'
",
        );
        let arena = Arena::new();
        let ports = extract_ports(&arena, dir.path()).unwrap();
        assert_eq!(ports.len(), 1);
        assert_eq!(ports[0].id, "foo");
    }

    #[test]
    fn extract_legacy_unnamed_ignored() {
        let dir = tempfile::tempdir().unwrap();
        setup_firewall(
            dir.path(),
            "\
config redirect
\toption name 'Unnamed Port'
\toption src 'wan'
\toption dest 'lan'
\toption target 'DNAT'
\tlist proto 'tcp'
\toption src_dport '8080'
\toption dest_port '8080'
",
        );
        let arena = Arena::new();
        let ports = extract_ports(&arena, dir.path()).unwrap();
        // Redirects without pp_ prefix or _pp_id are not published ports
        assert!(ports.is_empty());
    }

    #[test]
    fn extract_ignores_standard_firewall_rules() {
        let dir = tempfile::tempdir().unwrap();
        setup_firewall(
            dir.path(),
            "\
config rule 'Allow_DHCPv6'
\toption name 'Allow-DHCPv6'
\toption src 'wan'
\toption proto 'udp'
\toption dest_port '546'
\toption family 'ipv6'
\toption target 'ACCEPT'

config rule 'Allow_MLD'
\toption name 'Allow-MLD'
\toption src 'wan'
\toption proto 'icmp'
\toption family 'ipv6'
\toption target 'ACCEPT'

config rule 'Allow_ICMPv6_Forward'
\toption name 'Allow-ICMPv6-Forward'
\toption src 'wan'
\toption dest '*'
\toption proto 'icmp'
\toption family 'ipv6'
\toption target 'ACCEPT'
",
        );
        let arena = Arena::new();
        let ports = extract_ports(&arena, dir.path()).unwrap();
        assert!(ports.is_empty(), "standard firewall rules should not appear as published ports");
    }

    #[test]
    fn extract_ignores_ghost_entries_with_empty_mac() {
        let dir = tempfile::tempdir().unwrap();
        setup_firewall(
            dir.path(),
            "\
config redirect 'cfg0a0a0a'
\toption name 'Allow-DHCPv6'
\toption src 'wan'
\toption dest 'lan'
\toption target 'DNAT'
\tlist proto 'udp'
\toption src_dport '546'
\toption dest_port '546'
\toption enabled '1'
\toption _pp_id 'ghost1'
\toption _pp_mac ''

config rule 'cfg0b0b0b'
\toption name 'Allow-DHCPv6 v6'
\toption src 'wan'
\toption dest 'lan'
\toption target 'ACCEPT'
\tlist proto 'udp'
\toption dest_port '546'
\toption family 'ipv6'
\toption enabled '1'
\toption _pp_id 'ghost1'
\toption _pp_mac ''
",
        );
        let arena = Arena::new();
        let ports = extract_ports(&arena, dir.path()).unwrap();
        assert!(ports.is_empty(), "ghost entries with empty _pp_mac should be ignored");
    }

    #[test]
    fn extract_disabled() {
        let dir = tempfile::tempdir().unwrap();
        setup_firewall(
            dir.path(),
            "\
config redirect 'pp_dis1'
\toption name 'Disabled Port'
\toption src 'wan'
\toption dest 'lan'
\toption target 'DNAT'
\tlist proto 'tcp'
\toption src_dport '22'
\toption dest_port '22'
\toption enabled '0'
\toption _pp_id 'dis1'
\toption _pp_mac 'AA:BB:CC:DD:EE:FF'
",
        );
        let arena = Arena::new();
        let ports = extract_ports(&arena, dir.path()).unwrap();
        assert_eq!(ports.len(), 1);
        assert!(!ports[0].enabled);
    }

    #[test]
    fn extract_public_port_differs() {
        let dir = tempfile::tempdir().unwrap();
        setup_firewall(
            dir.path(),
            "\
config redirect 'pp_pub1'
\toption name 'Custom Public Port'
\toption src 'wan'
\toption dest 'lan'
\toption target 'DNAT'
\tlist proto 'tcp'
\toption src_dport '9090'
\toption dest_port '80'
\toption enabled '1'
\toption _pp_id 'pub1'
\toption _pp_mac 'AA:BB:CC:DD:EE:FF'
",
        );
        let arena = Arena::new();
        let ports = extract_ports(&arena, dir.path()).unwrap();
        assert_eq!(ports.len(), 1);
        assert_eq!(ports[0].ipv4_public_port.as_deref(), Some("9090"));
    }

    #[test]
    fn extract_public_port_same() {
        let dir = tempfile::tempdir().unwrap();
        setup_firewall(
            dir.path(),
            "\
config redirect 'pp_pub2'
\toption name 'Same Public Port'
\toption src 'wan'
\toption dest 'lan'
\toption target 'DNAT'
\tlist proto 'tcp'
\toption src_dport '80'
\toption dest_port '80'
\toption enabled '1'
\toption _pp_id 'pub2'
\toption _pp_mac 'AA:BB:CC:DD:EE:FF'
",
        );
        let arena = Arena::new();
        let ports = extract_ports(&arena, dir.path()).unwrap();
        assert_eq!(ports.len(), 1);
        assert!(ports[0].ipv4_public_port.is_none());
    }

    // ── set() tests ──

    fn make_port(id: &str, ipv4: bool, ipv6: bool) -> PublishedPortInput {
        PublishedPortInput {
            id: id.to_string(),
            enabled: true,
            label: format!("Port {}", id),
            device_mac: "AA:BB:CC:DD:EE:FF".to_string(),
            ports: "80".to_string(),
            protocol: Protocol::Tcp,
            ipv4,
            ipv6,
            ipv4_public_port: None,
            source: "any".to_string(),
        }
    }

    #[test]
    fn set_creates_redirect_and_rule() {
        let dir = tempfile::tempdir().unwrap();
        setup_firewall(dir.path(), "");
        let ctx = TestContext(dir.path().to_path_buf());

        set(
            ctx,
            DeserializeStdin(PublishedPortsSetRequest {
                ports: vec![make_port("test1", true, true)],
            }),
        )
        .unwrap();

        let arena = Arena::new();
        let ports = extract_ports(&arena, dir.path()).unwrap();
        assert_eq!(ports.len(), 1);
        assert!(ports[0].ipv4);
        assert!(ports[0].ipv6);
        assert_eq!(ports[0].id, "test1");

        // Verify section names in raw config
        let content = std::fs::read_to_string(dir.path().join("firewall")).unwrap();
        assert!(content.contains("config redirect pp_test1"), "missing redirect section");
        assert!(content.contains("config rule pp_test1_v6"), "missing rule section");
    }

    #[test]
    fn set_replaces_existing() {
        let dir = tempfile::tempdir().unwrap();
        setup_firewall(
            dir.path(),
            "\
config redirect 'pp_old1'
\toption name 'Old Port'
\toption src 'wan'
\toption dest 'lan'
\toption target 'DNAT'
\tlist proto 'tcp'
\toption src_dport '22'
\toption dest_port '22'
\toption _pp_id 'old1'
\toption _pp_mac 'AA:BB:CC:DD:EE:FF'
",
        );
        let ctx = TestContext(dir.path().to_path_buf());

        set(
            ctx,
            DeserializeStdin(PublishedPortsSetRequest {
                ports: vec![make_port("new1", true, false)],
            }),
        )
        .unwrap();

        let arena = Arena::new();
        let ports = extract_ports(&arena, dir.path()).unwrap();
        assert_eq!(ports.len(), 1);
        assert_eq!(ports[0].id, "new1");

        let content = std::fs::read_to_string(dir.path().join("firewall")).unwrap();
        assert!(!content.contains("pp_old1"), "old section should be removed");
    }

    #[test]
    fn set_clears_all() {
        let dir = tempfile::tempdir().unwrap();
        setup_firewall(
            dir.path(),
            "\
config redirect 'pp_del1'
\toption name 'To Delete'
\toption src 'wan'
\toption dest 'lan'
\toption target 'DNAT'
\tlist proto 'tcp'
\toption src_dport '80'
\toption dest_port '80'
\toption _pp_id 'del1'
\toption _pp_mac 'AA:BB:CC:DD:EE:FF'
",
        );
        let ctx = TestContext(dir.path().to_path_buf());

        set(
            ctx,
            DeserializeStdin(PublishedPortsSetRequest { ports: vec![] }),
        )
        .unwrap();

        let arena = Arena::new();
        let ports = extract_ports(&arena, dir.path()).unwrap();
        assert!(ports.is_empty());
    }

    #[test]
    fn set_disabled_port() {
        let dir = tempfile::tempdir().unwrap();
        setup_firewall(dir.path(), "");
        let ctx = TestContext(dir.path().to_path_buf());

        let mut port = make_port("dis1", true, true);
        port.enabled = false;

        set(
            ctx,
            DeserializeStdin(PublishedPortsSetRequest {
                ports: vec![port],
            }),
        )
        .unwrap();

        let arena = Arena::new();
        let ports = extract_ports(&arena, dir.path()).unwrap();
        assert_eq!(ports.len(), 1);
        assert!(!ports[0].enabled);

        let content = std::fs::read_to_string(dir.path().join("firewall")).unwrap();
        assert!(content.contains("option enabled '0'"), "expected 'option enabled 0' in:\n{content}");
    }

    #[test]
    fn set_custom_public_port() {
        let dir = tempfile::tempdir().unwrap();
        setup_firewall(dir.path(), "");
        let ctx = TestContext(dir.path().to_path_buf());

        let mut port = make_port("pub1", true, false);
        port.ipv4_public_port = Some("9090".to_string());

        set(
            ctx,
            DeserializeStdin(PublishedPortsSetRequest {
                ports: vec![port],
            }),
        )
        .unwrap();

        let content = std::fs::read_to_string(dir.path().join("firewall")).unwrap();
        assert!(content.contains("option src_dport '9090'"), "public port should be 9090");
        assert!(content.contains("option dest_port '80'"), "internal port should be 80");

        let arena = Arena::new();
        let ports = extract_ports(&arena, dir.path()).unwrap();
        assert_eq!(ports[0].ipv4_public_port.as_deref(), Some("9090"));
    }

    #[test]
    fn set_source_restriction() {
        let dir = tempfile::tempdir().unwrap();
        setup_firewall(dir.path(), "");
        let ctx = TestContext(dir.path().to_path_buf());

        let mut port = make_port("src1", true, true);
        port.source = "10.0.0.0/24".to_string();

        set(
            ctx.clone(),
            DeserializeStdin(PublishedPortsSetRequest {
                ports: vec![port],
            }),
        )
        .unwrap();

        let content = std::fs::read_to_string(dir.path().join("firewall")).unwrap();
        assert!(content.contains("src_ip"), "expected src_ip in:\n{content}");
        assert!(content.contains("10.0.0.0/24"), "expected source CIDR in:\n{content}");

        // Now test with "any" — no src_ip
        let port_any = make_port("src2", true, false);
        set(
            ctx,
            DeserializeStdin(PublishedPortsSetRequest {
                ports: vec![port_any],
            }),
        )
        .unwrap();

        let content = std::fs::read_to_string(dir.path().join("firewall")).unwrap();
        assert!(!content.contains("src_ip"), "source 'any' should not produce src_ip");
    }

    // ── uuid_v4 tests ──

    #[test]
    fn uuid_v4_format() {
        let a = uuid_v4();
        let b = uuid_v4();
        assert_ne!(a, b, "two UUIDs should differ");

        for id in [&a, &b] {
            let parts: Vec<&str> = id.split('-').collect();
            assert_eq!(parts.len(), 5, "UUID should have 5 parts: {}", id);
            assert_eq!(parts[0].len(), 8, "part 0 length");
            assert_eq!(parts[1].len(), 4, "part 1 length");
            assert_eq!(parts[2].len(), 4, "part 2 length");
            assert_eq!(parts[3].len(), 4, "part 3 length");
            assert_eq!(parts[4].len(), 12, "part 4 length");
            assert!(
                id.chars().all(|c| c.is_ascii_hexdigit() || c == '-'),
                "UUID contains invalid chars: {}",
                id
            );
        }
    }

    #[test]
    fn uuid_v4_version_bits() {
        let id = uuid_v4();
        let parts: Vec<&str> = id.split('-').collect();
        // Third group starts with version nibble
        let version_nibble = u8::from_str_radix(&parts[2][..1], 16).unwrap();
        assert_eq!(version_nibble, 4, "version nibble should be 4");
        // Fourth group starts with variant bits (8, 9, a, or b)
        let variant_nibble = u8::from_str_radix(&parts[3][..1], 16).unwrap();
        assert!(
            (0x8..=0xb).contains(&variant_nibble),
            "variant nibble should be 8-b, got {:x}",
            variant_nibble
        );
    }

    #[test]
    fn set_sanitizes_section_name() {
        let dir = tempfile::tempdir().unwrap();
        setup_firewall(dir.path(), "");
        let ctx = TestContext(dir.path().to_path_buf());

        set(
            ctx,
            DeserializeStdin(PublishedPortsSetRequest {
                ports: vec![make_port("a-b-c", true, true)],
            }),
        )
        .unwrap();

        let content = std::fs::read_to_string(dir.path().join("firewall")).unwrap();
        // Section names must use underscores (hyphens are illegal in UCI names)
        assert!(
            content.contains("config redirect pp_a_b_c"),
            "section name should have underscores, not hyphens, in:\n{content}"
        );
        assert!(
            content.contains("config rule pp_a_b_c_v6"),
            "v6 section name should have underscores in:\n{content}"
        );
        // But the _pp_id option value preserves the original hyphenated ID
        assert!(
            content.contains("option _pp_id 'a-b-c'"),
            "_pp_id should preserve original ID in:\n{content}"
        );
    }

    // ── Validation tests ──

    fn make_input(overrides: impl FnOnce(&mut PublishedPortInput)) -> PublishedPortInput {
        let mut p = make_port("valid-id", true, false);
        overrides(&mut p);
        p
    }

    fn assert_validation_fails(port: PublishedPortInput, expected_field: &str) {
        let err = validate_inputs(&[port]).unwrap_err();
        let msg = err.kind.to_string();
        assert!(
            msg.contains(expected_field),
            "expected error about '{expected_field}', got: {msg}"
        );
    }

    #[test]
    fn validate_rejects_invalid_id() {
        assert_validation_fails(make_input(|p| p.id = "has spaces".into()), "id");
        assert_validation_fails(make_input(|p| p.id = "has.dots".into()), "id");
        assert_validation_fails(make_input(|p| p.id = "".into()), "id");
        assert_validation_fails(make_input(|p| p.id = "has/slash".into()), "id");
    }

    #[test]
    fn validate_accepts_valid_id() {
        validate_inputs(&[make_input(|p| p.id = "abc-123".into())]).unwrap();
        validate_inputs(&[make_input(|p| p.id = "ABC".into())]).unwrap();
        validate_inputs(&[make_input(|p| p.id = "a".into())]).unwrap();
    }

    #[test]
    fn validate_rejects_empty_label() {
        assert_validation_fails(make_input(|p| p.label = "".into()), "label");
        assert_validation_fails(make_input(|p| p.label = "   ".into()), "label");
    }

    #[test]
    fn validate_rejects_invalid_mac() {
        assert_validation_fails(make_input(|p| p.device_mac = "not-a-mac".into()), "device_mac");
        assert_validation_fails(make_input(|p| p.device_mac = "AA:BB:CC:DD:EE".into()), "device_mac");
        assert_validation_fails(make_input(|p| p.device_mac = "GG:BB:CC:DD:EE:FF".into()), "device_mac");
    }

    #[test]
    fn validate_accepts_valid_mac() {
        validate_inputs(&[make_input(|p| p.device_mac = "aa:bb:cc:dd:ee:ff".into())]).unwrap();
        validate_inputs(&[make_input(|p| p.device_mac = "AA:BB:CC:DD:EE:FF".into())]).unwrap();
    }

    #[test]
    fn validate_allows_empty_mac() {
        // Disabled ports may have empty MAC
        validate_inputs(&[make_input(|p| p.device_mac = "".into())]).unwrap();
    }

    #[test]
    fn validate_rejects_invalid_ports() {
        assert_validation_fails(make_input(|p| p.ports = "0".into()), "ports");
        assert_validation_fails(make_input(|p| p.ports = "99999".into()), "ports");
        assert_validation_fails(make_input(|p| p.ports = "abc".into()), "ports");
        assert_validation_fails(make_input(|p| p.ports = "".into()), "ports");
        assert_validation_fails(make_input(|p| p.ports = "100-50".into()), "ports"); // start > end
    }

    #[test]
    fn validate_accepts_valid_ports() {
        validate_inputs(&[make_input(|p| p.ports = "80".into())]).unwrap();
        validate_inputs(&[make_input(|p| p.ports = "1".into())]).unwrap();
        validate_inputs(&[make_input(|p| p.ports = "65535".into())]).unwrap();
        validate_inputs(&[make_input(|p| p.ports = "80-443".into())]).unwrap();
    }

    #[test]
    fn validate_rejects_invalid_public_port() {
        assert_validation_fails(
            make_input(|p| p.ipv4_public_port = Some("0".into())),
            "ipv4_public_port",
        );
        assert_validation_fails(
            make_input(|p| p.ipv4_public_port = Some("abc".into())),
            "ipv4_public_port",
        );
    }

    #[test]
    fn validate_accepts_valid_public_port() {
        validate_inputs(&[make_input(|p| p.ipv4_public_port = Some("8080".into()))]).unwrap();
        validate_inputs(&[make_input(|p| p.ipv4_public_port = None)]).unwrap();
    }

    #[test]
    fn validate_rejects_invalid_source() {
        assert_validation_fails(make_input(|p| p.source = "".into()), "source");
        assert_validation_fails(make_input(|p| p.source = "not-an-ip".into()), "source");
        assert_validation_fails(make_input(|p| p.source = "10.0.0.0/33".into()), "source");
    }

    #[test]
    fn validate_accepts_valid_source() {
        validate_inputs(&[make_input(|p| p.source = "any".into())]).unwrap();
        validate_inputs(&[make_input(|p| p.source = "10.0.0.0/24".into())]).unwrap();
        validate_inputs(&[make_input(|p| p.source = "192.168.1.1".into())]).unwrap();
    }

    #[test]
    fn set_rejects_invalid_input() {
        let dir = tempfile::tempdir().unwrap();
        setup_firewall(dir.path(), "");
        let ctx = TestContext(dir.path().to_path_buf());

        let result = set(
            ctx,
            DeserializeStdin(PublishedPortsSetRequest {
                ports: vec![make_input(|p| p.ports = "0".into())],
            }),
        );
        assert!(result.is_err());
    }
}
