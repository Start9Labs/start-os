use crate::profiles::Lookup;
use crate::utils::{DeserializeStdin, HandlerExtSerde};
use crate::{CliContext, CtrlContext, Error, ServerContext};
use rpc_toolkit::{from_fn, from_fn_async, HandlerExt as _, ParentHandler};
use serde::{Deserialize, Serialize};
use std::collections::HashMap;
use std::process::Command;
use std::sync::Mutex;
use std::time::Instant;
use uciedit::openwrt::{DhcpHost, FirewallRule, FirewallTarget, WifiDevice, WifiInterface};
use uciedit::{dump_all, parse_all, Arena};

pub fn devices<C: CtrlContext>() -> ParentHandler<C> {
    ParentHandler::new()
        .subcommand(
            "list",
            from_fn_async(list)
                .with_display_serializable()
                .with_call_remote::<CliContext>(),
        )
        .subcommand("update", from_fn(update::<C>).no_display())
        .subcommand("block", from_fn(block::<C>).no_display())
        .subcommand("unblock", from_fn(unblock::<C>).no_display())
        .subcommand("forget", from_fn(forget::<C>).no_display())
        .subcommand(
            "data-usage",
            from_fn_async(data_usage)
                .with_display_serializable()
                .with_call_remote::<CliContext>(),
        )
}

// --- Types ---

#[derive(Debug, Serialize, Deserialize, Clone)]
#[serde(rename_all = "lowercase")]
pub enum DeviceStatus {
    Online,
    Offline,
    Blocked,
}

#[derive(Debug, Serialize, Deserialize, Clone)]
pub struct Device {
    pub mac: String,
    pub name: Option<String>,
    pub hostname: Option<String>,
    pub status: DeviceStatus,
    pub connection: Option<String>,
    pub ipv4: Option<String>,
    pub ipv6: Option<String>,
    pub ipv4_static: bool,
    pub ipv6_static: bool,
    pub security_profile: Option<String>,
    pub speed: Option<SpeedData>,
    pub data_usage: Option<f64>,
}

#[derive(Debug, Serialize, Deserialize, Clone)]
pub struct SpeedData {
    pub up: f64,
    pub down: f64,
}

#[derive(Debug, Serialize, Deserialize)]
pub struct DeviceUpdateReq {
    pub mac: String,
    pub name: String,
    pub ipv4_static: bool,
    pub ipv4: String,
    pub ipv6_static: bool,
    pub ipv6: String,
}

#[derive(Debug, Serialize, Deserialize)]
pub struct DeviceMacReq {
    pub mac: String,
}

#[derive(Debug, Serialize, Deserialize)]
#[serde(rename_all = "lowercase")]
pub enum DataUsagePeriod {
    Day,
    Week,
    Month,
    #[serde(rename = "3months")]
    ThreeMonths,
}

#[derive(Debug, Serialize, Deserialize)]
pub struct DataUsageReq {
    pub mac: String,
    pub period: DataUsagePeriod,
}

#[derive(Debug, Serialize, Deserialize)]
pub struct DataUsagePoint {
    pub timestamp: u64,
    pub upload: u64,
    pub download: u64,
}

// --- Traffic cache for speed computation ---

#[derive(Clone)]
struct TrafficSnapshot {
    timestamp: Instant,
    rx_bytes: u64,
    tx_bytes: u64,
}

static TRAFFIC_CACHE: Mutex<Option<HashMap<String, TrafficSnapshot>>> = Mutex::new(None);

// --- Helpers ---

struct ArpEntry {
    ip: String,
    mac: String,
    interface: String,
    state: String,
}

struct DhcpLease {
    mac: String,
    ip: String,
    hostname: String,
}

/// Parse a 32-char hex IPv6 address from /proc/net/if_inet6 into standard notation.
fn parse_proc_ipv6_addr(hex: &str) -> Option<String> {
    if hex.len() != 32 {
        return None;
    }
    let mut bytes = [0u8; 16];
    for i in 0..16 {
        bytes[i] = u8::from_str_radix(&hex[i * 2..i * 2 + 2], 16).ok()?;
    }
    Some(std::net::Ipv6Addr::from(bytes).to_string())
}

fn parse_arp_output(output: &str) -> Vec<ArpEntry> {
    let mut entries = Vec::new();
    for line in output.lines() {
        // Format: IP dev INTERFACE lladdr MAC STATE
        let parts: Vec<&str> = line.split_whitespace().collect();
        if parts.len() >= 6 && parts[2] == "lladdr" {
            // ip neigh: IP dev IFACE lladdr MAC STATE
            // but sometimes: IP dev IFACE lladdr MAC STATE
            // Actual format can vary. Let's parse more carefully.
        }
        // Try regex-like parsing
        if let Some((ip, rest)) = line.split_once(" dev ") {
            if let Some((iface, rest)) = rest.split_once(" lladdr ") {
                let mut rest_parts = rest.split_whitespace();
                if let Some(mac) = rest_parts.next() {
                    let state = rest_parts.next().unwrap_or("UNKNOWN");
                    // Only LAN interfaces
                    if iface.starts_with("br-lan") {
                        entries.push(ArpEntry {
                            ip: ip.trim().to_string(),
                            mac: mac.to_uppercase(),
                            interface: iface.to_string(),
                            state: state.to_string(),
                        });
                    }
                }
            }
        }
    }
    entries
}

fn parse_dhcp_leases(output: &str) -> Vec<DhcpLease> {
    let mut leases = Vec::new();
    for line in output.lines() {
        let line = line.trim();
        if line.is_empty() {
            continue;
        }
        // Format: expiry_timestamp mac_address ip_address hostname client_id
        let parts: Vec<&str> = line.split_whitespace().collect();
        if parts.len() >= 4 {
            leases.push(DhcpLease {
                mac: parts[1].to_uppercase(),
                ip: parts[2].to_string(),
                hostname: parts[3].to_string(),
            });
        }
    }
    leases
}

/// Run a command and return its stdout, or empty string on failure.
fn run_cmd(cmd: &str, args: &[&str]) -> String {
    Command::new(cmd)
        .args(args)
        .output()
        .ok()
        .filter(|o| o.status.success())
        .map(|o| String::from_utf8_lossy(&o.stdout).to_string())
        .unwrap_or_default()
}

/// Parse /proc/net/nf_conntrack and sum bytes per MAC address.
/// Returns HashMap<MAC, (rx_bytes, tx_bytes)>.
///
/// Each conntrack line has two directions:
///   Direction 1 (src=local): bytes= is tx from the local device
///   Direction 2 (src=remote): bytes= is rx by the local device
fn conntrack_bytes_by_mac(
    content: &str,
    ip_to_mac: &HashMap<String, String>,
) -> HashMap<String, (u64, u64)> {
    let mut result: HashMap<String, (u64, u64)> = HashMap::new();

    for line in content.lines() {
        let line = line.trim();
        if line.is_empty() {
            continue;
        }

        // Parse all src= and bytes= fields. A conntrack line has two halves,
        // each with src=, dst=, bytes= fields. We collect them in order.
        let mut srcs: Vec<&str> = Vec::new();
        let mut bytes_vals: Vec<u64> = Vec::new();

        for token in line.split_whitespace() {
            if let Some(val) = token.strip_prefix("src=") {
                srcs.push(val);
            } else if let Some(val) = token.strip_prefix("bytes=") {
                if let Ok(b) = val.parse::<u64>() {
                    bytes_vals.push(b);
                }
            }
        }

        // We need exactly 2 src values and 2 bytes values
        if srcs.len() < 2 || bytes_vals.len() < 2 {
            continue;
        }

        let local_ip = srcs[0]; // direction 1 src = local device
        let tx_bytes = bytes_vals[0]; // direction 1 bytes = tx
        let rx_bytes = bytes_vals[1]; // direction 2 bytes = rx

        if let Some(mac) = ip_to_mac.get(local_ip) {
            let entry = result.entry(mac.clone()).or_insert((0, 0));
            entry.0 += rx_bytes;
            entry.1 += tx_bytes;
        }
    }

    result
}

/// Parse nlbw JSON output. Returns Vec<(mac, rx_bytes, tx_bytes)>.
fn parse_nlbw_json(output: &str) -> Vec<(String, u64, u64)> {
    let parsed: serde_json::Value = match serde_json::from_str(output) {
        Ok(v) => v,
        Err(_) => return Vec::new(),
    };

    let columns = match parsed.get("columns").and_then(|c| c.as_array()) {
        Some(c) => c,
        None => return Vec::new(),
    };
    let data = match parsed.get("data").and_then(|d| d.as_array()) {
        Some(d) => d,
        None => return Vec::new(),
    };

    let mac_idx = columns
        .iter()
        .position(|c| c.as_str() == Some("mac"));
    let rx_idx = columns
        .iter()
        .position(|c| c.as_str() == Some("rx_bytes"));
    let tx_idx = columns
        .iter()
        .position(|c| c.as_str() == Some("tx_bytes"));

    let (mac_idx, rx_idx, tx_idx) = match (mac_idx, rx_idx, tx_idx) {
        (Some(m), Some(r), Some(t)) => (m, r, t),
        _ => return Vec::new(),
    };

    let mut results = Vec::new();
    for row in data {
        if let Some(row) = row.as_array() {
            let mac = row
                .get(mac_idx)
                .and_then(|v| v.as_str())
                .unwrap_or("")
                .to_uppercase();
            let rx = row
                .get(rx_idx)
                .and_then(|v| v.as_u64())
                .unwrap_or(0);
            let tx = row
                .get(tx_idx)
                .and_then(|v| v.as_u64())
                .unwrap_or(0);
            if !mac.is_empty() {
                results.push((mac, rx, tx));
            }
        }
    }
    results
}

/// Discover hostapd interfaces and their connected client MACs,
/// along with radio band info.
fn get_wifi_clients() -> HashMap<String, String> {
    // Maps MAC → connection type ("Wi-Fi 2.4GHz" or "Wi-Fi 5GHz")
    let mut wifi_macs: HashMap<String, String> = HashMap::new();

    // 1. List hostapd interfaces via ubus
    let ubus_list = run_cmd("ubus", &["list"]);
    let hostapd_ifaces: Vec<&str> = ubus_list
        .lines()
        .filter(|l| l.starts_with("hostapd."))
        .collect();

    if hostapd_ifaces.is_empty() {
        return wifi_macs;
    }

    // 2. Get wifi device → band mapping from UCI
    // We'll do this by reading the wireless config
    let mut iface_to_band: HashMap<String, String> = HashMap::new();

    // Read UCI wireless config for device bands
    let arena = Arena::new();
    if let Ok(cfgs) = parse_all("/etc/config", &arena, &["wireless"]) {
        // Build radio → band map
        let mut radio_band: HashMap<String, String> = HashMap::new();
        cfgs["wireless"].each::<WifiDevice, Error>(|name, dev| {
            if let Some(name) = name {
                let band_label = match dev.band.as_str() {
                    "2g" => "Wi-Fi 2.4GHz",
                    "5g" => "Wi-Fi 5GHz",
                    "6g" => "Wi-Fi 6GHz",
                    _ => "Wi-Fi",
                };
                radio_band.insert(name.to_string(), band_label.to_string());
            }
        }).ok();

        // Map wifi-iface section name → band via its device field
        cfgs["wireless"].try_each(|name, iface: WifiInterface| {
            if let Some(name) = name {
                if let Some(band) = radio_band.get(&iface.device) {
                    // hostapd uses the section name as the interface name
                    iface_to_band.insert(name.to_string(), band.clone());
                }
            }
            Ok::<_, Error>(())
        }).ok();
    }

    // 3. Query each hostapd interface for clients
    for hostapd in hostapd_ifaces {
        // hostapd.wlan0 → wlan0
        let wlan_name = hostapd.strip_prefix("hostapd.").unwrap_or(hostapd);
        let band = iface_to_band
            .get(wlan_name)
            .cloned()
            .unwrap_or_else(|| "Wi-Fi".to_string());

        let output = run_cmd("ubus", &["call", hostapd, "get_clients"]);
        if output.is_empty() {
            continue;
        }

        // Parse the JSON response: { "clients": { "MAC": { ... }, ... } }
        if let Ok(parsed) = serde_json::from_str::<serde_json::Value>(&output) {
            if let Some(clients) = parsed.get("clients").and_then(|c| c.as_object()) {
                for mac in clients.keys() {
                    wifi_macs.insert(mac.to_uppercase(), band.clone());
                }
            }
        }
    }

    wifi_macs
}

fn reload_firewall() {
    std::thread::spawn(|| {
        let _ = Command::new("/etc/init.d/firewall")
            .arg("reload")
            .spawn()
            .and_then(|mut c| c.wait());
    });
}

fn reload_dnsmasq() {
    std::thread::spawn(|| {
        let _ = Command::new("/etc/init.d/dnsmasq")
            .arg("reload")
            .spawn()
            .and_then(|mut c| c.wait());
    });
}

fn reload_firewall_and_dnsmasq() {
    std::thread::spawn(|| {
        let _ = Command::new("/etc/init.d/firewall")
            .arg("reload")
            .spawn()
            .and_then(|mut c| c.wait());
        let _ = Command::new("/etc/init.d/dnsmasq")
            .arg("reload")
            .spawn()
            .and_then(|mut c| c.wait());
    });
}

// --- Handlers ---

/// Identify non-WiFi MACs that need reachability verification.
///
/// Returns:
/// - `probe_targets`: IPv4 (ip, mac) pairs to ping-probe
/// - `ipv6_only_macs`: MACs with only IPv6 neighbor entries (no IPv4 to probe)
///    — these are treated as unreachable since we can't verify them
///
/// Active WiFi clients are skipped entirely — hostapd is authoritative.
fn non_wifi_probe_candidates(
    arp_entries: &[ArpEntry],
    wifi_clients: &HashMap<String, String>,
) -> (Vec<(String, String)>, std::collections::HashSet<String>) {
    // Collect MACs that have any REACHABLE entry.
    let reachable_macs: std::collections::HashSet<&str> = arp_entries
        .iter()
        .filter(|e| e.state == "REACHABLE")
        .map(|e| e.mac.as_str())
        .collect();

    // Track which non-WiFi MACs have any alive neighbor entry, and which
    // of those have at least one IPv4 address we can probe.
    let mut alive_non_wifi_macs = std::collections::HashSet::new();
    let mut has_ipv4 = std::collections::HashSet::new();

    let mut targets = Vec::new();
    let mut seen = std::collections::HashSet::new();
    for entry in arp_entries {
        // Skip WiFi clients (hostapd is authoritative for them).
        if wifi_clients.contains_key(&entry.mac) {
            continue;
        }
        // Only consider alive states.
        if !matches!(
            entry.state.as_str(),
            "REACHABLE" | "STALE" | "DELAY" | "PROBE"
        ) {
            continue;
        }
        alive_non_wifi_macs.insert(entry.mac.clone());
        // Only probe IPv4 (ping6 would add complexity).
        if entry.ip.contains(':') {
            continue;
        }
        has_ipv4.insert(entry.mac.clone());
        match entry.state.as_str() {
            // STALE non-WiFi entries need probing, unless they also have a
            // REACHABLE entry (kernel already confirmed them).
            "STALE" if reachable_macs.contains(entry.mac.as_str()) => continue,
            "STALE" => {}
            // REACHABLE entries are normally trusted, but if the MAC was
            // recently a WiFi client (not in hostapd anymore, not in
            // wifi_clients), the kernel's REACHABLE state is stale — probe it.
            "REACHABLE" => {}
            _ => continue,
        }
        // One probe per IP is enough.
        if seen.insert(entry.ip.clone()) {
            targets.push((entry.ip.clone(), entry.mac.clone()));
        }
    }

    // MACs with only IPv6 neighbor entries can't be probed — treat as
    // unreachable so lingering NDP entries don't resurrect offline devices.
    let ipv6_only = alive_non_wifi_macs
        .difference(&has_ipv4)
        .cloned()
        .collect();

    (targets, ipv6_only)
}

/// Ping STALE IPs concurrently and return the set of MACs that did NOT reply.
/// A MAC is only considered unreachable if ALL of its probed IPs failed.
fn ping_unreachable_macs(targets: Vec<(String, String)>) -> std::collections::HashSet<String> {
    let mut results: Vec<(String, std::process::Child)> = Vec::new();
    for (ip, mac) in &targets {
        if let Ok(child) = Command::new("ping")
            .args(["-c", "1", "-W", "1", ip])
            .stdout(std::process::Stdio::null())
            .stderr(std::process::Stdio::null())
            .spawn()
        {
            results.push((mac.clone(), child));
        }
    }
    // Track which MACs were probed and which responded to at least one ping.
    let mut probed = std::collections::HashSet::new();
    let mut responded = std::collections::HashSet::new();
    for (mac, mut child) in results {
        probed.insert(mac.clone());
        let ok = child.wait().map(|s| s.success()).unwrap_or(false);
        if ok {
            responded.insert(mac);
        }
    }
    probed.difference(&responded).cloned().collect()
}

pub async fn list(_ctx: ServerContext) -> Result<Vec<Device>, Error> {
    // --- Phase 1: IPv6 multicast discovery (all pings concurrent, ~1s) ---
    //
    // Ping ff02::1 to populate the NDP neighbor table. SLAAC is stateless —
    // the router never learns which addresses clients configured. All pings
    // fire concurrently so wall-clock time is 1s regardless of interface count.
    let _ = tokio::task::spawn_blocking(|| {
        // When bridge VLAN filtering is active, br-lan.X interfaces exist for
        // each profile VLAN — use those exclusively. Multicast on the bridge
        // master (br-lan) leaks out ALL member ports including WAN, which
        // causes upstream devices to appear as LAN clients.
        // When no VLANs exist (single admin profile), fall back to br-lan.
        let mut ifaces = Vec::new();
        if let Ok(entries) = std::fs::read_dir("/sys/class/net") {
            for entry in entries.flatten() {
                let name = entry.file_name().to_string_lossy().to_string();
                if name.starts_with("br-lan.") {
                    ifaces.push(name);
                }
            }
        }
        if ifaces.is_empty() {
            ifaces.push("br-lan".to_string());
        }

        let mut children = Vec::new();

        // Link-local discovery on each LAN VLAN interface.
        for iface in &ifaces {
            if let Ok(child) = Command::new("ping6")
                .args(["-c", "1", "-W", "1", "-I", iface, "ff02::1"])
                .stdout(std::process::Stdio::null())
                .stderr(std::process::Stdio::null())
                .spawn()
            {
                children.push(child);
            }
        }

        // ULA/global discovery — ping from each interface's global IPv6
        // address so clients respond from their ULA addresses (RFC 6724).
        if let Ok(content) = std::fs::read_to_string("/proc/net/if_inet6") {
            for line in content.lines() {
                let parts: Vec<&str> = line.split_whitespace().collect();
                // Format: addr_hex ifindex prefix_len scope flags ifname
                // scope "00" = global
                if parts.len() >= 6 && parts[3] == "00" && ifaces.iter().any(|i| i == parts[5]) {
                    if let Some(addr) = parse_proc_ipv6_addr(parts[0]) {
                        let dest = format!("ff02::1%{}", parts[5]);
                        if let Ok(child) = Command::new("ping6")
                            .args(["-c", "1", "-W", "1", "-I", &addr, &dest])
                            .stdout(std::process::Stdio::null())
                            .stderr(std::process::Stdio::null())
                            .spawn()
                        {
                            children.push(child);
                        }
                    }
                }
            }
        }

        for mut child in children {
            let _ = child.wait();
        }
    })
    .await;

    // --- Phase 2a: Initial data gather (parallel) ---
    //
    // We need ARP + WiFi clients first to identify which STALE entries need
    // probing. UCI parsing has no dependency on any of these, so it runs in
    // parallel too.
    let (arp_output, wifi_clients, uci_result) = tokio::join!(
        tokio::task::spawn_blocking(|| run_cmd("ip", &["neigh", "show"])),
        tokio::task::spawn_blocking(get_wifi_clients),
        tokio::task::spawn_blocking(|| -> Result<_, Error> {
            let arena = Arena::new();
            let cfgs = parse_all("/etc/config", &arena, &["dhcp", "firewall", "startwrt", "network"])?;

            let mut hosts_by_mac: HashMap<String, DhcpHost> = HashMap::new();
            cfgs["dhcp"].each::<DhcpHost, Error>(|_, host| {
                hosts_by_mac.insert(host.mac.to_uppercase(), host);
            })?;

            let mut blocked_macs: HashMap<String, bool> = HashMap::new();
            cfgs["firewall"].each::<FirewallRule, Error>(|_, rule| {
                if matches!(rule.target, FirewallTarget::REJECT | FirewallTarget::DROP) {
                    if let Some(mac) = rule.src_mac {
                        blocked_macs.insert(mac.to_uppercase(), true);
                    }
                }
            })?;

            let lookup = Lookup::parse(ServerContext, &cfgs)?;
            let profiles: HashMap<u16, String> = lookup
                .list()
                .iter()
                .map(|p| (p.vlan_tag, p.fullname.clone()))
                .collect();

            Ok::<_, Error>((hosts_by_mac, blocked_macs, profiles))
        }),
    );

    let arp_output = arp_output.unwrap_or_default();
    let wifi_clients = wifi_clients.unwrap_or_default();
    let initial_arp = parse_arp_output(&arp_output);

    // --- Phase 2b: Probe non-WiFi entries + remaining data (parallel) ---
    //
    // For any non-WiFi device (STALE or REACHABLE), ping it to check if it's
    // actually present. This catches both aged-out entries AND recently
    // disconnected WiFi devices whose ARP is still REACHABLE but hostapd has
    // already dropped them. MACs with only IPv6 neighbor entries (no IPv4 to
    // probe) are treated as unreachable. Runs concurrently with nlbw,
    // conntrack, and lease reads so it adds zero net latency.
    let (probe_targets, ipv6_only_macs) =
        non_wifi_probe_candidates(&initial_arp, &wifi_clients);
    let (unreachable_macs, leases_output, nlbw_output, conntrack_output) = tokio::join!(
        tokio::task::spawn_blocking(move || ping_unreachable_macs(probe_targets)),
        tokio::task::spawn_blocking(|| {
            std::fs::read_to_string("/tmp/dhcp.leases").unwrap_or_default()
        }),
        tokio::task::spawn_blocking(|| run_cmd("nlbw", &["-c", "json", "-g", "mac"])),
        tokio::task::spawn_blocking(|| {
            std::fs::read_to_string("/proc/net/nf_conntrack").unwrap_or_default()
        }),
    );

    let mut unreachable_macs = unreachable_macs.unwrap_or_default();
    unreachable_macs.extend(ipv6_only_macs);
    let leases_output = leases_output.unwrap_or_default();
    let nlbw_output = nlbw_output.unwrap_or_default();
    let conntrack_output = conntrack_output.unwrap_or_default();

    // Use the initial ARP snapshot — no need to re-read since we use ping
    // exit codes (not kernel NUD state) to determine reachability.
    let arp_entries = initial_arp;
    let dhcp_leases = parse_dhcp_leases(&leases_output);

    // Parse nlbw data for cumulative data usage totals
    let nlbw_data = parse_nlbw_json(&nlbw_output);
    let mut nlbw_by_mac: HashMap<String, (u64, u64)> = HashMap::new();
    for (mac, rx, tx) in &nlbw_data {
        let entry = nlbw_by_mac.entry(mac.clone()).or_insert((0, 0));
        entry.0 += rx;
        entry.1 += tx;
    }

    // Build IP → MAC map from ARP entries for conntrack lookup
    let ip_to_mac: HashMap<String, String> = arp_entries
        .iter()
        .map(|e| (e.ip.clone(), e.mac.clone()))
        .collect();

    // Parse conntrack for real-time byte counters
    let conntrack_by_mac = conntrack_bytes_by_mac(&conntrack_output, &ip_to_mac);

    // Compute speed from conntrack traffic cache
    let now = Instant::now();
    let mut speeds: HashMap<String, SpeedData> = HashMap::new();
    {
        let mut cache = TRAFFIC_CACHE.lock().unwrap();
        let prev = cache.take().unwrap_or_default();
        let mut new_cache = prev.clone();

        for (mac, &(rx, tx)) in &conntrack_by_mac {
            if let Some(prev_snap) = new_cache.get(mac) {
                let elapsed = now.duration_since(prev_snap.timestamp).as_secs_f64();
                // Only compute speed if enough time has passed. Use saturating_sub
                // per-direction since conntrack totals can decrease when connections close.
                if elapsed > 1.0 {
                    let rx_delta = rx.saturating_sub(prev_snap.rx_bytes);
                    let tx_delta = tx.saturating_sub(prev_snap.tx_bytes);
                    // Convert to MB/s
                    let down = rx_delta as f64 / elapsed / 1_048_576.0;
                    let up = tx_delta as f64 / elapsed / 1_048_576.0;
                    speeds.insert(
                        mac.clone(),
                        SpeedData {
                            up: (up * 10.0).round() / 10.0,
                            down: (down * 10.0).round() / 10.0,
                        },
                    );
                }
            }
            new_cache.insert(
                mac.clone(),
                TrafficSnapshot {
                    timestamp: now,
                    rx_bytes: rx,
                    tx_bytes: tx,
                },
            );
        }

        *cache = Some(new_cache);
    }

    // Unpack UCI results
    let (hosts_by_mac, blocked_macs, profile_by_vlan) = uci_result.unwrap_or_else(|_| {
        Err(Error::from(std::io::Error::new(
            std::io::ErrorKind::Other,
            "UCI parse failed in background task",
        )))
    })?;

    // Build ARP index
    let mut arp_by_mac: HashMap<String, Vec<&ArpEntry>> = HashMap::new();
    for entry in &arp_entries {
        arp_by_mac
            .entry(entry.mac.clone())
            .or_default()
            .push(entry);
    }

    // Lease index
    let mut lease_by_mac: HashMap<String, &DhcpLease> = HashMap::new();
    for lease in &dhcp_leases {
        lease_by_mac.insert(lease.mac.clone(), lease);
    }

    // Collect all unique MACs
    let mut all_macs: std::collections::BTreeSet<String> = std::collections::BTreeSet::new();
    for mac in arp_by_mac.keys() {
        all_macs.insert(mac.clone());
    }
    for mac in lease_by_mac.keys() {
        all_macs.insert(mac.clone());
    }
    for mac in hosts_by_mac.keys() {
        all_macs.insert(mac.clone());
    }
    for mac in blocked_macs.keys() {
        all_macs.insert(mac.clone());
    }

    // --- Phase 4: Build device list ---
    let mut devices = Vec::new();
    for mac in &all_macs {
        let arp_list = arp_by_mac.get(mac).cloned().unwrap_or_default();
        let lease = lease_by_mac.get(mac);
        let host = hosts_by_mac.get(mac);
        let is_blocked = blocked_macs.contains_key(mac);

        // Status: STALE entries for devices that failed the ping probe are
        // treated as offline. WiFi clients skip probing (hostapd is
        // authoritative) so they remain online via their STALE entries.
        let status = if is_blocked {
            DeviceStatus::Blocked
        } else if unreachable_macs.contains(mac) {
            DeviceStatus::Offline
        } else if arp_list
            .iter()
            .any(|e| matches!(e.state.as_str(), "REACHABLE" | "STALE" | "DELAY" | "PROBE"))
        {
            DeviceStatus::Online
        } else {
            DeviceStatus::Offline
        };

        // IPs from ARP
        let ipv4 = arp_list
            .iter()
            .find(|e| !e.ip.contains(':'))
            .map(|e| e.ip.clone())
            .or_else(|| lease.map(|l| l.ip.clone()));
        let ipv6 = arp_list
            .iter()
            .find(|e| e.ip.contains(':') && !e.ip.starts_with("fe80:"))
            .map(|e| e.ip.clone());

        // Profile from VLAN tag
        let vlan_tag = arp_list
            .first()
            .and_then(|e| {
                e.interface
                    .split('.')
                    .nth(1)
                    .and_then(|s| s.parse::<u16>().ok())
            })
            .unwrap_or(1);
        let security_profile = profile_by_vlan.get(&vlan_tag).cloned();

        // Name
        let name = host
            .and_then(|h| h.name.clone())
            .or_else(|| {
                lease.and_then(|l| {
                    if l.hostname != "*" {
                        Some(l.hostname.clone())
                    } else {
                        None
                    }
                })
            });
        let hostname = lease.map(|l| l.hostname.clone());

        // Connection type
        let connection = if matches!(status, DeviceStatus::Online) {
            wifi_clients
                .get(mac)
                .cloned()
                .or_else(|| {
                    if !arp_list.is_empty() {
                        Some("Ethernet".to_string())
                    } else {
                        None
                    }
                })
        } else {
            None
        };

        // Speed (only for online)
        let speed = if matches!(status, DeviceStatus::Online) {
            speeds.get(mac).cloned()
        } else {
            None
        };

        // Data usage (GB)
        let data_usage = nlbw_by_mac.get(mac).map(|(rx, tx)| {
            ((rx + tx) as f64 / 1_073_741_824.0 * 10.0).round() / 10.0
        });

        devices.push(Device {
            mac: mac.clone(),
            name,
            hostname,
            status,
            connection,
            ipv4,
            ipv6,
            ipv4_static: host.map(|h| h.ip.is_some()).unwrap_or(false),
            ipv6_static: host.map(|h| h.hostid.is_some()).unwrap_or(false),
            security_profile,
            speed,
            data_usage,
        });
    }

    Ok(devices)
}

pub fn update<C: CtrlContext>(
    ctx: C,
    DeserializeStdin(req): DeserializeStdin<DeviceUpdateReq>,
) -> Result<(), Error> {
    let mac_upper = req.mac.to_uppercase();
    let mut retries = 4;
    loop {
        let arena = Arena::new();
        let mut cfgs = parse_all(ctx.uci_root(), &arena, &["dhcp"])?;

        let mut found = false;
        for section in &mut cfgs["dhcp"].sections {
            if let Ok(mut host) = section.get::<DhcpHost>() {
                if host.mac.to_uppercase() == mac_upper {
                    host.name = Some(req.name.clone());
                    if req.ipv4_static && !req.ipv4.is_empty() {
                        host.ip = Some(req.ipv4.clone());
                    } else {
                        host.ip = None;
                    }
                    if req.ipv6_static && !req.ipv6.is_empty() {
                        host.hostid = Some(extract_ipv6_hostid(&req.ipv6));
                    } else {
                        host.hostid = None;
                    }
                    section.set(&host)?;
                    found = true;
                    break;
                }
            }
        }

        if !found {
            // Create new host section
            let new_host = DhcpHost {
                mac: req.mac.clone(),
                name: Some(req.name.clone()),
                ip: if req.ipv4_static && !req.ipv4.is_empty() {
                    Some(req.ipv4.clone())
                } else {
                    None
                },
                hostid: if req.ipv6_static && !req.ipv6.is_empty() {
                    Some(extract_ipv6_hostid(&req.ipv6))
                } else {
                    None
                },
                dns: Some("1".to_string()),
            };
            let section_name = format!(
                "host_{}",
                req.mac.replace(':', "").to_lowercase()
            );
            cfgs["dhcp"].append(&new_host, Some(&section_name))?;
        }

        match dump_all(ctx.uci_root(), cfgs) {
            Err(uciedit::Error::Conflict { .. }) if retries > 0 => {
                retries -= 1;
                continue;
            }
            Err(err) => return Err(err.into()),
            Ok(()) => {
                if ctx.effectful() {
                    reload_dnsmasq();
                }
                return Ok(());
            }
        }
    }
}

pub fn block<C: CtrlContext>(
    ctx: C,
    DeserializeStdin(req): DeserializeStdin<DeviceMacReq>,
) -> Result<(), Error> {
    let mac_upper = req.mac.to_uppercase();
    let mut retries = 4;
    loop {
        let arena = Arena::new();
        let mut cfgs = parse_all(ctx.uci_root(), &arena, &["dhcp", "firewall"])?;

        // Check if already blocked
        let mut already_blocked = false;
        cfgs["firewall"].each::<FirewallRule, Error>(|_, rule| {
            if matches!(rule.target, FirewallTarget::REJECT | FirewallTarget::DROP) {
                if let Some(ref src_mac) = rule.src_mac {
                    if src_mac.to_uppercase() == mac_upper {
                        already_blocked = true;
                    }
                }
            }
        })?;

        if !already_blocked {
            let rule_name = format!(
                "block_{}",
                req.mac.replace(':', "").to_lowercase()
            );
            let block_rule = FirewallRule {
                name: format!("Block {}", req.mac),
                src: "lan".to_string(),
                dest: Some("wan".to_string()),
                src_mac: Some(req.mac.clone()),
                target: FirewallTarget::REJECT,
                ..Default::default()
            };
            cfgs["firewall"].append(&block_rule, Some(&rule_name))?;
        }

        // Remove static IP from DHCP host (keep name)
        for section in &mut cfgs["dhcp"].sections {
            if let Ok(mut host) = section.get::<DhcpHost>() {
                if host.mac.to_uppercase() == mac_upper {
                    host.ip = None;
                    host.hostid = None;
                    section.set(&host)?;
                    break;
                }
            }
        }

        match dump_all(ctx.uci_root(), cfgs) {
            Err(uciedit::Error::Conflict { .. }) if retries > 0 => {
                retries -= 1;
                continue;
            }
            Err(err) => return Err(err.into()),
            Ok(()) => {
                if ctx.effectful() {
                    reload_firewall_and_dnsmasq();
                }
                return Ok(());
            }
        }
    }
}

pub fn unblock<C: CtrlContext>(
    ctx: C,
    DeserializeStdin(req): DeserializeStdin<DeviceMacReq>,
) -> Result<(), Error> {
    let mac_upper = req.mac.to_uppercase();
    let mut retries = 4;
    loop {
        let arena = Arena::new();
        let mut cfgs = parse_all(ctx.uci_root(), &arena, &["firewall"])?;

        cfgs["firewall"].sections.retain(|section| {
            if let Ok(rule) = section.get::<FirewallRule>() {
                if matches!(rule.target, FirewallTarget::REJECT | FirewallTarget::DROP) {
                    if let Some(ref src_mac) = rule.src_mac {
                        if src_mac.to_uppercase() == mac_upper {
                            return false;
                        }
                    }
                }
            }
            true
        });

        match dump_all(ctx.uci_root(), cfgs) {
            Err(uciedit::Error::Conflict { .. }) if retries > 0 => {
                retries -= 1;
                continue;
            }
            Err(err) => return Err(err.into()),
            Ok(()) => {
                if ctx.effectful() {
                    reload_firewall();
                }
                return Ok(());
            }
        }
    }
}

pub fn forget<C: CtrlContext>(
    ctx: C,
    DeserializeStdin(req): DeserializeStdin<DeviceMacReq>,
) -> Result<(), Error> {
    let mac_upper = req.mac.to_uppercase();
    let mut retries = 4;
    loop {
        let arena = Arena::new();
        let mut cfgs = parse_all(ctx.uci_root(), &arena, &["dhcp", "firewall"])?;

        // Remove DHCP host
        cfgs["dhcp"].sections.retain(|section| {
            if let Ok(host) = section.get::<DhcpHost>() {
                if host.mac.to_uppercase() == mac_upper {
                    return false;
                }
            }
            true
        });

        // Remove firewall block rules
        cfgs["firewall"].sections.retain(|section| {
            if let Ok(rule) = section.get::<FirewallRule>() {
                if matches!(rule.target, FirewallTarget::REJECT | FirewallTarget::DROP) {
                    if let Some(ref src_mac) = rule.src_mac {
                        if src_mac.to_uppercase() == mac_upper {
                            return false;
                        }
                    }
                }
            }
            true
        });

        match dump_all(ctx.uci_root(), cfgs) {
            Err(uciedit::Error::Conflict { .. }) if retries > 0 => {
                retries -= 1;
                continue;
            }
            Err(err) => return Err(err.into()),
            Ok(()) => {
                if ctx.effectful() {
                    reload_firewall_and_dnsmasq();
                }
                return Ok(());
            }
        }
    }
}

pub async fn data_usage(
    _ctx: ServerContext,
    DeserializeStdin(req): DeserializeStdin<DataUsageReq>,
) -> Result<Vec<DataUsagePoint>, Error> {
    let mac_upper = req.mac.to_uppercase();

    // Calculate start date
    let days_back: i64 = match req.period {
        DataUsagePeriod::Day => 1,
        DataUsagePeriod::Week => 7,
        DataUsagePeriod::Month => 30,
        DataUsagePeriod::ThreeMonths => 90,
    };

    let now = std::time::SystemTime::now()
        .duration_since(std::time::UNIX_EPOCH)
        .unwrap()
        .as_secs();
    let start_secs = now - (days_back as u64 * 86400);

    // Format as YYYY-MM-DD
    let start_date = {
        let days_since_epoch = start_secs / 86400;
        // Simple date calculation
        let (y, m, d) = days_to_ymd(days_since_epoch);
        format!("{:04}-{:02}-{:02}", y, m, d)
    };

    let output = tokio::task::spawn_blocking(move || {
        run_cmd("nlbw", &["-c", "json", "-g", "mac,interval", "-t", &start_date])
    })
    .await
    .unwrap_or_default();

    if output.is_empty() {
        return Ok(Vec::new());
    }

    // Parse and filter for this MAC
    let parsed: serde_json::Value = match serde_json::from_str(&output) {
        Ok(v) => v,
        Err(_) => return Ok(Vec::new()),
    };

    let columns = match parsed.get("columns").and_then(|c| c.as_array()) {
        Some(c) => c,
        None => return Ok(Vec::new()),
    };
    let data = match parsed.get("data").and_then(|d| d.as_array()) {
        Some(d) => d,
        None => return Ok(Vec::new()),
    };

    let mac_idx = columns.iter().position(|c| c.as_str() == Some("mac"));
    let rx_idx = columns.iter().position(|c| c.as_str() == Some("rx_bytes"));
    let tx_idx = columns.iter().position(|c| c.as_str() == Some("tx_bytes"));
    let time_idx = columns
        .iter()
        .position(|c| c.as_str() == Some("interval_start"));

    let (mac_idx, rx_idx, tx_idx) = match (mac_idx, rx_idx, tx_idx) {
        (Some(m), Some(r), Some(t)) => (m, r, t),
        _ => return Ok(Vec::new()),
    };

    let mut points = Vec::new();
    for row in data {
        if let Some(row) = row.as_array() {
            let row_mac = row
                .get(mac_idx)
                .and_then(|v| v.as_str())
                .unwrap_or("")
                .to_uppercase();
            if row_mac == mac_upper {
                let timestamp = time_idx
                    .and_then(|i| row.get(i))
                    .and_then(|v| v.as_u64())
                    .unwrap_or(now);
                let download = row.get(rx_idx).and_then(|v| v.as_u64()).unwrap_or(0);
                let upload = row.get(tx_idx).and_then(|v| v.as_u64()).unwrap_or(0);
                points.push(DataUsagePoint {
                    timestamp,
                    upload,
                    download,
                });
            }
        }
    }

    Ok(points)
}

/// Extract the last 4 hextets from an IPv6 address for use as hostid.
fn extract_ipv6_hostid(ipv6: &str) -> String {
    let parts: Vec<&str> = ipv6.split(':').collect();
    if parts.len() >= 4 {
        parts[parts.len() - 4..].join(":")
    } else {
        ipv6.to_string()
    }
}

/// Convert days since Unix epoch to (year, month, day).
fn days_to_ymd(days: u64) -> (u64, u64, u64) {
    // Adapted from Howard Hinnant's algorithm
    let z = days + 719468;
    let era = z / 146097;
    let doe = z - era * 146097;
    let yoe = (doe - doe / 1460 + doe / 36524 - doe / 146096) / 365;
    let y = yoe + era * 400;
    let doy = doe - (365 * yoe + yoe / 4 - yoe / 100);
    let mp = (5 * doy + 2) / 153;
    let d = doy - (153 * mp + 2) / 5 + 1;
    let m = if mp < 10 { mp + 3 } else { mp - 9 };
    let y = if m <= 2 { y + 1 } else { y };
    (y, m, d)
}
