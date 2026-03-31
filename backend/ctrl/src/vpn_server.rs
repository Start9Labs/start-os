use crate::profiles::{self, ProfileIdOpt, reload_system};
use crate::utils::{DeserializeStdin, HandlerExtSerde};
use crate::wg::{Base64, WgKey, generate_psk};
use crate::{CliContext, Error, ErrorKind, ServerContext};
use clap::Parser;
use rpc_toolkit::{from_fn, HandlerExt, ParentHandler};
use serde::{Deserialize, Serialize};
use std::collections::{BTreeMap, BTreeSet};
use std::net::Ipv4Addr;
use std::process::Command;
use uciedit::openwrt::{DhcpHost, NetworkInterface, NetworkRoute};
use uciedit::{dump_all, parse_all, Arena, Configs, Line, LineComment, Section, Token, TypedSection};

// === IP Allocation Constants ===
// VPN server uses .254, peers use .200-.253 (54 peers max)
// This range is after the default DHCP pool (.2-.199) to avoid conflicts
const VPN_SERVER_HOST_OCTET: u8 = 254;
const PEER_IP_START: u8 = 200;
const PEER_IP_END: u8 = 253;

/// WireGuard interface section in /etc/config/network
#[derive(Debug, TypedSection)]
#[uci(ty = "interface")]
pub struct WgInterface {
    #[uci(rename = "proto")]
    pub proto: String,
    pub private_key: String,
    #[uci(default)]
    pub listen_port: Option<u16>,
    #[uci(default)]
    pub addresses: Vec<String>,
    #[uci(default)]
    pub disabled: Option<String>,
}

impl WgInterface {
    pub fn is_wireguard(&self) -> bool {
        self.proto == "wireguard"
    }

    pub fn disabled(&self) -> bool {
        self.disabled.as_deref() == Some("1")
    }
}

/// VPN server metadata stored in /etc/config/startwrt
#[derive(Debug, TypedSection)]
#[uci(ty = "vpn_server")]
pub struct UciVpnServer {
    /// WireGuard interface name (e.g., "wg_lan")
    pub interface: String,
    /// Profile interface name (e.g., "lan")
    pub profile_interface: String,
    /// Human-readable label
    pub label: String,
    /// Listen port for WireGuard
    pub listen_port: u16,
    /// Public endpoint address for client connections (hostname or IP)
    pub endpoint: String,
}

// === Public API Types ===

/// Peer configuration for VPN server
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct VpnServerPeer {
    /// Human-readable name for the peer
    pub name: String,
    /// Assigned IP address within the profile's subnet (auto-allocated if not specified)
    #[serde(skip_serializing_if = "Option::is_none")]
    pub ip: Option<Ipv4Addr>,
    /// Public key. If not provided when adding a peer, a key pair will be generated.
    #[serde(skip_serializing_if = "Option::is_none")]
    pub public_key: Option<String>,
    /// Pre-shared key (auto-generated if not provided)
    #[serde(skip_serializing_if = "Option::is_none")]
    pub preshared_key: Option<String>,
    /// Route all traffic (LAN + WAN) through the tunnel. Default (false/absent) = split tunnel (LAN only).
    #[serde(skip_serializing_if = "Option::is_none")]
    pub route_all: Option<bool>,
}

/// VPN server configuration returned by list (excludes sensitive data)
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct VpnServer {
    /// Profile interface name this VPN server is associated with (e.g., "lan", "guest")
    pub profile: String,
    /// Human-readable label for the VPN server
    pub label: String,
    /// Whether the VPN server is enabled (listening)
    pub enabled: bool,
    /// Listen port for WireGuard connections
    pub listen_port: u16,
    /// Public endpoint address for inbound client connections (hostname or IP)
    pub endpoint: String,
    /// Server's public key
    pub public_key: String,
    /// Server's VPN address (derived from profile's gateway)
    pub server_address: Ipv4Addr,
    /// Connected peers
    pub peers: Vec<VpnServerPeer>,
}

/// VPN server configuration for set/create requests
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct VpnServerConfig {
    /// Human-readable label for the VPN server
    pub label: String,
    /// Whether the VPN server is enabled (listening)
    pub enabled: bool,
    /// Listen port for WireGuard connections
    pub listen_port: u16,
    /// Public endpoint address for inbound client connections (hostname or IP)
    pub endpoint: String,
    /// Private key. If not provided, a new key will be generated.
    #[serde(skip_serializing_if = "Option::is_none")]
    pub private_key: Option<String>,
}

/// Response containing all VPN servers
#[derive(Debug, Serialize, Deserialize)]
pub struct VpnServers {
    pub servers: Vec<VpnServer>,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct SetArgs {
    /// Profile interface name (e.g., "lan", "guest")
    pub profile: String,
    /// VPN server configuration
    pub config: VpnServerConfig,
}

#[derive(Debug, Clone, Serialize, Deserialize, Parser)]
pub struct DeleteArgs {
    /// Profile interface name
    #[clap(short, long)]
    pub profile: String,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct PeerAddArgs {
    /// Profile interface name (e.g., "lan", "guest")
    pub profile: String,
    /// Peer configuration
    pub peer: VpnServerPeer,
}

#[derive(Debug, Clone, Serialize, Deserialize, Parser)]
pub struct PeerDeleteArgs {
    /// Profile interface name
    #[clap(short, long)]
    pub profile: String,
    /// Public key of the peer to delete
    #[clap(long)]
    pub public_key: String,
}

/// Response from adding a peer, contains client config if keys were generated
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct PeerAddResponse {
    /// WireGuard client configuration (only present if keys were generated server-side)
    #[serde(skip_serializing_if = "Option::is_none")]
    pub client_config: Option<String>,
    /// The public key of the peer
    pub public_key: String,
    /// The assigned IP address
    pub ip: Ipv4Addr,
}

// === API Handler ===

pub fn vpn_server<C: rpc_toolkit::Context>() -> ParentHandler<C> {
    ParentHandler::new()
        .subcommand(
            "list",
            from_fn(list)
                .with_display_serializable()
                .with_call_remote::<CliContext>(),
        )
        .subcommand(
            "set",
            from_fn(set)
                .with_display_serializable()
                .with_call_remote::<CliContext>(),
        )
        .subcommand(
            "delete",
            from_fn(delete)
                .with_display_serializable()
                .with_call_remote::<CliContext>(),
        )
        .subcommand(
            "peer-add",
            from_fn(peer_add)
                .with_display_serializable()
                .with_call_remote::<CliContext>(),
        )
        .subcommand(
            "peer-delete",
            from_fn(peer_delete)
                .with_display_serializable()
                .with_call_remote::<CliContext>(),
        )
}

// === Implementation ===

/// Restart a WireGuard interface to apply peer changes.
/// This is more reliable than network reload for WireGuard peer updates.
fn restart_wireguard_interface(interface_name: &str) -> Result<(), Error> {
    // ifdown may fail if interface isn't up yet — that's fine
    let _ = crate::run_quiet(Command::new("ifdown").arg(interface_name));

    // ifup must succeed for the new config to take effect
    let status = crate::run_quiet(Command::new("ifup").arg(interface_name))?;
    if !status.success() {
        return Err(Error::other(format!(
            "ifup {} failed with exit code {:?}",
            interface_name,
            status.code()
        )));
    }

    Ok(())
}

// === Input Validation ===

/// Validate that an endpoint string is safe for use in UCI config and WireGuard client configs.
/// Rejects empty strings, newlines, and whitespace to prevent config injection.
fn validate_endpoint(endpoint: &str) -> Result<(), Error> {
    if endpoint.is_empty()
        || endpoint.contains('\n')
        || endpoint.contains('\r')
        || endpoint.contains(char::is_whitespace)
    {
        return Err(ErrorKind::InvalidValue {
            field: "endpoint".into(),
            value: endpoint.into(),
        }
        .into());
    }
    Ok(())
}

/// Validate that a peer name is safe for use in UCI config and WireGuard client configs.
fn validate_peer_name(name: &str) -> Result<(), Error> {
    if name.contains('\n') || name.contains('\r') {
        return Err(ErrorKind::InvalidValue {
            field: "name".into(),
            value: name.into(),
        }
        .into());
    }
    Ok(())
}

/// Validate that a user-supplied private key is valid base64 and exactly 32 bytes.
fn validate_private_key(key: &str) -> Result<(), Error> {
    let _: Base64<WgKey> = key.parse().map_err(|_| ErrorKind::InvalidValue {
        field: "private_key".into(),
        value: "[redacted]".into(),
    })?;
    Ok(())
}

/// Derive VPN server address from profile's gateway IP
/// Gateway is typically .1, VPN server uses .254
fn derive_server_address(gateway_ip: Ipv4Addr) -> Ipv4Addr {
    let octets = gateway_ip.octets();
    Ipv4Addr::new(octets[0], octets[1], octets[2], VPN_SERVER_HOST_OCTET)
}

/// Extract gateway IP for a profile from already-parsed configs.
/// Avoids redundant re-parsing that `profiles::get()` would do.
fn get_gateway_ip(cfgs: &Configs, interface_name: &str) -> Result<Ipv4Addr, Error> {
    use uciedit::openwrt::NetworkInterface;

    for section in &cfgs["network"].sections {
        let Ok(iface) = section.get::<NetworkInterface>() else { continue };
        let Some(name) = section.name() else { continue };
        if name == interface_name {
            return iface.ipaddr.ok_or_else(|| Error::other(
                format!("No IP address configured for interface {}", interface_name)
            ));
        }
    }
    Err(Error::other(format!("Network interface {} not found", interface_name)))
}

/// Get the set of allocated peer IPs for a VPN server from WireGuard peer configs
fn get_vpn_peer_ips(cfgs: &Configs, wg_interface_name: &str) -> BTreeSet<Ipv4Addr> {
    let peer_type = format!("wireguard_{}", wg_interface_name);

    cfgs["network"]
        .sections
        .iter()
        .filter(|section| section.ty() == peer_type)
        .filter_map(|section| {
            for line in &section.lines {
                if let Line::List { list, item, .. } = line {
                    if list.as_str() == "allowed_ips" {
                        // Parse IP from "x.x.x.x/32" format
                        let ip_str = item.as_str();
                        if let Some(ip_part) = ip_str.split('/').next() {
                            if let Ok(ip) = ip_part.parse::<Ipv4Addr>() {
                                return Some(ip);
                            }
                        }
                    }
                }
            }
            None
        })
        .collect()
}

/// Sync /32 host routes for VPN peers in the profile's policy routing table.
///
/// When a profile uses an outbound VPN, policy routing directs traffic from the
/// profile's subnet through the VPN tunnel. The policy table includes a /24 subnet
/// route for local LAN traffic, but this also catches VPN peer IPs — causing the
/// router's locally-generated responses (DNS, HTTP) to be sent to the LAN bridge
/// instead of back through the WireGuard tunnel.
///
/// This function adds /32 routes for each peer, which override the /24 in the
/// same table due to longest-prefix match.
pub(crate) fn sync_peer_policy_routes(
    cfgs: &mut Configs,
    profile_interface: &str,
) -> Result<(), Error> {
    use crate::profiles::UciProfile;

    let wg_interface_name = format!("wg_{}", profile_interface);
    let route_prefix = format!("vpr_{}_", profile_interface);

    // 1. Remove all existing VPN peer routes for this profile
    cfgs["network"].sections.retain(|s| {
        s.name()
            .as_deref()
            .map(|n| !n.starts_with(&route_prefix))
            .unwrap_or(true)
    });

    // 2. Find the profile's outbound and vlan_tag
    let profile = cfgs["startwrt"]
        .sections
        .iter()
        .filter_map(|s| s.get::<UciProfile>().ok())
        .find(|p| p.interface == profile_interface);

    let Some(profile) = profile else {
        return Ok(());
    };

    let outbound = profile.outbound.unwrap_or_else(|| "wan".to_string());
    if outbound == "wan" {
        return Ok(());
    }

    let vlan_tag = profile.vlan_tag;

    // 3. Check if a VPN server exists for this profile
    let vpn_exists = cfgs["startwrt"]
        .sections
        .iter()
        .filter_map(|s| s.get::<UciVpnServer>().ok())
        .any(|meta| meta.interface == wg_interface_name);

    if !vpn_exists {
        return Ok(());
    }

    // 4. Add a /32 route for each peer in the policy routing table
    for ip in get_vpn_peer_ips(cfgs, &wg_interface_name) {
        let route_name = format!("vpr_{}_{}", profile_interface, ip.octets()[3]);
        cfgs["network"].append(
            &NetworkRoute {
                interface: wg_interface_name.clone(),
                target: format!("{}/32", ip),
                table: Some(vlan_tag as u32),
                ..Default::default()
            },
            Some(&route_name),
        )?;
    }

    Ok(())
}

/// Returns a map of profile_interface -> peer_count for profiles that have VPN peers.
pub(crate) fn profiles_with_vpn_peers(
    cfgs: &Configs,
    profile_interfaces: &[&str],
) -> BTreeMap<String, usize> {
    let mut result = BTreeMap::new();
    for section in &cfgs["startwrt"].sections {
        let Ok(vpn_meta) = section.get::<UciVpnServer>() else {
            continue;
        };
        if !profile_interfaces.contains(&vpn_meta.profile_interface.as_str()) {
            continue;
        }
        let peers = get_peers_for_interface(cfgs, &vpn_meta.interface);
        if !peers.is_empty() {
            result.insert(vpn_meta.profile_interface.clone(), peers.len());
        }
    }
    result
}

/// Fully remove the VPN server for a profile interface, including the WireGuard
/// interface, all peers, metadata, firewall rules, policy routes, and proxy ARP UCI.
/// Operates on already-parsed configs (caller handles dump_all and service reload).
///
/// Returns the profile's VLAN tag so the caller can call
/// `apply_proxy_arp_sysctl(tag, false)` after dump (netifd ignores the UCI option).
pub(crate) fn remove_vpn_server(
    cfgs: &mut Configs,
    profile_interface: &str,
) -> Result<Option<u16>, Error> {
    let wg_interface_name = format!("wg_{}", profile_interface);
    let peer_type = format!("wireguard_{}", wg_interface_name);

    // Read vlan_tag before modifying configs
    let vlan_tag = {
        use crate::profiles::UciProfile;
        cfgs["startwrt"]
            .sections
            .iter()
            .filter_map(|s| s.get::<UciProfile>().ok())
            .find(|p| p.interface == profile_interface)
            .map(|p| p.vlan_tag)
    };

    // Remove WireGuard interface
    cfgs["network"].sections.retain(|section| {
        if section.name().as_deref() == Some(wg_interface_name.as_str()) {
            if let Ok(iface) = section.get::<WgInterface>() {
                if iface.is_wireguard() {
                    return false;
                }
            }
        }
        true
    });

    // Remove all peers
    cfgs["network"]
        .sections
        .retain(|section| section.ty() != peer_type);

    // Remove VPN server metadata
    cfgs["startwrt"].sections.retain(|section| {
        if let Ok(meta) = section.get::<UciVpnServer>() {
            return meta.interface != wg_interface_name;
        }
        true
    });

    // Clean up firewall
    remove_from_firewall_zones(cfgs, &wg_interface_name)?;
    remove_wireguard_firewall_rule(cfgs, &wg_interface_name)?;

    // Clean up routing and proxy ARP UCI
    sync_peer_policy_routes(cfgs, profile_interface)?;
    sync_proxy_arp(cfgs, profile_interface)?;

    Ok(vlan_tag)
}

/// Enable proxy_arp on the profile's bridge VLAN interface when a VPN server exists.
///
/// VPN peers share the same /24 subnet as LAN clients. Without proxy ARP, LAN devices
/// try to ARP directly for VPN peer IPs (which are behind the WireGuard tunnel) and fail.
/// With proxy_arp enabled, the router answers ARP requests on behalf of VPN peers,
/// allowing LAN devices to route responses through the router.
pub(crate) fn sync_proxy_arp(
    cfgs: &mut Configs,
    profile_interface: &str,
) -> Result<(), Error> {
    let wg_interface_name = format!("wg_{}", profile_interface);

    // Check if a VPN server exists for this profile
    let vpn_exists = cfgs["startwrt"]
        .sections
        .iter()
        .filter_map(|s| s.get::<UciVpnServer>().ok())
        .any(|meta| meta.interface == wg_interface_name);

    let desired = if vpn_exists { Some("1".to_string()) } else { None };

    // Find the profile's network interface and update proxy_arp if needed
    let section = cfgs["network"]
        .sections
        .iter_mut()
        .find(|s| s.name().as_deref() == Some(profile_interface));

    let Some(section) = section else {
        return Ok(());
    };

    let mut iface = section.get::<NetworkInterface>().map_err(|_| Error::from(
        ErrorKind::CorruptedProfile {
            id: ProfileIdOpt {
                fullname: None,
                interface: Some(profile_interface.to_string()),
                vlan_tag: None,
            },
        },
    ))?;

    if iface.proxy_arp != desired {
        iface.proxy_arp = desired;
        section.set(&iface)?;
    }

    Ok(())
}

/// Set the proxy_arp sysctl on the profile's bridge VLAN interface.
///
/// netifd does not honor the UCI `proxy_arp` option on regular interfaces,
/// so we write directly to `/proc/sys/net/ipv4/conf/<dev>/proxy_arp`.
pub(crate) fn apply_proxy_arp_sysctl(vlan_tag: u16, enable: bool) {
    let path = format!("/proc/sys/net/ipv4/conf/br-lan.{}/proxy_arp", vlan_tag);
    let _ = std::fs::write(&path, if enable { "1" } else { "0" });
}

/// Tracks VPN servers removed by [`guard_vpn_peers`] so callers can tear down
/// the live WireGuard interfaces and proxy ARP sysctls after dumping configs.
#[derive(Default)]
pub(crate) struct RemovedVpnServers {
    pub wg_ifaces: Vec<String>,
    pub proxy_arp_tags: Vec<u16>,
}

impl RemovedVpnServers {
    /// Bring down orphaned WireGuard interfaces and clear proxy ARP sysctls.
    /// Call AFTER `dump_all` and the main service reload.
    pub fn apply_post_reload(&self) {
        for wg in &self.wg_ifaces {
            let _ = crate::run_quiet(Command::new("ifdown").arg(wg));
        }
        for tag in &self.proxy_arp_tags {
            apply_proxy_arp_sysctl(*tag, false);
        }
    }
}

/// Check whether changing the IP/subnet of the given profile interfaces would
/// break existing VPN peers. If `force` is false and peers exist, returns
/// `VpnPeersWouldBreak`. If `force` is true, removes the affected VPN servers
/// from the parsed configs (caller still needs to `dump_all`).
pub(crate) fn guard_vpn_peers(
    cfgs: &mut Configs,
    affected_interfaces: &[&str],
    force: bool,
) -> Result<RemovedVpnServers, Error> {
    let peers = profiles_with_vpn_peers(cfgs, affected_interfaces);
    if peers.is_empty() {
        return Ok(RemovedVpnServers::default());
    }

    let total: usize = peers.values().sum();
    if !force {
        return Err(crate::ErrorKind::VpnPeersWouldBreak {
            profiles: peers.keys().cloned().collect(),
            peer_count: total,
        }
        .into());
    }

    let mut removed = RemovedVpnServers::default();
    for iface in peers.keys() {
        if let Some(tag) = remove_vpn_server(cfgs, iface)? {
            removed.proxy_arp_tags.push(tag);
        }
        removed.wg_ifaces.push(format!("wg_{}", iface));
    }
    Ok(removed)
}

/// Get IPs reserved by DHCP static leases that fall within the given subnet
fn get_dhcp_static_lease_ips(cfgs: &Configs, subnet_prefix: [u8; 3]) -> BTreeSet<Ipv4Addr> {
    cfgs["dhcp"]
        .sections
        .iter()
        .filter_map(|section| section.get::<DhcpHost>().ok())
        .filter_map(|host| host.ip.as_deref()?.parse::<Ipv4Addr>().ok())
        .filter(|ip| {
            let octets = ip.octets();
            octets[0] == subnet_prefix[0]
                && octets[1] == subnet_prefix[1]
                && octets[2] == subnet_prefix[2]
        })
        .collect()
}

/// Get all reserved IPs in a subnet (VPN peers + DHCP static leases)
fn get_reserved_ips(cfgs: &Configs, wg_interface_name: &str, gateway_ip: Ipv4Addr) -> BTreeSet<Ipv4Addr> {
    let octets = gateway_ip.octets();
    let subnet_prefix = [octets[0], octets[1], octets[2]];

    let mut reserved = get_vpn_peer_ips(cfgs, wg_interface_name);
    reserved.extend(get_dhcp_static_lease_ips(cfgs, subnet_prefix));
    reserved
}

/// Allocate next available IP for a peer within the profile's subnet
fn allocate_peer_ip(gateway_ip: Ipv4Addr, reserved: &BTreeSet<Ipv4Addr>) -> Result<Ipv4Addr, Error> {
    let octets = gateway_ip.octets();

    for host in PEER_IP_START..=PEER_IP_END {
        let ip = Ipv4Addr::new(octets[0], octets[1], octets[2], host);
        if !reserved.contains(&ip) {
            return Ok(ip);
        }
    }

    Err(Error::other("No available IP addresses for new peer (max 54 peers per VPN server)"))
}

/// List all VPN servers
pub fn list(_ctx: ServerContext) -> Result<VpnServers, Error> {
    let arena = Arena::new();
    let cfgs = parse_all("/etc/config", &arena, &["network", "startwrt", "firewall"])?;

    let profile_lookup = profiles::Lookup::parse(ServerContext::default(), &cfgs)?;

    let servers = cfgs["startwrt"]
        .sections
        .iter()
        .filter_map(|section| section.get::<UciVpnServer>().ok())
        .filter_map(|vpn_meta| {
            // Verify profile exists
            let _profile_id = profile_lookup.from_interface(&vpn_meta.profile_interface)?;
            let gateway_ip = get_gateway_ip(&cfgs, &vpn_meta.profile_interface).ok()?;

            // Get WireGuard interface
            let wg_iface = cfgs["network"]
                .sections
                .iter()
                .find(|s| s.name().as_deref() == Some(vpn_meta.interface.as_str()))
                .and_then(|s| s.get::<WgInterface>().ok())
                .filter(|wg| wg.is_wireguard())?;

            let peers = get_peers_for_interface(&cfgs, &vpn_meta.interface);
            let public_key = derive_public_key(&wg_iface.private_key).ok()?;
            let server_address = derive_server_address(gateway_ip);

            Some(VpnServer {
                profile: vpn_meta.profile_interface.clone(),
                label: vpn_meta.label.clone(),
                enabled: wg_iface.listen_port.is_some(),
                listen_port: vpn_meta.listen_port,
                endpoint: vpn_meta.endpoint.clone(),
                public_key,
                server_address,
                peers,
            })
        })
        .collect();

    Ok(VpnServers { servers })
}

/// Set (create or update) VPN server configuration for a profile
pub fn set(
    _ctx: ServerContext,
    DeserializeStdin(SetArgs { profile, config }): DeserializeStdin<SetArgs>,
) -> Result<(), Error> {
    // Validate inputs before touching any config
    validate_endpoint(&config.endpoint)?;
    if config.listen_port == 0 {
        return Err(ErrorKind::InvalidValue {
            field: "listen_port".into(),
            value: "0".into(),
        }
        .into());
    }
    if let Some(ref key) = config.private_key {
        validate_private_key(key)?;
    }

    let profile_interface = &profile;
    let wg_interface_name = format!("wg_{}", profile_interface);

    let mut retries = 4;
    loop {
        let arena = Arena::new();
        let mut cfgs = parse_all("/etc/config", &arena, &["network", "startwrt", "firewall"])?;

        // Verify the profile exists and get its gateway IP
        let profile_lookup = profiles::Lookup::parse(ServerContext::default(), &cfgs)?;
        let profile_id = profile_lookup.from_interface(profile_interface)
            .ok_or_else(|| ErrorKind::MissingProfile {
                id: ProfileIdOpt {
                    fullname: None,
                    interface: Some(profile_interface.clone()),
                    vlan_tag: None,
                }
            })?;
        let profile_name = profile_id.fullname.clone();

        // Check for listen port conflicts with other VPN servers
        let port_conflict = cfgs["startwrt"]
            .sections
            .iter()
            .filter_map(|s| s.get::<UciVpnServer>().ok())
            .any(|meta| meta.listen_port == config.listen_port && meta.interface != wg_interface_name);
        if port_conflict {
            return Err(ErrorKind::InvalidValue {
                field: "listen_port".into(),
                value: config.listen_port.to_string(),
            }
            .into());
        }

        let gateway_ip = get_gateway_ip(&cfgs, profile_interface)?;
        let server_address = derive_server_address(gateway_ip);

        // Update or create the WireGuard interface
        set_wireguard_interface(&mut cfgs, &wg_interface_name, &config, server_address)?;

        // Update or create VPN server metadata
        set_vpn_server_metadata(&mut cfgs, &wg_interface_name, profile_interface, &config)?;

        // Ensure the WireGuard interface is in the profile's firewall zone
        ensure_firewall_zone(&mut cfgs, &wg_interface_name, profile_interface)?;

        if config.enabled {
            // Ensure firewall rule allows WireGuard traffic on WAN
            ensure_wireguard_firewall_rule(&mut cfgs, &wg_interface_name, config.listen_port)?;
        } else {
            // Remove firewall rule when disabled — no inbound WireGuard traffic should be accepted
            remove_wireguard_firewall_rule(&mut cfgs, &wg_interface_name)?;
        }

        // Sync /32 peer routes in the profile's policy routing table
        sync_peer_policy_routes(&mut cfgs, profile_interface)?;

        // Enable proxy ARP on the profile's LAN interface so LAN devices can reach VPN peers
        sync_proxy_arp(&mut cfgs, profile_interface)?;

        // Read vlan_tag before dump_all consumes cfgs
        let vlan_tag = {
            use crate::profiles::UciProfile;
            cfgs["startwrt"]
                .sections
                .iter()
                .filter_map(|s| s.get::<UciProfile>().ok())
                .find(|p| p.interface == *profile_interface)
                .map(|p| p.vlan_tag)
        };

        match dump_all("/etc/config", cfgs) {
            Err(uciedit::Error::Conflict { .. }) if retries > 0 => {
                retries -= 1;
                continue;
            }
            Err(err) => {
                crate::activity::log("vpn-server", "configured", false, &format!("Failed to configure inbound VPN for profile '{}'", profile_name), Some(&err.to_string()));
                return Err(err.into());
            }
            Ok(()) => {
                if config.enabled {
                    // Restart the interface to ensure it comes up with new config.
                    // reload_system() alone won't bring up an interface that was
                    // previously taken down with ifdown.
                    restart_wireguard_interface(&wg_interface_name)?;
                } else {
                    // Bring the WireGuard interface down to disconnect existing clients.
                    // reload_system() alone won't tear down an active WireGuard tunnel.
                    let _ = crate::run_quiet(Command::new("ifdown").arg(&wg_interface_name));
                }
                reload_system()?;
                // Apply proxy_arp sysctl directly — netifd ignores the UCI option
                if let Some(tag) = vlan_tag {
                    apply_proxy_arp_sysctl(tag, true);
                }
                crate::activity::log("vpn-server", "configured", true, &format!("Configured inbound VPN for profile '{}'", profile_name), None);
                return Ok(());
            }
        }
    }
}

/// Delete VPN server configuration for a profile
pub fn delete(_ctx: ServerContext, args: DeleteArgs) -> Result<(), Error> {
    let profile_interface = &args.profile;
    let wg_interface_name = format!("wg_{}", profile_interface);

    let mut retries = 4;
    loop {
        let arena = Arena::new();
        let mut cfgs = parse_all("/etc/config", &arena, &["network", "startwrt", "firewall", "dhcp"])?;

        // Resolve profile fullname before modifying configs
        let profile_name = {
            let profile_lookup = profiles::Lookup::parse(ServerContext::default(), &cfgs)?;
            profile_lookup.from_interface(profile_interface)
                .map(|p| p.fullname.clone())
                .unwrap_or_else(|| profile_interface.clone())
        };

        // Remove the WireGuard interface from network config
        cfgs["network"].sections.retain(|section| {
            if section.name().as_deref() == Some(wg_interface_name.as_str()) {
                if let Ok(iface) = section.get::<WgInterface>() {
                    if iface.is_wireguard() {
                        return false;
                    }
                }
            }
            true
        });

        // Remove peers (sections of type wireguard_<interface_name>)
        let peer_type = format!("wireguard_{}", wg_interface_name);
        cfgs["network"].sections.retain(|section| {
            section.ty() != peer_type
        });

        // Remove VPN server metadata from startwrt
        cfgs["startwrt"].sections.retain(|section| {
            if let Ok(meta) = section.get::<UciVpnServer>() {
                return meta.interface != wg_interface_name;
            }
            true
        });

        // Remove from firewall zones
        remove_from_firewall_zones(&mut cfgs, &wg_interface_name)?;

        // Remove the firewall rule allowing WireGuard on WAN
        remove_wireguard_firewall_rule(&mut cfgs, &wg_interface_name)?;

        // Clean up /32 peer routes from the profile's policy routing table
        sync_peer_policy_routes(&mut cfgs, profile_interface)?;

        // Disable proxy ARP now that no VPN server exists for this profile
        sync_proxy_arp(&mut cfgs, profile_interface)?;

        // Read vlan_tag before dump_all consumes cfgs
        let vlan_tag = {
            use crate::profiles::UciProfile;
            cfgs["startwrt"]
                .sections
                .iter()
                .filter_map(|s| s.get::<UciProfile>().ok())
                .find(|p| p.interface == *profile_interface)
                .map(|p| p.vlan_tag)
        };

        match dump_all("/etc/config", cfgs) {
            Err(uciedit::Error::Conflict { .. }) if retries > 0 => {
                retries -= 1;
                continue;
            }
            Err(err) => {
                crate::activity::log("vpn-server", "deleted", false, &format!("Failed to delete inbound VPN for profile '{}'", profile_name), Some(&err.to_string()));
                return Err(err.into());
            }
            Ok(()) => {
                reload_system()?;
                // Clear proxy_arp sysctl — netifd ignores the UCI option
                if let Some(tag) = vlan_tag {
                    apply_proxy_arp_sysctl(tag, false);
                }
                crate::activity::log("vpn-server", "deleted", true, &format!("Deleted inbound VPN for profile '{}'", profile_name), None);
                return Ok(());
            }
        }
    }
}

/// Add a peer to a VPN server
pub fn peer_add(
    _ctx: ServerContext,
    DeserializeStdin(PeerAddArgs { profile, mut peer }): DeserializeStdin<PeerAddArgs>,
) -> Result<PeerAddResponse, Error> {
    // Validate peer name before any other processing
    validate_peer_name(&peer.name)?;

    let profile_interface = &profile;
    let wg_interface_name = format!("wg_{}", profile_interface);

    // Generate key pair if public_key not provided (using native Rust crypto)
    let (peer_public_key, client_private_key) = match &peer.public_key {
        Some(pk) => (pk.clone(), None),
        None => {
            let private_key = Base64::new(WgKey::generate());
            let public_key = private_key.public_key();
            (public_key.to_base64(), Some(private_key))
        }
    };

    // Always generate PSK for better security if not provided
    if peer.preshared_key.is_none() {
        peer.preshared_key = Some(Base64::new(generate_psk()).to_base64());
    }

    // Update peer with the public key (in case it was generated)
    peer.public_key = Some(peer_public_key.clone());

    let mut retries = 4;
    loop {
        let arena = Arena::new();
        let mut cfgs = parse_all("/etc/config", &arena, &["network", "startwrt", "firewall", "dhcp"])?;

        // Verify the profile exists and get its gateway IP
        let profile_lookup = profiles::Lookup::parse(ServerContext::default(), &cfgs)?;
        let profile_id = profile_lookup.from_interface(profile_interface)
            .ok_or_else(|| ErrorKind::MissingProfile {
                id: ProfileIdOpt {
                    fullname: None,
                    interface: Some(profile_interface.clone()),
                    vlan_tag: None,
                }
            })?;
        let profile_name = profile_id.fullname.clone();

        let gateway_ip = get_gateway_ip(&cfgs, profile_interface)?;

        // Find VPN server metadata for this profile
        let vpn_meta = cfgs["startwrt"]
            .sections
            .iter()
            .filter_map(|s| s.get::<UciVpnServer>().ok())
            .find(|meta| meta.interface == wg_interface_name)
            .ok_or_else(|| Error::other(format!(
                "VPN server not found for profile: {}",
                profile_interface
            )))?;

        let server_endpoint = vpn_meta.endpoint.clone();
        let server_port = vpn_meta.listen_port;

        // Get server's public key from the WireGuard interface
        let server_public_key = cfgs["network"]
            .sections
            .iter()
            .find(|s| s.name().as_deref() == Some(wg_interface_name.as_str()))
            .and_then(|s| s.get::<WgInterface>().ok())
            .filter(|wg| wg.is_wireguard())
            .map(|wg| derive_public_key(&wg.private_key))
            .transpose()?
            .ok_or_else(|| Error::other("WireGuard interface not found"))?;

        // Check if peer with this public key already exists
        let peer_type = format!("wireguard_{}", wg_interface_name);
        let existing_peer = cfgs["network"]
            .sections
            .iter()
            .filter(|s| s.ty() == peer_type)
            .any(|section| {
                section.lines.iter().any(|line| {
                    matches!(line, Line::Option { option, value, .. }
                        if option.as_str() == "public_key" && value.as_str() == peer_public_key.as_str())
                })
            });

        if existing_peer {
            return Err(Error::other(format!(
                "Peer with public key {} already exists",
                peer_public_key
            )));
        }

        // Allocate IP for peer if not specified, checking both VPN peers and DHCP static leases
        let reserved_ips = get_reserved_ips(&cfgs, &wg_interface_name, gateway_ip);
        let peer_ip = match peer.ip {
            Some(ip) => {
                // Validate the IP is in the valid range
                let octets = ip.octets();
                let gateway_octets = gateway_ip.octets();
                if octets[0] != gateway_octets[0] || octets[1] != gateway_octets[1] || octets[2] != gateway_octets[2] {
                    return Err(Error::other("Peer IP must be in the same subnet as the profile"));
                }
                if octets[3] < PEER_IP_START || octets[3] > PEER_IP_END {
                    return Err(Error::other(format!(
                        "Peer IP host octet must be between {} and {}",
                        PEER_IP_START, PEER_IP_END
                    )));
                }
                if reserved_ips.contains(&ip) {
                    return Err(Error::other(format!("IP {} is already allocated", ip)));
                }
                ip
            }
            None => allocate_peer_ip(gateway_ip, &reserved_ips)?,
        };

        peer.ip = Some(peer_ip);

        // Add the new peer
        add_single_peer(&mut cfgs, &wg_interface_name, &peer, &arena)?;

        // Sync /32 peer routes in the profile's policy routing table
        sync_peer_policy_routes(&mut cfgs, profile_interface)?;

        match dump_all("/etc/config", cfgs) {
            Err(uciedit::Error::Conflict { .. }) if retries > 0 => {
                retries -= 1;
                continue;
            }
            Err(err) => {
                crate::activity::log("vpn-server", "peer-added", false, &format!("Failed to add peer '{}' to inbound VPN for profile '{}'", peer.name, profile_name), Some(&err.to_string()));
                return Err(err.into());
            }
            Ok(()) => {
                // Restart the WireGuard interface to apply peer changes immediately
                restart_wireguard_interface(&wg_interface_name)?;

                // Generate client config if we generated the keys
                let route_all = peer.route_all == Some(true);
                let allowed_ips = if route_all {
                    "0.0.0.0/0, ::/0".to_string()
                } else {
                    let octets = gateway_ip.octets();
                    format!("{}.{}.{}.0/24", octets[0], octets[1], octets[2])
                };

                let client_config = client_private_key.map(|private_key| {
                    let psk = peer.preshared_key.as_deref().unwrap_or("");

                    format!(
                        "# WireGuard client configuration for {name}\n\
                         # Generated by StartWRT\n\
                         \n\
                         [Interface]\n\
                         PrivateKey = {privkey}\n\
                         Address = {addr}/32\n\
                         DNS = {gateway}\n\
                         \n\
                         [Peer]\n\
                         PublicKey = {server_pubkey}\n\
                         PresharedKey = {psk}\n\
                         Endpoint = {endpoint}:{port}\n\
                         AllowedIPs = {allowed_ips}\n\
                         PersistentKeepalive = 25\n",
                        name = peer.name,
                        privkey = private_key.to_base64(),
                        addr = peer_ip,
                        server_pubkey = server_public_key,
                        psk = psk,
                        endpoint = server_endpoint,
                        port = server_port,
                        gateway = gateway_ip,
                    )
                });

                crate::activity::log("vpn-server", "peer-added", true, &format!("Added peer '{}' to inbound VPN for profile '{}'", peer.name, profile_name), None);
                return Ok(PeerAddResponse {
                    client_config,
                    public_key: peer_public_key,
                    ip: peer_ip,
                });
            }
        }
    }
}

/// Delete a peer from a VPN server
pub fn peer_delete(_ctx: ServerContext, args: PeerDeleteArgs) -> Result<(), Error> {
    let profile_interface = &args.profile;
    let wg_interface_name = format!("wg_{}", profile_interface);
    let public_key = &args.public_key;

    let mut retries = 4;
    loop {
        let arena = Arena::new();
        let mut cfgs = parse_all("/etc/config", &arena, &["network", "startwrt"])?;

        // Resolve profile fullname before modifying configs
        let profile_name = {
            let profile_lookup = profiles::Lookup::parse(ServerContext::default(), &cfgs)?;
            profile_lookup.from_interface(profile_interface)
                .map(|p| p.fullname.clone())
                .unwrap_or_else(|| profile_interface.clone())
        };

        // Verify the VPN server exists for this profile
        let vpn_exists = cfgs["startwrt"]
            .sections
            .iter()
            .filter_map(|s| s.get::<UciVpnServer>().ok())
            .any(|meta| meta.interface == wg_interface_name);

        if !vpn_exists {
            return Err(Error::other(format!(
                "VPN server not found for profile: {}",
                profile_interface
            )));
        }

        // Find and remove the peer with matching public key
        let peer_type = format!("wireguard_{}", wg_interface_name);
        let original_len = cfgs["network"].sections.len();

        cfgs["network"].sections.retain(|section| {
            if section.ty() != peer_type {
                return true;
            }
            // Keep the section if it doesn't have the matching public key
            !section.lines.iter().any(|line| {
                matches!(line, Line::Option { option, value, .. }
                    if option.as_str() == "public_key" && value.as_str() == public_key.as_str())
            })
        });

        if cfgs["network"].sections.len() == original_len {
            return Err(Error::other(format!(
                "Peer with public key {} not found",
                public_key
            )));
        }

        // Sync /32 peer routes in the profile's policy routing table
        sync_peer_policy_routes(&mut cfgs, profile_interface)?;

        match dump_all("/etc/config", cfgs) {
            Err(uciedit::Error::Conflict { .. }) if retries > 0 => {
                retries -= 1;
                continue;
            }
            Err(err) => {
                crate::activity::log("vpn-server", "peer-deleted", false, &format!("Failed to delete peer from inbound VPN for profile '{}'", profile_name), Some(&err.to_string()));
                return Err(err.into());
            }
            Ok(()) => {
                // Restart the WireGuard interface to apply peer changes immediately
                restart_wireguard_interface(&wg_interface_name)?;
                crate::activity::log("vpn-server", "peer-deleted", true, &format!("Deleted peer from inbound VPN for profile '{}'", profile_name), None);
                return Ok(());
            }
        }
    }
}

// === Helper Functions ===

fn add_single_peer<'a>(
    cfgs: &mut Configs<'a>,
    interface_name: &str,
    peer: &VpnServerPeer,
    arena: &'a Arena,
) -> Result<(), Error> {
    let peer_type = format!("wireguard_{}", interface_name);
    let peer_type_str: &str = arena.alloc(peer_type);

    // Generate a unique peer section name using max existing suffix + 1
    // Using count would collide after deletions (e.g., delete peer 1 of 3, count=2, collides with peer 2)
    let prefix = format!("{}_", interface_name);
    let next_index = cfgs["network"]
        .sections
        .iter()
        .filter(|s| s.ty() == peer_type_str)
        .filter_map(|s| s.name()?.strip_prefix(&prefix)?.parse::<u32>().ok())
        .max()
        .map(|n| n + 1)
        .unwrap_or(0);
    let peer_name = format!("{}{}", prefix, next_index);
    let peer_name_str: &str = arena.alloc(peer_name);

    let mut lines = vec![Line::Section {
        ty: Token::from_str(peer_type_str, arena),
        name: Some(Token::from_str(peer_name_str, arena)),
        comment: LineComment::None,
    }];

    // Add public_key option (must be set by caller)
    let public_key = peer.public_key.as_ref()
        .ok_or_else(|| Error::other("public_key must be set when adding peer"))?;
    let pub_key_str: &str = arena.alloc(public_key.clone());
    lines.push(Line::Option {
        option: Token::from_str("public_key", arena),
        value: Token::from_str(pub_key_str, arena),
        comment: LineComment::None,
    });

    // Add description option (for peer name)
    let desc_str: &str = arena.alloc(peer.name.clone());
    lines.push(Line::Option {
        option: Token::from_str("description", arena),
        value: Token::from_str(desc_str, arena),
        comment: LineComment::None,
    });

    // Add preshared_key if present
    if let Some(ref psk) = peer.preshared_key {
        let psk_str: &str = arena.alloc(psk.clone());
        lines.push(Line::Option {
            option: Token::from_str("preshared_key", arena),
            value: Token::from_str(psk_str, arena),
            comment: LineComment::None,
        });
    }

    // Add persistent_keepalive (always 25 for mobile clients)
    let keepalive_str: &str = arena.alloc("25".to_string());
    lines.push(Line::Option {
        option: Token::from_str("persistent_keepalive", arena),
        value: Token::from_str(keepalive_str, arena),
        comment: LineComment::None,
    });

    // Store route_all flag (only when true, absence = split tunnel)
    if peer.route_all == Some(true) {
        lines.push(Line::Option {
            option: Token::from_str("startwrt_route_all", arena),
            value: Token::from_str("1", arena),
            comment: LineComment::None,
        });
    }

    // Enable route creation for this peer's allowed_ips
    lines.push(Line::Option {
        option: Token::from_str("route_allowed_ips", arena),
        value: Token::from_str("1", arena),
        comment: LineComment::None,
    });

    // Add allowed_ips with the peer's assigned IP
    let ip = peer.ip.ok_or_else(|| Error::other("peer IP must be set"))?;
    let ip_str: &str = arena.alloc(format!("{}/32", ip));
    lines.push(Line::List {
        list: Token::from_str("allowed_ips", arena),
        item: Token::from_str(ip_str, arena),
        comment: LineComment::None,
    });

    let section = Section { arena, lines };
    cfgs["network"].sections.push(section);

    Ok(())
}

fn get_peers_for_interface(cfgs: &Configs, interface_name: &str) -> Vec<VpnServerPeer> {
    let peer_type = format!("wireguard_{}", interface_name);

    cfgs["network"]
        .sections
        .iter()
        .filter(|section| section.ty() == peer_type)
        .filter_map(|section| {
            let mut public_key = String::new();
            let mut ip: Option<Ipv4Addr> = None;
            let mut description = None;
            let mut route_all = false;

            for line in &section.lines {
                match line {
                    Line::Option { option, value, .. } => match option.as_str().as_ref() {
                        "public_key" => public_key = value.as_str().to_string(),
                        "description" => description = Some(value.as_str().to_string()),
                        "startwrt_route_all" => route_all = value.as_str() == "1",
                        _ => {}
                    },
                    Line::List { list, item, .. } => {
                        if list.as_str() == "allowed_ips" {
                            // Parse IP from "x.x.x.x/32" format
                            let ip_str = item.as_str();
                            if let Some(ip_part) = ip_str.split('/').next() {
                                ip = ip_part.parse().ok();
                            }
                        }
                    }
                    _ => {}
                }
            }

            if public_key.is_empty() {
                return None;
            }

            Some(VpnServerPeer {
                name: description
                    .unwrap_or_else(|| section.name().unwrap_or_default().to_string()),
                ip,
                public_key: Some(public_key),
                // Never expose PSK in list responses — it's only needed at peer creation time
                preshared_key: None,
                route_all: if route_all { Some(true) } else { None },
            })
        })
        .collect()
}

fn set_wireguard_interface(
    cfgs: &mut Configs,
    interface_name: &str,
    config: &VpnServerConfig,
    server_address: Ipv4Addr,
) -> Result<(), Error> {
    let mut found = false;
    // Use /32 for WireGuard to avoid routing conflicts with the LAN bridge.
    // WireGuard handles routing to peers via allowed_ips, not interface subnet.
    let address_cidr = format!("{}/32", server_address);

    for section in &mut cfgs["network"].sections {
        if section.name().as_deref() != Some(interface_name) {
            continue;
        }

        let Ok(mut wg_iface) = section.get::<WgInterface>() else {
            continue;
        };

        if !wg_iface.is_wireguard() {
            continue;
        }

        // Update existing interface - keep existing private_key if not provided
        if let Some(ref new_key) = config.private_key {
            wg_iface.private_key = new_key.clone();
        }
        wg_iface.listen_port = if config.enabled { Some(config.listen_port) } else { None };
        wg_iface.addresses = vec![address_cidr.clone()];
        section.set(&wg_iface)?;
        found = true;
        break;
    }

    if !found {
        // Create new interface - generate private_key if not provided (using native Rust crypto)
        let private_key = match &config.private_key {
            Some(key) => key.clone(),
            None => Base64::new(WgKey::generate()).to_base64(),
        };

        let new_iface = WgInterface {
            proto: "wireguard".to_string(),
            private_key,
            listen_port: if config.enabled { Some(config.listen_port) } else { None },
            addresses: vec![address_cidr],
            disabled: None,
        };
        cfgs["network"].append(&new_iface, Some(interface_name))?;
    }

    Ok(())
}

fn set_vpn_server_metadata(
    cfgs: &mut Configs,
    interface_name: &str,
    profile_interface: &str,
    config: &VpnServerConfig,
) -> Result<(), Error> {
    let mut found = false;

    for section in &mut cfgs["startwrt"].sections {
        let Ok(mut meta) = section.get::<UciVpnServer>() else {
            continue;
        };

        if meta.interface != interface_name {
            continue;
        }

        // Update existing metadata
        meta.label = config.label.clone();
        meta.listen_port = config.listen_port;
        meta.endpoint = config.endpoint.clone();
        meta.profile_interface = profile_interface.to_string();
        section.set(&meta)?;
        found = true;
        break;
    }

    if !found {
        // Create new metadata
        let meta = UciVpnServer {
            interface: interface_name.to_string(),
            profile_interface: profile_interface.to_string(),
            label: config.label.clone(),
            listen_port: config.listen_port,
            endpoint: config.endpoint.clone(),
        };
        cfgs["startwrt"].append(&meta, Some(interface_name))?;
    }

    Ok(())
}

fn ensure_firewall_zone(cfgs: &mut Configs, wg_interface_name: &str, profile_interface: &str) -> Result<(), Error> {
    // Find the firewall zone for the profile
    let zone_name = format!("vlan_{}", profile_interface);

    for section in &mut cfgs["firewall"].sections {
        let Ok(mut zone) = section.get::<uciedit::openwrt::FirewallZone>() else {
            continue;
        };

        // Check if this is the profile's zone (by name or by containing the profile interface)
        let is_profile_zone = zone.name == zone_name ||
            zone.network.iter().any(|n| n == profile_interface);

        if !is_profile_zone {
            continue;
        }

        // Add the WireGuard interface to this zone if not already present
        if !zone.network.iter().any(|n| n == wg_interface_name) {
            zone.network.push(wg_interface_name.to_string());
            section.set(&zone)?;
        }

        return Ok(());
    }

    // Zone not found - this shouldn't happen if the profile exists
    Err(ErrorKind::MissingFirewallZone {
        interface: profile_interface.to_string(),
    }.into())
}

fn remove_from_firewall_zones(cfgs: &mut Configs, wg_interface_name: &str) -> Result<(), Error> {
    for section in &mut cfgs["firewall"].sections {
        let Ok(mut zone) = section.get::<uciedit::openwrt::FirewallZone>() else {
            continue;
        };

        // Remove the WireGuard interface from this zone
        let original_len = zone.network.len();
        zone.network.retain(|n| n != wg_interface_name);

        if zone.network.len() != original_len {
            section.set(&zone)?;
        }
    }

    Ok(())
}

/// Firewall rule name for a WireGuard VPN server (used in the 'name' option)
fn wireguard_rule_name(wg_interface_name: &str) -> String {
    format!("Allow-WireGuard-{}", wg_interface_name)
}

/// UCI section name for the firewall rule (must be alphanumeric/underscore only)
fn wireguard_rule_section_name(wg_interface_name: &str) -> String {
    format!("allow_wireguard_{}", wg_interface_name)
}

/// Ensure a firewall rule exists to allow WireGuard traffic on the WAN interface.
/// This is required because the WAN zone typically has input=REJECT.
fn ensure_wireguard_firewall_rule(cfgs: &mut Configs, wg_interface_name: &str, listen_port: u16) -> Result<(), Error> {
    use uciedit::openwrt::{FirewallRule, FirewallTarget};

    let rule_name = wireguard_rule_name(wg_interface_name);
    let port_str = listen_port.to_string();

    // Check if rule already exists and update it
    for section in &mut cfgs["firewall"].sections {
        let Ok(mut rule) = section.get::<FirewallRule>() else {
            continue;
        };

        if rule.name == rule_name {
            // Update existing rule with new port if changed
            rule.dest_port = Some(port_str.clone());
            section.set(&rule)?;
            return Ok(());
        }
    }

    // Create new rule
    let rule = FirewallRule {
        name: rule_name.clone(),
        src: "wan".to_string(),
        src_ip: None,
        src_mac: None,
        src_port: None,
        dest: None,
        dest_ip: None,
        dest_port: Some(port_str),
        proto: vec!["udp".to_string()],
        target: FirewallTarget::ACCEPT,
        family: None,
        enabled: None,
        _pp_id: None,
        _pp_mac: None,
    };

    // Use a valid UCI section name (alphanumeric and underscores only)
    let section_name = wireguard_rule_section_name(wg_interface_name);
    cfgs["firewall"].append(&rule, Some(&section_name))?;

    Ok(())
}

/// Remove the firewall rule for a WireGuard VPN server.
pub(crate) fn remove_wireguard_firewall_rule(cfgs: &mut Configs, wg_interface_name: &str) -> Result<(), Error> {
    use uciedit::openwrt::FirewallRule;

    let rule_name = wireguard_rule_name(wg_interface_name);

    cfgs["firewall"].sections.retain(|section| {
        if let Ok(rule) = section.get::<FirewallRule>() {
            return rule.name != rule_name;
        }
        true
    });

    Ok(())
}

/// Derive public key from a base64-encoded private key using native Rust crypto.
fn derive_public_key(private_key: &str) -> Result<String, Error> {
    let key: Base64<WgKey> = private_key.parse()?;
    Ok(key.public_key().to_base64())
}

#[cfg(test)]
mod tests {
    use super::*;
    use std::path::{Path, PathBuf};
    use std::sync::Arc;
    use tokio::runtime::Runtime;
    use crate::CtrlContext;
    use rpc_toolkit::Context;

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

    /// Generate a valid WireGuard private key as base64 string
    fn gen_key() -> String {
        Base64::new(WgKey::generate()).to_base64()
    }

    /// Write base configs: two profiles (Admin/lan/vlan99, Guest/guest/vlan101)
    /// with network interfaces, firewall zones, and DHCP. No VPN server.
    fn setup_base_configs(dir: &Path) {
        std::fs::write(
            dir.join("startwrt"),
            "\
config profile lan
\toption fullname 'Admin'
\toption interface 'lan'
\toption vlan_tag '99'
\toption access_to_new_profiles '1'

config profile guest
\toption fullname 'Guest'
\toption interface 'guest'
\toption vlan_tag '101'
",
        )
        .unwrap();

        std::fs::write(
            dir.join("network"),
            "\
config device
\toption name 'br-lan'
\toption type 'bridge'
\tlist ports 'eth0'

config interface 'lan'
\toption device 'br-lan.99'
\toption proto 'static'
\toption ipaddr '192.168.1.1'
\toption netmask '255.255.255.0'

config interface 'guest'
\toption device 'br-lan.101'
\toption proto 'static'
\toption ipaddr '192.168.101.1'
\toption netmask '255.255.255.0'
",
        )
        .unwrap();

        std::fs::write(
            dir.join("firewall"),
            "\
config zone
\toption name 'lan'
\tlist network 'lan'
\toption input 'ACCEPT'
\toption output 'ACCEPT'
\toption forward 'ACCEPT'

config zone
\toption name 'wan'
\tlist network 'wan'
\toption input 'REJECT'
\toption output 'ACCEPT'
\toption forward 'REJECT'

config zone
\toption name 'vlan_guest'
\tlist network 'guest'
\toption input 'ACCEPT'
\toption output 'ACCEPT'
\toption forward 'ACCEPT'

config forwarding
\toption src 'lan'
\toption dest 'wan'

config forwarding
\toption src 'vlan_guest'
\toption dest 'wan'
",
        )
        .unwrap();

        std::fs::write(
            dir.join("dhcp"),
            "\
config dhcp 'lan'
\toption interface 'lan'
\toption start '100'
\toption limit '150'
\toption leasetime '12h'

config dhcp 'guest'
\toption interface 'guest'
\toption start '100'
\toption limit '150'
\toption leasetime '12h'
",
        )
        .unwrap();
    }

    /// Write configs that include a VPN server for Guest profile.
    fn setup_with_vpn_server(dir: &Path) -> (String, String, String) {
        let server_key = gen_key();
        let peer0_key = gen_key();
        let peer1_key = gen_key();

        std::fs::write(
            dir.join("startwrt"),
            format!(
                "\
config profile lan
\toption fullname 'Admin'
\toption interface 'lan'
\toption vlan_tag '99'
\toption access_to_new_profiles '1'

config profile guest
\toption fullname 'Guest'
\toption interface 'guest'
\toption vlan_tag '101'

config vpn_server 'wg_guest'
\toption interface 'wg_guest'
\toption profile_interface 'guest'
\toption label 'Guest VPN'
\toption listen_port '51820'
\toption endpoint 'vpn.example.com'
"
            ),
        )
        .unwrap();

        std::fs::write(
            dir.join("network"),
            format!(
                "\
config device
\toption name 'br-lan'
\toption type 'bridge'
\tlist ports 'eth0'

config interface 'lan'
\toption device 'br-lan.99'
\toption proto 'static'
\toption ipaddr '192.168.1.1'
\toption netmask '255.255.255.0'

config interface 'guest'
\toption device 'br-lan.101'
\toption proto 'static'
\toption ipaddr '192.168.101.1'
\toption netmask '255.255.255.0'

config interface 'wg_guest'
\toption proto 'wireguard'
\toption private_key '{server_key}'
\toption listen_port '51820'
\tlist addresses '192.168.101.254/32'

config wireguard_wg_guest 'wg_guest_0'
\toption public_key '{peer0_key}'
\toption description 'Phone'
\toption persistent_keepalive '25'
\toption route_allowed_ips '1'
\tlist allowed_ips '192.168.101.200/32'

config wireguard_wg_guest 'wg_guest_1'
\toption public_key '{peer1_key}'
\toption description 'Laptop'
\toption persistent_keepalive '25'
\toption route_allowed_ips '1'
\tlist allowed_ips '192.168.101.201/32'
"
            ),
        )
        .unwrap();

        std::fs::write(
            dir.join("firewall"),
            "\
config zone
\toption name 'lan'
\tlist network 'lan'
\toption input 'ACCEPT'
\toption output 'ACCEPT'
\toption forward 'ACCEPT'

config zone
\toption name 'wan'
\tlist network 'wan'
\toption input 'REJECT'
\toption output 'ACCEPT'
\toption forward 'REJECT'

config zone
\toption name 'vlan_guest'
\tlist network 'guest'
\tlist network 'wg_guest'
\toption input 'ACCEPT'
\toption output 'ACCEPT'
\toption forward 'ACCEPT'

config forwarding
\toption src 'lan'
\toption dest 'wan'

config forwarding
\toption src 'vlan_guest'
\toption dest 'wan'

config rule 'allow_wireguard_wg_guest'
\toption name 'Allow-WireGuard-wg_guest'
\toption src 'wan'
\toption dest_port '51820'
\tlist proto 'udp'
\toption target 'ACCEPT'
",
        )
        .unwrap();

        std::fs::write(
            dir.join("dhcp"),
            "\
config dhcp 'lan'
\toption interface 'lan'
\toption start '100'
\toption limit '150'
\toption leasetime '12h'

config dhcp 'guest'
\toption interface 'guest'
\toption start '100'
\toption limit '150'
\toption leasetime '12h'
",
        )
        .unwrap();

        (server_key, peer0_key, peer1_key)
    }

    // === Pure function tests ===

    #[test]
    fn test_derive_server_address() {
        let gateway = Ipv4Addr::new(192, 168, 101, 1);
        let server = derive_server_address(gateway);
        assert_eq!(server, Ipv4Addr::new(192, 168, 101, 254));
    }

    #[test]
    fn test_validate_endpoint_valid() {
        assert!(validate_endpoint("vpn.example.com").is_ok());
        assert!(validate_endpoint("1.2.3.4").is_ok());
    }

    #[test]
    fn test_validate_endpoint_invalid() {
        assert!(validate_endpoint("").is_err());
        assert!(validate_endpoint("has space").is_err());
        assert!(validate_endpoint("has\nnewline").is_err());
    }

    #[test]
    fn test_validate_peer_name_valid() {
        assert!(validate_peer_name("My Phone").is_ok());
    }

    #[test]
    fn test_validate_peer_name_invalid() {
        assert!(validate_peer_name("has\nnewline").is_err());
    }

    #[test]
    fn test_validate_private_key() {
        let valid_key = gen_key();
        assert!(validate_private_key(&valid_key).is_ok());
        assert!(validate_private_key("not-valid").is_err());
        // Valid base64 but wrong length (only 16 bytes)
        assert!(validate_private_key("dGVzdHRlc3R0ZXN0dGVzdHQ=").is_err());
    }

    #[test]
    fn test_allocate_peer_ip_first() {
        let gateway = Ipv4Addr::new(192, 168, 101, 1);
        let reserved = BTreeSet::new();
        let ip = allocate_peer_ip(gateway, &reserved).unwrap();
        assert_eq!(ip, Ipv4Addr::new(192, 168, 101, 200));
    }

    #[test]
    fn test_allocate_peer_ip_skips_reserved() {
        let gateway = Ipv4Addr::new(192, 168, 101, 1);
        let mut reserved = BTreeSet::new();
        reserved.insert(Ipv4Addr::new(192, 168, 101, 200));
        reserved.insert(Ipv4Addr::new(192, 168, 101, 201));
        let ip = allocate_peer_ip(gateway, &reserved).unwrap();
        assert_eq!(ip, Ipv4Addr::new(192, 168, 101, 202));
    }

    #[test]
    fn test_allocate_peer_ip_exhausted() {
        let gateway = Ipv4Addr::new(192, 168, 101, 1);
        let mut reserved = BTreeSet::new();
        for host in PEER_IP_START..=PEER_IP_END {
            reserved.insert(Ipv4Addr::new(192, 168, 101, host));
        }
        assert!(allocate_peer_ip(gateway, &reserved).is_err());
    }

    #[test]
    fn test_wireguard_rule_names() {
        assert_eq!(wireguard_rule_name("wg_guest"), "Allow-WireGuard-wg_guest");
        assert_eq!(wireguard_rule_section_name("wg_guest"), "allow_wireguard_wg_guest");
    }

    // === Config manipulation tests ===

    #[test]
    fn test_set_wireguard_interface_creates() {
        let dir = tempfile::tempdir().unwrap();
        setup_base_configs(dir.path());

        let arena = Arena::new();
        let mut cfgs = parse_all(dir.path(), &arena, &["network", "startwrt", "firewall", "dhcp"]).unwrap();

        let private_key = gen_key();
        let config = VpnServerConfig {
            label: "Guest VPN".into(),
            enabled: true,
            listen_port: 51820,
            endpoint: "vpn.example.com".into(),
            private_key: Some(private_key.clone()),
        };
        let server_addr = Ipv4Addr::new(192, 168, 101, 254);

        set_wireguard_interface(&mut cfgs, "wg_guest", &config, server_addr).unwrap();

        // Verify interface was created
        let wg = cfgs["network"]
            .sections
            .iter()
            .find(|s| s.name().as_deref() == Some("wg_guest"))
            .and_then(|s| s.get::<WgInterface>().ok())
            .expect("wg_guest interface should exist");

        assert_eq!(wg.proto, "wireguard");
        assert_eq!(wg.listen_port, Some(51820));
        assert_eq!(wg.addresses, vec!["192.168.101.254/32"]);
        assert_eq!(wg.private_key, private_key);
    }

    #[test]
    fn test_set_wireguard_interface_updates() {
        let dir = tempfile::tempdir().unwrap();
        let (server_key, _, _) = setup_with_vpn_server(dir.path());

        let arena = Arena::new();
        let mut cfgs = parse_all(dir.path(), &arena, &["network", "startwrt", "firewall", "dhcp"]).unwrap();

        // Update with no private_key → should preserve existing
        let config = VpnServerConfig {
            label: "Guest VPN".into(),
            enabled: true,
            listen_port: 51821, // changed port
            endpoint: "vpn.example.com".into(),
            private_key: None,
        };
        let server_addr = Ipv4Addr::new(192, 168, 101, 254);

        set_wireguard_interface(&mut cfgs, "wg_guest", &config, server_addr).unwrap();

        let wg = cfgs["network"]
            .sections
            .iter()
            .find(|s| s.name().as_deref() == Some("wg_guest"))
            .and_then(|s| s.get::<WgInterface>().ok())
            .unwrap();

        assert_eq!(wg.listen_port, Some(51821));
        assert_eq!(wg.private_key, server_key, "private_key should be preserved when None");

        // Update with new private_key → should replace
        let new_key = gen_key();
        let config2 = VpnServerConfig {
            label: "Guest VPN".into(),
            enabled: true,
            listen_port: 51821,
            endpoint: "vpn.example.com".into(),
            private_key: Some(new_key.clone()),
        };
        set_wireguard_interface(&mut cfgs, "wg_guest", &config2, server_addr).unwrap();

        let wg2 = cfgs["network"]
            .sections
            .iter()
            .find(|s| s.name().as_deref() == Some("wg_guest"))
            .and_then(|s| s.get::<WgInterface>().ok())
            .unwrap();

        assert_eq!(wg2.private_key, new_key, "private_key should be replaced when Some");
    }

    #[test]
    fn test_set_wireguard_interface_disabled() {
        let dir = tempfile::tempdir().unwrap();
        setup_base_configs(dir.path());

        let arena = Arena::new();
        let mut cfgs = parse_all(dir.path(), &arena, &["network", "startwrt", "firewall", "dhcp"]).unwrap();

        let config = VpnServerConfig {
            label: "Guest VPN".into(),
            enabled: false,
            listen_port: 51820,
            endpoint: "vpn.example.com".into(),
            private_key: Some(gen_key()),
        };
        let server_addr = Ipv4Addr::new(192, 168, 101, 254);

        set_wireguard_interface(&mut cfgs, "wg_guest", &config, server_addr).unwrap();

        let wg = cfgs["network"]
            .sections
            .iter()
            .find(|s| s.name().as_deref() == Some("wg_guest"))
            .and_then(|s| s.get::<WgInterface>().ok())
            .unwrap();

        assert_eq!(wg.listen_port, None, "disabled interface should have no listen_port");
    }

    #[test]
    fn test_set_vpn_server_metadata_creates() {
        let dir = tempfile::tempdir().unwrap();
        setup_base_configs(dir.path());

        let arena = Arena::new();
        let mut cfgs = parse_all(dir.path(), &arena, &["network", "startwrt", "firewall", "dhcp"]).unwrap();

        let config = VpnServerConfig {
            label: "Guest VPN".into(),
            enabled: true,
            listen_port: 51820,
            endpoint: "vpn.example.com".into(),
            private_key: None,
        };

        set_vpn_server_metadata(&mut cfgs, "wg_guest", "guest", &config).unwrap();

        let meta = cfgs["startwrt"]
            .sections
            .iter()
            .filter_map(|s| s.get::<UciVpnServer>().ok())
            .find(|m| m.interface == "wg_guest")
            .expect("vpn_server metadata should exist");

        assert_eq!(meta.profile_interface, "guest");
        assert_eq!(meta.label, "Guest VPN");
        assert_eq!(meta.listen_port, 51820);
        assert_eq!(meta.endpoint, "vpn.example.com");
    }

    #[test]
    fn test_set_vpn_server_metadata_updates() {
        let dir = tempfile::tempdir().unwrap();
        setup_with_vpn_server(dir.path());

        let arena = Arena::new();
        let mut cfgs = parse_all(dir.path(), &arena, &["network", "startwrt", "firewall", "dhcp"]).unwrap();

        let config = VpnServerConfig {
            label: "Updated VPN".into(),
            enabled: true,
            listen_port: 51821,
            endpoint: "new.example.com".into(),
            private_key: None,
        };

        set_vpn_server_metadata(&mut cfgs, "wg_guest", "guest", &config).unwrap();

        let metas: Vec<_> = cfgs["startwrt"]
            .sections
            .iter()
            .filter_map(|s| s.get::<UciVpnServer>().ok())
            .filter(|m| m.interface == "wg_guest")
            .collect();

        assert_eq!(metas.len(), 1, "should have exactly one vpn_server, no duplicate");
        assert_eq!(metas[0].label, "Updated VPN");
        assert_eq!(metas[0].listen_port, 51821);
        assert_eq!(metas[0].endpoint, "new.example.com");
    }

    #[test]
    fn test_ensure_firewall_zone() {
        let dir = tempfile::tempdir().unwrap();
        setup_base_configs(dir.path());

        let arena = Arena::new();
        let mut cfgs = parse_all(dir.path(), &arena, &["network", "startwrt", "firewall", "dhcp"]).unwrap();

        ensure_firewall_zone(&mut cfgs, "wg_guest", "guest").unwrap();

        let zone = cfgs["firewall"]
            .sections
            .iter()
            .filter_map(|s| s.get::<uciedit::openwrt::FirewallZone>().ok())
            .find(|z| z.name == "vlan_guest")
            .expect("vlan_guest zone should exist");

        assert!(zone.network.iter().any(|n| n == "wg_guest"), "wg_guest should be in zone network list");
        assert!(zone.network.iter().any(|n| n == "guest"), "guest should still be in zone network list");
    }

    #[test]
    fn test_ensure_firewall_zone_idempotent() {
        let dir = tempfile::tempdir().unwrap();
        setup_base_configs(dir.path());

        let arena = Arena::new();
        let mut cfgs = parse_all(dir.path(), &arena, &["network", "startwrt", "firewall", "dhcp"]).unwrap();

        ensure_firewall_zone(&mut cfgs, "wg_guest", "guest").unwrap();
        ensure_firewall_zone(&mut cfgs, "wg_guest", "guest").unwrap();

        let zone = cfgs["firewall"]
            .sections
            .iter()
            .filter_map(|s| s.get::<uciedit::openwrt::FirewallZone>().ok())
            .find(|z| z.name == "vlan_guest")
            .unwrap();

        let wg_count = zone.network.iter().filter(|n| *n == "wg_guest").count();
        assert_eq!(wg_count, 1, "wg_guest should appear exactly once after two calls");
    }

    #[test]
    fn test_ensure_firewall_zone_missing() {
        let dir = tempfile::tempdir().unwrap();
        setup_base_configs(dir.path());

        let arena = Arena::new();
        let mut cfgs = parse_all(dir.path(), &arena, &["network", "startwrt", "firewall", "dhcp"]).unwrap();

        let result = ensure_firewall_zone(&mut cfgs, "wg_nonexistent", "nonexistent");
        assert!(result.is_err());
        assert!(
            matches!(result.unwrap_err().kind, ErrorKind::MissingFirewallZone { .. }),
            "should return MissingFirewallZone error"
        );
    }

    #[test]
    fn test_ensure_wireguard_firewall_rule() {
        let dir = tempfile::tempdir().unwrap();
        setup_base_configs(dir.path());

        let arena = Arena::new();
        let mut cfgs = parse_all(dir.path(), &arena, &["network", "startwrt", "firewall", "dhcp"]).unwrap();

        ensure_wireguard_firewall_rule(&mut cfgs, "wg_guest", 51820).unwrap();

        let rule = cfgs["firewall"]
            .sections
            .iter()
            .filter_map(|s| s.get::<uciedit::openwrt::FirewallRule>().ok())
            .find(|r| r.name == "Allow-WireGuard-wg_guest")
            .expect("WireGuard firewall rule should exist");

        assert_eq!(rule.src, "wan");
        assert_eq!(rule.dest_port.as_deref(), Some("51820"));
        assert_eq!(rule.proto, vec!["udp"]);
        assert_eq!(rule.target, uciedit::openwrt::FirewallTarget::ACCEPT);
    }

    #[test]
    fn test_ensure_wireguard_firewall_rule_updates_port() {
        let dir = tempfile::tempdir().unwrap();
        setup_base_configs(dir.path());

        let arena = Arena::new();
        let mut cfgs = parse_all(dir.path(), &arena, &["network", "startwrt", "firewall", "dhcp"]).unwrap();

        ensure_wireguard_firewall_rule(&mut cfgs, "wg_guest", 51820).unwrap();
        ensure_wireguard_firewall_rule(&mut cfgs, "wg_guest", 51821).unwrap();

        let rules: Vec<_> = cfgs["firewall"]
            .sections
            .iter()
            .filter_map(|s| s.get::<uciedit::openwrt::FirewallRule>().ok())
            .filter(|r| r.name == "Allow-WireGuard-wg_guest")
            .collect();

        assert_eq!(rules.len(), 1, "should update existing rule, not create duplicate");
        assert_eq!(rules[0].dest_port.as_deref(), Some("51821"), "port should be updated");
    }

    #[test]
    fn test_remove_wireguard_firewall_rule() {
        let dir = tempfile::tempdir().unwrap();
        setup_with_vpn_server(dir.path());

        let arena = Arena::new();
        let mut cfgs = parse_all(dir.path(), &arena, &["network", "startwrt", "firewall", "dhcp"]).unwrap();

        // Verify rule exists before removal
        let has_rule = cfgs["firewall"]
            .sections
            .iter()
            .filter_map(|s| s.get::<uciedit::openwrt::FirewallRule>().ok())
            .any(|r| r.name == "Allow-WireGuard-wg_guest");
        assert!(has_rule, "rule should exist before removal");

        remove_wireguard_firewall_rule(&mut cfgs, "wg_guest").unwrap();

        let has_rule_after = cfgs["firewall"]
            .sections
            .iter()
            .filter_map(|s| s.get::<uciedit::openwrt::FirewallRule>().ok())
            .any(|r| r.name == "Allow-WireGuard-wg_guest");
        assert!(!has_rule_after, "rule should be gone after removal");

        // Other sections should survive
        let zone_count = cfgs["firewall"]
            .sections
            .iter()
            .filter_map(|s| s.get::<uciedit::openwrt::FirewallZone>().ok())
            .count();
        assert!(zone_count >= 3, "firewall zones should survive rule removal");
    }

    #[test]
    fn test_remove_from_firewall_zones() {
        let dir = tempfile::tempdir().unwrap();
        setup_with_vpn_server(dir.path());

        let arena = Arena::new();
        let mut cfgs = parse_all(dir.path(), &arena, &["network", "startwrt", "firewall", "dhcp"]).unwrap();

        remove_from_firewall_zones(&mut cfgs, "wg_guest").unwrap();

        let zone = cfgs["firewall"]
            .sections
            .iter()
            .filter_map(|s| s.get::<uciedit::openwrt::FirewallZone>().ok())
            .find(|z| z.name == "vlan_guest")
            .unwrap();

        assert!(!zone.network.iter().any(|n| n == "wg_guest"), "wg_guest should be removed");
        assert!(zone.network.iter().any(|n| n == "guest"), "guest should remain");
    }

    #[test]
    fn test_add_single_peer() {
        let dir = tempfile::tempdir().unwrap();
        setup_with_vpn_server(dir.path());

        let arena = Arena::new();
        let mut cfgs = parse_all(dir.path(), &arena, &["network", "startwrt", "firewall", "dhcp"]).unwrap();

        let peer_key = gen_key();
        let psk = Base64::new(generate_psk()).to_base64();
        let peer = VpnServerPeer {
            name: "Tablet".into(),
            ip: Some(Ipv4Addr::new(192, 168, 101, 202)),
            public_key: Some(peer_key.clone()),
            preshared_key: Some(psk.clone()),
            route_all: None,
        };

        add_single_peer(&mut cfgs, "wg_guest", &peer, &arena).unwrap();

        // Verify the peer was created
        let peer_sections: Vec<_> = cfgs["network"]
            .sections
            .iter()
            .filter(|s| s.ty() == "wireguard_wg_guest")
            .collect();

        // 2 existing + 1 new
        assert_eq!(peer_sections.len(), 3);

        // Find our new peer by public key
        let new_peer = peer_sections
            .iter()
            .find(|s| {
                s.lines.iter().any(|l| matches!(l, Line::Option { option, value, .. }
                    if option.as_str() == "public_key" && value.as_str() == peer_key))
            })
            .expect("new peer should exist");

        // Verify fields
        let has_description = new_peer.lines.iter().any(|l| matches!(l, Line::Option { option, value, .. }
            if option.as_str() == "description" && value.as_str() == "Tablet"));
        assert!(has_description, "should have description");

        let has_psk = new_peer.lines.iter().any(|l| matches!(l, Line::Option { option, value, .. }
            if option.as_str() == "preshared_key" && value.as_str() == psk));
        assert!(has_psk, "should have preshared_key");

        let has_keepalive = new_peer.lines.iter().any(|l| matches!(l, Line::Option { option, value, .. }
            if option.as_str() == "persistent_keepalive" && value.as_str() == "25"));
        assert!(has_keepalive, "should have persistent_keepalive=25");

        let has_route = new_peer.lines.iter().any(|l| matches!(l, Line::Option { option, value, .. }
            if option.as_str() == "route_allowed_ips" && value.as_str() == "1"));
        assert!(has_route, "should have route_allowed_ips=1");

        let has_ip = new_peer.lines.iter().any(|l| matches!(l, Line::List { list, item, .. }
            if list.as_str() == "allowed_ips" && item.as_str() == "192.168.101.202/32"));
        assert!(has_ip, "should have allowed_ips with correct IP/32");
    }

    #[test]
    fn test_add_peer_index_after_deletion() {
        let dir = tempfile::tempdir().unwrap();
        setup_with_vpn_server(dir.path()); // has peers _0 and _1

        let arena = Arena::new();
        let mut cfgs = parse_all(dir.path(), &arena, &["network", "startwrt", "firewall", "dhcp"]).unwrap();

        // Remove peer _0 (middle deletion scenario)
        cfgs["network"].sections.retain(|s| {
            s.name().as_deref() != Some("wg_guest_0")
        });

        // Add a new peer → should be _2 (max existing suffix + 1), not _1 (count)
        let peer = VpnServerPeer {
            name: "New Device".into(),
            ip: Some(Ipv4Addr::new(192, 168, 101, 202)),
            public_key: Some(gen_key()),
            preshared_key: None,
            route_all: None,
        };
        add_single_peer(&mut cfgs, "wg_guest", &peer, &arena).unwrap();

        let has_2 = cfgs["network"]
            .sections
            .iter()
            .any(|s| s.name().as_deref() == Some("wg_guest_2"));
        assert!(has_2, "new peer should be named wg_guest_2 (max existing suffix + 1)");

        let has_0 = cfgs["network"]
            .sections
            .iter()
            .any(|s| s.name().as_deref() == Some("wg_guest_0"));
        assert!(!has_0, "wg_guest_0 should not exist (was deleted)");

        // _1 should still exist
        let has_1 = cfgs["network"]
            .sections
            .iter()
            .any(|s| s.name().as_deref() == Some("wg_guest_1"));
        assert!(has_1, "wg_guest_1 should still exist");
    }

    #[test]
    fn test_get_peers_for_interface() {
        let dir = tempfile::tempdir().unwrap();
        let (_, peer0_key, peer1_key) = setup_with_vpn_server(dir.path());

        let arena = Arena::new();
        let cfgs = parse_all(dir.path(), &arena, &["network", "startwrt", "firewall", "dhcp"]).unwrap();

        let peers = get_peers_for_interface(&cfgs, "wg_guest");

        assert_eq!(peers.len(), 2);
        assert_eq!(peers[0].name, "Phone");
        assert_eq!(peers[0].ip, Some(Ipv4Addr::new(192, 168, 101, 200)));
        assert_eq!(peers[0].public_key.as_deref(), Some(peer0_key.as_str()));
        assert_eq!(peers[0].preshared_key, None, "PSK should never be exposed in list");

        assert_eq!(peers[1].name, "Laptop");
        assert_eq!(peers[1].ip, Some(Ipv4Addr::new(192, 168, 101, 201)));
        assert_eq!(peers[1].public_key.as_deref(), Some(peer1_key.as_str()));

        // Peers from setup_with_vpn_server have route_all: None
        assert_eq!(peers[0].route_all, None, "default peers should have route_all=None");
        assert_eq!(peers[1].route_all, None, "default peers should have route_all=None");
    }

    #[test]
    fn test_add_peer_route_all_stored_in_uci() {
        let dir = tempfile::tempdir().unwrap();
        setup_with_vpn_server(dir.path());

        let arena = Arena::new();
        let mut cfgs = parse_all(dir.path(), &arena, &["network", "startwrt", "firewall", "dhcp"]).unwrap();

        // Add peer with route_all = true
        let peer_key = gen_key();
        let peer = VpnServerPeer {
            name: "Full Tunnel".into(),
            ip: Some(Ipv4Addr::new(192, 168, 101, 202)),
            public_key: Some(peer_key.clone()),
            preshared_key: None,
            route_all: Some(true),
        };
        add_single_peer(&mut cfgs, "wg_guest", &peer, &arena).unwrap();

        // Find the new peer section
        let new_peer = cfgs["network"]
            .sections
            .iter()
            .find(|s| {
                s.lines.iter().any(|l| matches!(l, Line::Option { option, value, .. }
                    if option.as_str() == "public_key" && value.as_str() == peer_key))
            })
            .expect("new peer should exist");

        // Should have startwrt_route_all = 1
        let has_route_all = new_peer.lines.iter().any(|l| matches!(l, Line::Option { option, value, .. }
            if option.as_str() == "startwrt_route_all" && value.as_str() == "1"));
        assert!(has_route_all, "route_all peer should have startwrt_route_all=1 in UCI");
    }

    #[test]
    fn test_add_peer_route_all_absent_when_false() {
        let dir = tempfile::tempdir().unwrap();
        setup_with_vpn_server(dir.path());

        let arena = Arena::new();
        let mut cfgs = parse_all(dir.path(), &arena, &["network", "startwrt", "firewall", "dhcp"]).unwrap();

        // Add peer with route_all = None (split tunnel default)
        let peer_key = gen_key();
        let peer = VpnServerPeer {
            name: "Split Tunnel".into(),
            ip: Some(Ipv4Addr::new(192, 168, 101, 202)),
            public_key: Some(peer_key.clone()),
            preshared_key: None,
            route_all: None,
        };
        add_single_peer(&mut cfgs, "wg_guest", &peer, &arena).unwrap();

        let new_peer = cfgs["network"]
            .sections
            .iter()
            .find(|s| {
                s.lines.iter().any(|l| matches!(l, Line::Option { option, value, .. }
                    if option.as_str() == "public_key" && value.as_str() == peer_key))
            })
            .expect("new peer should exist");

        // Should NOT have startwrt_route_all option
        let has_route_all = new_peer.lines.iter().any(|l| matches!(l, Line::Option { option, .. }
            if option.as_str() == "startwrt_route_all"));
        assert!(!has_route_all, "split tunnel peer should not have startwrt_route_all in UCI");
    }

    #[test]
    fn test_get_peers_reads_route_all() {
        let dir = tempfile::tempdir().unwrap();
        setup_with_vpn_server(dir.path());

        let arena = Arena::new();
        let mut cfgs = parse_all(dir.path(), &arena, &["network", "startwrt", "firewall", "dhcp"]).unwrap();

        // Add a peer with route_all = true
        let peer = VpnServerPeer {
            name: "Full Tunnel".into(),
            ip: Some(Ipv4Addr::new(192, 168, 101, 202)),
            public_key: Some(gen_key()),
            preshared_key: None,
            route_all: Some(true),
        };
        add_single_peer(&mut cfgs, "wg_guest", &peer, &arena).unwrap();

        let peers = get_peers_for_interface(&cfgs, "wg_guest");

        assert_eq!(peers.len(), 3);
        // First two peers from setup have no route_all
        assert_eq!(peers[0].route_all, None);
        assert_eq!(peers[1].route_all, None);
        // Third peer has route_all = true
        assert_eq!(peers[2].route_all, Some(true));
    }

    #[test]
    fn test_get_reserved_ips_includes_dhcp() {
        let dir = tempfile::tempdir().unwrap();
        setup_with_vpn_server(dir.path());

        // Add a DHCP static lease in the guest subnet
        let dhcp_content = std::fs::read_to_string(dir.path().join("dhcp")).unwrap();
        std::fs::write(
            dir.path().join("dhcp"),
            format!(
                "{}\nconfig host\n\toption name 'server'\n\toption mac '00:11:22:33:44:55'\n\toption ip '192.168.101.205'\n",
                dhcp_content
            ),
        )
        .unwrap();

        let arena = Arena::new();
        let cfgs = parse_all(dir.path(), &arena, &["network", "startwrt", "firewall", "dhcp"]).unwrap();

        let gateway = Ipv4Addr::new(192, 168, 101, 1);
        let reserved = get_reserved_ips(&cfgs, "wg_guest", gateway);

        assert!(reserved.contains(&Ipv4Addr::new(192, 168, 101, 200)), "VPN peer .200 should be reserved");
        assert!(reserved.contains(&Ipv4Addr::new(192, 168, 101, 201)), "VPN peer .201 should be reserved");
        assert!(reserved.contains(&Ipv4Addr::new(192, 168, 101, 205)), "DHCP lease .205 should be reserved");
    }

    #[test]
    fn test_get_reserved_ips_ignores_other_subnets() {
        let dir = tempfile::tempdir().unwrap();
        setup_with_vpn_server(dir.path());

        // Add a DHCP static lease in a different subnet
        let dhcp_content = std::fs::read_to_string(dir.path().join("dhcp")).unwrap();
        std::fs::write(
            dir.path().join("dhcp"),
            format!(
                "{}\nconfig host\n\toption name 'other'\n\toption mac '00:11:22:33:44:55'\n\toption ip '192.168.1.50'\n",
                dhcp_content
            ),
        )
        .unwrap();

        let arena = Arena::new();
        let cfgs = parse_all(dir.path(), &arena, &["network", "startwrt", "firewall", "dhcp"]).unwrap();

        let gateway = Ipv4Addr::new(192, 168, 101, 1);
        let reserved = get_reserved_ips(&cfgs, "wg_guest", gateway);

        assert!(!reserved.contains(&Ipv4Addr::new(192, 168, 1, 50)), "lease on different subnet should not be reserved");
    }

    // === Integration-style tests ===

    #[test]
    fn test_full_create_and_delete_flow() {
        let dir = tempfile::tempdir().unwrap();
        setup_base_configs(dir.path());

        let arena = Arena::new();
        let mut cfgs = parse_all(dir.path(), &arena, &["network", "startwrt", "firewall", "dhcp"]).unwrap();

        let config = VpnServerConfig {
            label: "Guest VPN".into(),
            enabled: true,
            listen_port: 51820,
            endpoint: "vpn.example.com".into(),
            private_key: Some(gen_key()),
        };
        let server_addr = Ipv4Addr::new(192, 168, 101, 254);

        // Create VPN server (all 4 helpers)
        set_wireguard_interface(&mut cfgs, "wg_guest", &config, server_addr).unwrap();
        set_vpn_server_metadata(&mut cfgs, "wg_guest", "guest", &config).unwrap();
        ensure_firewall_zone(&mut cfgs, "wg_guest", "guest").unwrap();
        ensure_wireguard_firewall_rule(&mut cfgs, "wg_guest", 51820).unwrap();

        // Add 2 peers
        let peer1 = VpnServerPeer {
            name: "Phone".into(),
            ip: Some(Ipv4Addr::new(192, 168, 101, 200)),
            public_key: Some(gen_key()),
            preshared_key: Some(Base64::new(generate_psk()).to_base64()),
            route_all: None,
        };
        let peer2 = VpnServerPeer {
            name: "Laptop".into(),
            ip: Some(Ipv4Addr::new(192, 168, 101, 201)),
            public_key: Some(gen_key()),
            preshared_key: Some(Base64::new(generate_psk()).to_base64()),
            route_all: None,
        };
        add_single_peer(&mut cfgs, "wg_guest", &peer1, &arena).unwrap();
        add_single_peer(&mut cfgs, "wg_guest", &peer2, &arena).unwrap();

        // Verify everything exists
        assert!(cfgs["network"].sections.iter().any(|s| s.name().as_deref() == Some("wg_guest")));
        assert_eq!(cfgs["network"].sections.iter().filter(|s| s.ty() == "wireguard_wg_guest").count(), 2);
        assert!(cfgs["startwrt"].sections.iter().filter_map(|s| s.get::<UciVpnServer>().ok()).any(|m| m.interface == "wg_guest"));

        // Now delete — mimicking vpn_server::delete logic
        cfgs["network"].sections.retain(|section| {
            if section.name().as_deref() == Some("wg_guest") {
                if let Ok(iface) = section.get::<WgInterface>() {
                    if iface.is_wireguard() {
                        return false;
                    }
                }
            }
            true
        });
        cfgs["network"].sections.retain(|section| section.ty() != "wireguard_wg_guest");
        cfgs["startwrt"].sections.retain(|section| {
            if let Ok(meta) = section.get::<UciVpnServer>() {
                return meta.interface != "wg_guest";
            }
            true
        });
        remove_from_firewall_zones(&mut cfgs, "wg_guest").unwrap();
        remove_wireguard_firewall_rule(&mut cfgs, "wg_guest").unwrap();

        // Verify cleanup
        assert!(!cfgs["network"].sections.iter().any(|s| s.name().as_deref() == Some("wg_guest")));
        assert_eq!(cfgs["network"].sections.iter().filter(|s| s.ty() == "wireguard_wg_guest").count(), 0);
        assert!(!cfgs["startwrt"].sections.iter().filter_map(|s| s.get::<UciVpnServer>().ok()).any(|m| m.interface == "wg_guest"));

        let zone = cfgs["firewall"]
            .sections
            .iter()
            .filter_map(|s| s.get::<uciedit::openwrt::FirewallZone>().ok())
            .find(|z| z.name == "vlan_guest")
            .unwrap();
        assert!(!zone.network.iter().any(|n| n == "wg_guest"), "wg_guest should be removed from zone");
        assert!(zone.network.iter().any(|n| n == "guest"), "guest should still be in zone");

        let has_wg_rule = cfgs["firewall"]
            .sections
            .iter()
            .filter_map(|s| s.get::<uciedit::openwrt::FirewallRule>().ok())
            .any(|r| r.name == "Allow-WireGuard-wg_guest");
        assert!(!has_wg_rule);

        // Profile still exists
        let profile_count = cfgs["startwrt"]
            .sections
            .iter()
            .filter(|s| s.ty() == "profile")
            .count();
        assert_eq!(profile_count, 2, "both profiles should survive VPN deletion");
    }

    #[test]
    fn test_enable_disable_toggle() {
        let dir = tempfile::tempdir().unwrap();
        setup_base_configs(dir.path());

        let arena = Arena::new();
        let mut cfgs = parse_all(dir.path(), &arena, &["network", "startwrt", "firewall", "dhcp"]).unwrap();

        let server_addr = Ipv4Addr::new(192, 168, 101, 254);

        // Create enabled
        let config_enabled = VpnServerConfig {
            label: "Guest VPN".into(),
            enabled: true,
            listen_port: 51820,
            endpoint: "vpn.example.com".into(),
            private_key: Some(gen_key()),
        };
        set_wireguard_interface(&mut cfgs, "wg_guest", &config_enabled, server_addr).unwrap();
        ensure_wireguard_firewall_rule(&mut cfgs, "wg_guest", 51820).unwrap();

        // Verify enabled state
        let wg = cfgs["network"].sections.iter()
            .find(|s| s.name().as_deref() == Some("wg_guest"))
            .and_then(|s| s.get::<WgInterface>().ok()).unwrap();
        assert_eq!(wg.listen_port, Some(51820));

        // Disable
        let config_disabled = VpnServerConfig {
            label: "Guest VPN".into(),
            enabled: false,
            listen_port: 51820,
            endpoint: "vpn.example.com".into(),
            private_key: None,
        };
        set_wireguard_interface(&mut cfgs, "wg_guest", &config_disabled, server_addr).unwrap();
        remove_wireguard_firewall_rule(&mut cfgs, "wg_guest").unwrap();

        let wg = cfgs["network"].sections.iter()
            .find(|s| s.name().as_deref() == Some("wg_guest"))
            .and_then(|s| s.get::<WgInterface>().ok()).unwrap();
        assert_eq!(wg.listen_port, None, "disabled should have no listen_port");

        let has_rule = cfgs["firewall"].sections.iter()
            .filter_map(|s| s.get::<uciedit::openwrt::FirewallRule>().ok())
            .any(|r| r.name == "Allow-WireGuard-wg_guest");
        assert!(!has_rule, "disabled should have no WAN rule");

        // Re-enable
        let config_reenable = VpnServerConfig {
            label: "Guest VPN".into(),
            enabled: true,
            listen_port: 51820,
            endpoint: "vpn.example.com".into(),
            private_key: None,
        };
        set_wireguard_interface(&mut cfgs, "wg_guest", &config_reenable, server_addr).unwrap();
        ensure_wireguard_firewall_rule(&mut cfgs, "wg_guest", 51820).unwrap();

        let wg = cfgs["network"].sections.iter()
            .find(|s| s.name().as_deref() == Some("wg_guest"))
            .and_then(|s| s.get::<WgInterface>().ok()).unwrap();
        assert_eq!(wg.listen_port, Some(51820), "re-enabled should restore listen_port");

        let has_rule = cfgs["firewall"].sections.iter()
            .filter_map(|s| s.get::<uciedit::openwrt::FirewallRule>().ok())
            .any(|r| r.name == "Allow-WireGuard-wg_guest");
        assert!(has_rule, "re-enabled should restore WAN rule");
    }
}
