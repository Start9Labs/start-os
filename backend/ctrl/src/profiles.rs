use crate::dns::{self, DnsServer};
use crate::ethernet::find_lan_bridge;
use crate::system::UciPreferences;
use crate::utils::DeserializeStdin;
use crate::CtrlContext;
use crate::{utils::HandlerExtSerde, Error, ErrorKind};
use clap::Parser;
use color_eyre::eyre::{eyre, OptionExt};
use rpc_toolkit::{from_fn, HandlerExt as _, ParentHandler};
use serde::{Deserialize, Serialize};
use std::cell::OnceCell;
use std::collections::{BTreeMap, BTreeSet, HashMap};
use std::io::Read;
use std::net::Ipv4Addr;
use std::path::PathBuf;
use std::process::{Command, Stdio};
use uciedit::openwrt::{
    DeviceType, Dhcp, FirewallForwarding, FirewallRedirect, FirewallRule, FirewallTarget,
    FirewallZone, InterfaceProto, NetworkBridgeVlan, NetworkDevice, NetworkInterface, NetworkRoute,
    NetworkRule, NetworkVlanPort, NetworkVlanPortTagging, ProfileDnsmasq, WifiStation, WifiVlan,
};
use uciedit::{dump_all, parse_all, Arena, Configs, Line, LineComment, Token, TypedSection};

pub const DEFAULT_WAN_ZONE: &str = "wan";
pub const INTERFACE_NAME_LIMIT: usize = 5;

#[derive(Debug, Serialize, Deserialize)]
pub enum LanAccess<Id: Ord> {
    #[serde(rename = "ALL")]
    All,
    #[serde(rename = "SAME_PROFILE")]
    SameProfile,
    #[serde(rename = "other_profiles")]
    OtherProfiles(BTreeSet<Id>),
}

#[derive(Debug, Serialize, Deserialize, Clone)]
pub enum WanAccess {
    #[serde(rename = "ALL")]
    All,
    #[serde(rename = "NONE")]
    None,
    #[serde(rename = "whitelist")]
    Whitelist(Vec<String>), // List of allowed destination IPs/CIDRs
    #[serde(rename = "blacklist")]
    Blacklist(Vec<String>), // List of blocked destination IPs/CIDRs
}

#[derive(Debug, Serialize, Deserialize, Clone)]
#[serde(rename_all = "camelCase")]
pub struct ScheduleWindow {
    pub start_time: String,
    pub end_time: String,
    pub days: [bool; 7],
}

#[derive(Debug, Serialize, Deserialize, Clone)]
pub struct ScheduleWindows {
    pub interface: String,
    pub windows: Vec<ScheduleWindow>,
}

#[derive(Debug, Serialize, Deserialize)]
pub struct Profile<Id: Ord = ProfileId> {
    #[serde(flatten)]
    pub id: Id,
    pub gateway_ip: Ipv4Addr,
    pub outbound: String,
    pub lan_access: LanAccess<Id>,
    pub wan_access: WanAccess,
    #[serde(default)]
    pub dns_override: Vec<DnsServer>,
    #[serde(default)]
    pub dns_source: String,
    pub access_to_new_profiles: bool,
    pub owns_lan: bool,
}

#[derive(Debug, Deserialize, Serialize)]
pub struct ProfileSetRequest {
    #[serde(flatten)]
    pub profile: Profile<ProfileIdOpt>,
    #[serde(default)]
    pub force: bool,
}

pub fn profiles<C: CtrlContext>() -> ParentHandler<C> {
    ParentHandler::new()
        .subcommand("get", from_fn(get::<C>).with_display_serializable())
        .subcommand("set", from_fn(set::<C>).with_display_serializable())
        .subcommand("delete", from_fn(delete::<C>).no_display())
        .subcommand("list", from_fn(list::<C>).with_display_serializable())
        .subcommand("create", from_fn(create::<C>).with_display_serializable())
        .subcommand("edit", from_fn(edit::<C>).with_display_serializable())
        .subcommand(
            "schedule-get",
            from_fn(schedule_get::<C>).with_display_serializable(),
        )
        .subcommand(
            "schedule-set",
            from_fn(schedule_set::<C>).no_display(),
        )
}

#[derive(Debug, Deserialize, Serialize, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct ProfileId {
    pub fullname: String,
    pub interface: String,
    pub vlan_tag: u16,
}

#[derive(Debug, Deserialize, Serialize, Clone, PartialEq, Eq, PartialOrd, Ord, Hash, Parser)]
pub struct ProfileIdOpt {
    #[clap(short, long)]
    pub fullname: Option<String>,
    #[clap(short, long)]
    pub interface: Option<String>,
    #[clap(short, long)]
    pub vlan_tag: Option<u16>,
}

impl From<ProfileId> for ProfileIdOpt {
    fn from(id: ProfileId) -> Self {
        ProfileIdOpt {
            fullname: Some(id.fullname),
            interface: Some(id.interface),
            vlan_tag: Some(id.vlan_tag),
        }
    }
}

impl ProfileIdOpt {
    pub fn matches(&self, other: &ProfileIdOpt) -> bool {
        let i = match (&self.interface, &other.interface) {
            (Some(a), Some(b)) => a == b,
            _ => true,
        };
        let v = match (self.vlan_tag, other.vlan_tag) {
            (Some(a), Some(b)) => a == b,
            _ => true,
        };
        let n = match (&self.fullname, &other.fullname) {
            (Some(a), Some(b)) => a == b,
            _ => true,
        };
        i && v && n
    }
}

#[derive(Debug, TypedSection)]
#[uci(ty = "profile")]
pub(crate) struct UciProfile {
    pub fullname: String,
    pub interface: String,
    pub vlan_tag: u16,
    pub outbound: Option<String>,
    #[uci(default_value = "false")]
    pub access_to_new_profiles: bool,
    #[uci(default)]
    pub wan_access: Option<String>,
    #[uci(default)]
    pub wan_access_list: Vec<String>,
    #[uci(default)]
    pub dns_override: Vec<String>,
    #[uci(default)]
    pub wan_schedule: Vec<String>,
}

/// Snapshot of profile fields before an update, for activity log diffing.
struct OldProfileState {
    outbound: Option<String>,
    wan_access: Option<String>,
    access_to_new_profiles: bool,
    dns_override: Vec<String>,
    gateway_ip: Option<Ipv4Addr>,
}

impl UciProfile {
    pub fn id(&self) -> ProfileId {
        ProfileId {
            fullname: self.fullname.clone(),
            interface: self.interface.clone(),
            vlan_tag: self.vlan_tag,
        }
    }
}

fn wan_access_type_str(wa: &WanAccess) -> String {
    match wa {
        WanAccess::All => "all".into(),
        WanAccess::None => "none".into(),
        WanAccess::Whitelist(_) => "whitelist".into(),
        WanAccess::Blacklist(_) => "blacklist".into(),
    }
}

fn wan_access_destinations(wa: &WanAccess) -> Vec<String> {
    match wa {
        WanAccess::Whitelist(d) | WanAccess::Blacklist(d) => d.clone(),
        _ => Vec::new(),
    }
}

fn get_vpn_dns(cfgs: &Configs, vpn_interface: &str) -> Vec<String> {
    for section in &cfgs["network"].sections {
        if let Ok(iface) = section.get::<NetworkInterface>() {
            if section.name().as_deref() == Some(vpn_interface) && !iface.dns.is_empty() {
                return iface.dns.iter().map(|s| s.to_string()).collect();
            }
        }
    }
    Vec::new()
}

fn compute_dns_source(cfgs: &Configs, profile: &Profile) -> String {
    if !profile.dns_override.is_empty() {
        "custom".into()
    } else if profile.outbound != "wan" && !get_vpn_dns(cfgs, &profile.outbound).is_empty() {
        "vpn".into()
    } else {
        "system".into()
    }
}

/// Returns true if the profile has non-system DNS (custom override or VPN DNS).
fn has_effective_dns(cfgs: &Configs, profile: &Profile) -> bool {
    !profile.dns_override.is_empty()
        || (profile.outbound != "wan" && !get_vpn_dns(cfgs, &profile.outbound).is_empty())
}

/// Rewrite DNS forwarding (dnsmasq sections) for ALL profiles.
/// Call after any operation that changes system DNS settings.
pub(crate) fn rewrite_all_dns_forwarding(cfgs: &mut Configs) -> Result<(), Error> {
    // Collect all profiles first to avoid borrow issues
    let profiles: Vec<Profile> = cfgs["startwrt"]
        .sections
        .iter()
        .filter_map(|section| {
            let uci_profile = section.get::<UciProfile>().ok()?;
            let dns_override = dns::parse_dns_server_list(&uci_profile.dns_override);
            Some(Profile {
                id: uci_profile.id(),
                gateway_ip: {
                    // Look up gateway from network config
                    let interface = &uci_profile.interface;
                    cfgs["network"]
                        .sections
                        .iter()
                        .find_map(|s| {
                            if s.name().as_deref() == Some(interface) {
                                s.get::<NetworkInterface>().ok().and_then(|i| i.ipaddr)
                            } else {
                                None
                            }
                        })
                        .unwrap_or(Ipv4Addr::new(0, 0, 0, 0))
                },
                outbound: uci_profile.outbound.unwrap_or_else(|| "wan".into()),
                lan_access: LanAccess::All, // Not used by rewrite_dns_forwarding
                wan_access: WanAccess::All,  // Not used by rewrite_dns_forwarding
                dns_override,
                dns_source: String::new(),
                access_to_new_profiles: false,
                owns_lan: false,
            })
        })
        .collect();

    for profile in &profiles {
        rewrite_dns_forwarding(cfgs, profile)?;
        rewrite_dns_redirect(cfgs, profile)?;
    }
    Ok(())
}

/// Rewrite the DNS-Override DNAT redirect rule for a single profile.
/// Adds or removes the redirect based on whether the profile needs DNS hijacking.
fn rewrite_dns_redirect(cfgs: &mut Configs, profile: &Profile) -> Result<(), Error> {
    // Find this profile's firewall zone name
    let zone_name = cfgs["firewall"]
        .sections
        .iter()
        .find_map(|s| {
            let zone = s.get::<FirewallZone>().ok()?;
            if zone.network.iter().any(|n| n == &profile.id.interface) {
                Some(zone.name.clone())
            } else {
                None
            }
        })
        .unwrap_or_else(|| format!("vlan_{}", profile.id.interface));

    // Remove existing DNS-Override redirect for this profile
    cfgs["firewall"].sections.retain(|section| {
        let Ok(redir) = section.get::<FirewallRedirect>() else {
            return true;
        };
        !(redir.src == zone_name && redir.name.contains("DNS-Override"))
    });

    // Add redirect if this profile needs DNS hijacking
    if has_effective_dns(cfgs, profile) || !dns::get_system_dns_servers(cfgs).is_empty() {
        cfgs["firewall"].append(
            &FirewallRedirect {
                name: format!(
                    "DNS-Override-{}",
                    profile.id.fullname.replace(" ", "-"),
                ),
                src: zone_name,
                proto: vec!["tcp".into(), "udp".into()],
                src_dport: Some("53".into()),
                dest_ip: Some(profile.gateway_ip.to_string()),
                dest_port: Some("53".into()),
                target: "DNAT".into(),
                ..Default::default()
            },
            None,
        )?;
    }
    Ok(())
}

pub(crate) fn rewrite_dns_forwarding(cfgs: &mut Configs, profile: &Profile) -> Result<(), Error> {
    let section_name = format!("dns_{}", profile.id.interface);

    // Remove any existing per-profile dnsmasq section
    cfgs["dhcp"]
        .sections
        .retain(|s| s.name().as_deref() != Some(&section_name));

    // Remove notinterface entry for this profile from main dnsmasq
    for section in &mut cfgs["dhcp"].sections {
        if section.ty() == "dnsmasq"
            && !section
                .name()
                .map(|n| n.starts_with("dns_"))
                .unwrap_or(false)
        {
            section.lines.retain(|line| {
                !matches!(line, Line::List { list, item, .. }
                    if list.as_str() == "notinterface"
                        && item.as_str() == profile.id.interface.as_str())
            });
            break;
        }
    }

    // Determine the dnsmasq server list based on DNS source:
    // 1. Custom DNS → SmartDNS profile group
    // 2. VPN DNS → dnsmasq server IP@interface directly (SmartDNS can't bind to VPN iface)
    // 3. System DNS (custom) → SmartDNS system group
    // 4. System DNS (ISP) → no per-profile dnsmasq (uses default resolvfile)
    let servers: Vec<String> = if !profile.dns_override.is_empty() {
        // Custom DNS: route through SmartDNS profile group
        let port = dns::smartdns_port_for_vlan(profile.id.vlan_tag);
        vec![format!("127.0.0.1#{}", port)]
    } else if profile.outbound != "wan" {
        // VPN DNS: use dnsmasq server IP@interface directly
        let vpn_dns = get_vpn_dns(cfgs, &profile.outbound);
        if vpn_dns.is_empty() {
            vec![]
        } else {
            vpn_dns
                .into_iter()
                .map(|ip| format!("{}@{}", ip, profile.outbound))
                .collect()
        }
    } else {
        // System DNS: route through SmartDNS system group if configured
        let system_dns = dns::get_system_dns_servers(cfgs);
        if !system_dns.is_empty() {
            vec![format!("127.0.0.1#{}", dns::SMARTDNS_SYSTEM_PORT)]
        } else {
            vec![] // ISP mode — no per-profile dnsmasq needed
        }
    };

    if !servers.is_empty() {
        cfgs["dhcp"].append(
            &ProfileDnsmasq {
                server: servers,
                noresolv: Some("1".to_string()),
                interface: vec![],
                localservice: Some("1".to_string()),
                nonwildcard: Some("1".to_string()),
                listen_address: vec![profile.gateway_ip.to_string()],
                notinterface: vec!["loopback".to_string()],
                rebind_domain: vec![],
                rebind_protection: Some("0".to_string()),
                localuse: Some("0".to_string()),
                leasefile: Some(format!("/tmp/dhcp.leases.{}", section_name)),
                domain: Some("lan".to_string()),
                expandhosts: Some("1".to_string()),
                boguspriv: Some("0".to_string()),
                local: Some("/lan/".to_string()),
            },
            Some(&section_name),
        )?;

        // Add notinterface to main dnsmasq
        for section in &mut cfgs["dhcp"].sections {
            if section.ty() == "dnsmasq"
                && !section
                    .name()
                    .map(|n| n.starts_with("dns_"))
                    .unwrap_or(false)
            {
                let arena = section.arena;
                section.lines.push(Line::List {
                    list: Token::from_string("notinterface".to_string(), arena),
                    item: Token::from_string(profile.id.interface.clone(), arena),
                    comment: uciedit::LineComment::None,
                });
                break;
            }
        }
    }

    Ok(())
}

pub fn get<C: CtrlContext>(ctx: C, query: ProfileIdOpt) -> Result<Profile, Error> {
    let arena = Arena::new();
    let cfgs = parse_all(ctx.uci_root(), &arena, &["startwrt", "network", "firewall"])?;
    get_config(ctx, &cfgs, query)
}

fn get_config(
    ctx: impl CtrlContext,
    cfgs: &Configs,
    query: ProfileIdOpt,
) -> Result<Profile, Error> {
    let lookup = Lookup::parse(ctx.clone(), cfgs)?;
    let id = lookup.resolve(&query)?;
    let mut uciprofile = None;
    cfgs["startwrt"].each::<UciProfile, Error>(|_, profile| {
        if &profile.id() == id {
            uciprofile = Some(profile);
        }
    })?;
    let mut wip_profile = Profile {
        id: id.clone(),
        owns_lan: id.interface == "lan",
        gateway_ip: Ipv4Addr::new(0, 0, 0, 0),
        outbound: match &uciprofile {
            Some(p) => p.outbound.clone().unwrap_or_else(|| "wan".to_string()),
            None => "wan".to_string(),
        },
        lan_access: LanAccess::SameProfile,
        wan_access: WanAccess::None,
        dns_override: uciprofile
            .as_ref()
            .map(|p| dns::parse_dns_server_list(&p.dns_override))
            .unwrap_or_default(),
        dns_source: String::new(),
        access_to_new_profiles: match &uciprofile {
            Some(p) => p.access_to_new_profiles,
            None => false,
        },
    };
    let mut found = false;
    for section in &cfgs["network"].sections {
        if let Ok(iface) = section.get::<NetworkInterface>() {
            let Some(name) = section.name() else { continue };
            if name == id.interface {
                if iface.proto != InterfaceProto::STATIC {
                    return Err(ErrorKind::CorruptedProfile { id: query.clone() }.into());
                }
                if let Some(ip) = iface.ipaddr {
                    wip_profile.gateway_ip = ip;
                } else {
                    return Err(ErrorKind::CorruptedProfile { id: query.clone() }.into());
                }
                found = true;
                break;
            }
        }
    }
    if !found {
        return Err(ErrorKind::CorruptedProfile { id: query.clone() }.into());
    }
    let mut this_zone_name = format!("vlan_{}", id.interface);
    cfgs["firewall"].each::<FirewallZone, Error>(|_, zone| {
        for zone_interface in zone.network {
            if id.interface == zone_interface {
                this_zone_name = zone.name.clone();
            }
        }
    })?;
    wip_profile.wan_access = match uciprofile.as_ref().and_then(|p| p.wan_access.as_deref()) {
        Some("whitelist") => WanAccess::Whitelist(
            uciprofile
                .as_ref()
                .map(|p| p.wan_access_list.clone())
                .unwrap_or_default(),
        ),
        Some("blacklist") => WanAccess::Blacklist(
            uciprofile
                .as_ref()
                .map(|p| p.wan_access_list.clone())
                .unwrap_or_default(),
        ),
        Some("none") => WanAccess::None,
        _ => WanAccess::All,
    };
    let mut forwarding = BTreeSet::new();
    cfgs["firewall"].each::<FirewallForwarding, Error>(|_, FirewallForwarding { src, dest }| {
        if src == this_zone_name && src != dest {
            forwarding.insert(dest);
        }
    })?;
    let mut other_profiles = BTreeSet::new();
    cfgs["firewall"].each::<FirewallZone, Error>(|_, FirewallZone { name, network, .. }| {
        if forwarding.contains(&name) {
            for other_interface in network {
                if let Some(other) = lookup.from_interface(&other_interface) {
                    other_profiles.insert(other.clone());
                }
            }
        }
    })?;
    if other_profiles.is_empty() {
        wip_profile.lan_access = LanAccess::SameProfile;
    } else if (other_profiles.len() + 1) >= lookup.list().len() {
        wip_profile.lan_access = LanAccess::All;
    } else {
        wip_profile.lan_access = LanAccess::OtherProfiles(other_profiles);
    }
    wip_profile.dns_source = compute_dns_source(cfgs, &wip_profile);
    Ok(wip_profile)
}

pub fn delete<C: CtrlContext>(ctx: C, id: ProfileIdOpt) -> Result<(), Error> {
    let name = id.fullname.clone().unwrap_or_default();

    // Resolve the profile interface name before the retry loop so we can
    // use it for ifdown after dump_all succeeds (delete_config consumes the query).
    let interface_name = {
        let arena = Arena::new();
        let cfgs = parse_all(ctx.uci_root(), &arena, &["startwrt"])?;
        let lookup = Lookup::parse(ctx.clone(), &cfgs)?;
        lookup.resolve(&id)?.interface.clone()
    };
    let wg_interface_name = format!("wg_{}", interface_name);

    let mut retries = 4;
    loop {
        let arena = Arena::new();
        let mut cfgs = parse_all(
            ctx.uci_root(),
            &arena,
            &["startwrt", "network", "firewall", "dhcp", "wireless"],
        )?;
        if let Err(e) = delete_config(ctx.clone(), &mut cfgs, &id) {
            crate::activity::log("profile", "deleted", false, &format!("Failed to delete profile '{name}'"), Some(&e.to_string()));
            return Err(e);
        }
        match dump_all(ctx.uci_root(), cfgs) {
            Err(uciedit::Error::Conflict { .. }) if retries > 0 => {
                retries -= 1;
                continue;
            }
            Err(err) => {
                crate::activity::log("profile", "deleted", false, &format!("Failed to delete profile '{name}'"), Some(&err.to_string()));
                return Err(err.into());
            }
            Ok(()) => {
                if ctx.effectful() {
                    // Regenerate SmartDNS config after UCI dump so the deleted
                    // profile's group is removed.
                    let arena2 = Arena::new();
                    let cfgs2 = parse_all(ctx.uci_root(), &arena2, &["startwrt"])?;
                    dns::regenerate_smartdns(&ctx, &cfgs2)?;

                    // Bring down the WireGuard interface before reloading services.
                    // reload_system() alone won't tear down an active WireGuard tunnel.
                    let _ = crate::run_quiet(Command::new("ifdown").arg(&wg_interface_name));
                    reload_system_and_wifi()?;
                }
                // Regenerate schedule crontab to remove deleted profile's entries
                if let Err(e) = regenerate_schedule_crontab(&ctx) {
                    tracing::error!("Failed to regenerate schedule crontab after profile delete: {e}");
                }
                if ctx.effectful() {
                    let _ = crate::run_quiet(Command::new("/etc/init.d/cron").arg("restart"));
                }
                crate::activity::log("profile", "deleted", true, &format!("Deleted profile '{name}'"), None);
                return Ok(());
            }
        }
    }
}

fn delete_config(
    ctx: impl CtrlContext,
    cfgs: &mut Configs,
    query: &ProfileIdOpt,
) -> Result<(), Error> {
    let lookup = Lookup::parse(ctx, cfgs)?;
    let id = lookup.resolve(query)?.clone();

    // Prevent deleting the LAN owner profile
    if id.interface == "lan" {
        return Err(ErrorKind::CannotDeleteLanOwner.into());
    }

    // Remove profile and its VPN server metadata from startwrt config
    let wg_interface_name = format!("wg_{}", id.interface);
    cfgs["startwrt"].sections.retain(|section| {
        if let Ok(profile) = section.get::<UciProfile>() {
            if profile.id() == id {
                return false;
            }
        }
        // Remove VPN server metadata for this profile's WireGuard interface
        if section.ty() == "vpn_server" {
            if section.lines.iter().any(|line| {
                matches!(line, uciedit::Line::Option { option, value, .. }
                    if option.as_str() == "interface" && value.as_str() == wg_interface_name.as_str())
            }) {
                return false;
            }
        }
        true
    });

    // Remove network interface, bridge-vlan, and WireGuard interface + peers
    let wg_peer_type = format!("wireguard_{}", wg_interface_name);
    cfgs["network"].sections.retain(|section| {
        if section.get::<NetworkInterface>().is_ok() {
            if section.name().as_deref() == Some(&id.interface) {
                return false;
            }
        }
        // Remove WireGuard interface for this profile (checked separately because
        // WireGuard interfaces lack a `device` field so NetworkInterface parsing fails)
        if section.ty() == "interface"
            && section.name().as_deref() == Some(wg_interface_name.as_str())
        {
            return false;
        }
        if let Ok(vlan) = section.get::<NetworkBridgeVlan>() {
            if vlan.vlan == id.vlan_tag {
                return false;
            }
        }
        // Remove WireGuard peer sections for this profile's VPN server
        if section.ty() == wg_peer_type {
            return false;
        }
        true
    });

    // Find the firewall zone name for this profile's interface
    let mut zone_name = None;
    for section in &cfgs["firewall"].sections {
        let Ok(zone) = section.get::<FirewallZone>() else {
            continue;
        };
        if zone.network.iter().any(|n| n == &id.interface) {
            zone_name = Some(zone.name.clone());
            break;
        }
    }

    // Remove zone, its DHCP/DNS rule, and all forwarding rules from/to this zone
    if let Some(zone_name) = &zone_name {
        cfgs["firewall"].sections.retain(|section| {
            if let Ok(zone) = section.get::<FirewallZone>() {
                if &zone.name == zone_name {
                    return false;
                }
            }
            if let Ok(rule) = section.get::<FirewallRule>() {
                if &rule.src == zone_name {
                    return false;
                }
            }
            if let Ok(fwd) = section.get::<FirewallForwarding>() {
                if &fwd.src == zone_name || &fwd.dest == zone_name {
                    return false;
                }
            }
            if let Ok(redir) = section.get::<FirewallRedirect>() {
                if &redir.src == zone_name {
                    return false;
                }
            }
            true
        });
    }

    // Remove DHCP server for this interface
    cfgs["dhcp"].sections.retain(|section| {
        let Ok(dhcp) = section.get::<Dhcp>() else {
            return true;
        };
        dhcp.interface != id.interface
    });

    // Remove per-profile dnsmasq section
    let dns_section_name = format!("dns_{}", id.interface);
    cfgs["dhcp"]
        .sections
        .retain(|s| s.name().as_deref() != Some(dns_section_name.as_str()));

    // Remove notinterface entry from main dnsmasq
    for section in &mut cfgs["dhcp"].sections {
        if section.ty() == "dnsmasq"
            && !section
                .name()
                .map(|n| n.starts_with("dns_"))
                .unwrap_or(false)
        {
            section.lines.retain(|line| {
                !matches!(line, Line::List { list, item, .. }
                    if list.as_str() == "notinterface"
                        && item.as_str() == id.interface.as_str())
            });
            break;
        }
    }

    // Remove WiFi stations and VLAN sections referencing this profile's VLAN tag
    cfgs["wireless"].sections.retain(|section| {
        if let Ok(station) = section.get::<WifiStation>() {
            if station.vid == Some(id.vlan_tag) {
                return false;
            }
        }
        if let Ok(vlan) = section.get::<WifiVlan>() {
            if vlan.vid == id.vlan_tag {
                return false;
            }
        }
        true
    });

    // Remove policy routing entries for this profile
    let route_name = format!("prt_{}", id.interface);
    let local_route_name = format!("plr_{}", id.interface);
    let rule_name = format!("prr_{}", id.interface);
    cfgs["network"].sections.retain(|s| {
        let n = s.name();
        n.as_deref() != Some(route_name.as_str())
            && n.as_deref() != Some(local_route_name.as_str())
            && n.as_deref() != Some(rule_name.as_str())
    });

    // Remove WireGuard WAN firewall rule (src: "wan", not caught by zone cleanup above)
    crate::vpn_server::remove_wireguard_firewall_rule(cfgs, &wg_interface_name)?;

    // Rebuild cross-subnet routes (the deleted profile's subnet is removed,
    // and remaining VPN profiles no longer need a route to it)
    sync_cross_subnet_routes(cfgs)?;

    // Clean up orphaned VPN interfaces from WAN zone
    cleanup_orphaned_wan_vpns(cfgs);

    Ok(())
}

pub fn list<C: CtrlContext>(ctx: C) -> Result<Vec<ProfileId>, Error> {
    let arena = Arena::new();
    let cfgs = parse_all(ctx.uci_root(), &arena, &["startwrt"])?;
    list_config(ctx, &cfgs)
}

pub(crate) fn list_config(_ctx: impl CtrlContext, cfgs: &Configs) -> Result<Vec<ProfileId>, Error> {
    let mut found = Vec::new();
    for section in &cfgs["startwrt"].sections {
        let Ok(UciProfile {
            fullname,
            interface,
            vlan_tag,
            ..
        }) = section.get()
        else {
            continue;
        };
        found.push(ProfileId {
            fullname,
            interface,
            vlan_tag,
        })
    }
    Ok(found)
}

pub fn reload_system() -> Result<(), Error> {
    let _ = crate::run_quiet(Command::new("/etc/init.d/network").arg("reload"));
    let _ = crate::run_quiet(Command::new("/etc/init.d/smartdns").arg("restart"));
    let _ = crate::run_quiet(Command::new("/etc/init.d/firewall").arg("restart"));
    let _ = crate::run_quiet(Command::new("/etc/init.d/dnsmasq").arg("restart"));
    // Re-apply WAN schedules — firewall restart flushes the iptables chain
    reapply_schedules_after_reload();
    Ok(())
}

/// Combined reload for operations that also need a WiFi restart (profile create/delete).
/// Runs everything sequentially in one thread to avoid race conditions — the `wifi` command
/// tears down and recreates wireless interfaces, which destabilizes the network if the
/// firewall reloads concurrently.
pub fn reload_system_and_wifi() -> Result<(), Error> {
    let _ = crate::run_quiet(Command::new("/etc/init.d/network").arg("reload"));
    let _ = crate::run_quiet(&mut Command::new("wifi"));
    let _ = crate::run_quiet(Command::new("/etc/init.d/smartdns").arg("restart"));
    let _ = crate::run_quiet(Command::new("/etc/init.d/firewall").arg("restart"));
    let _ = crate::run_quiet(Command::new("/etc/init.d/dnsmasq").arg("restart"));
    // Re-apply WAN schedules — firewall restart flushes the iptables chain
    reapply_schedules_after_reload();
    Ok(())
}

/// Helper for reload functions (which have no CtrlContext) to re-apply WAN schedules
/// after a firewall restart. The schedule rules are already in UCI, so this just
/// ensures the current time-based state is correct.
fn reapply_schedules_after_reload() {
    #[derive(Clone)]
    struct ReloadCtx;
    impl rpc_toolkit::Context for ReloadCtx {}
    impl CtrlContext for ReloadCtx {
        fn uci_root(&self) -> PathBuf {
            PathBuf::from("/etc/config/")
        }
        fn effectful(&self) -> bool {
            true
        }
    }
    if let Err(e) = evaluate_and_apply_schedules(&ReloadCtx) {
        tracing::error!("Failed to re-apply WAN schedules after firewall reload: {e}");
    }
}

/// When the first bridge-vlan is about to be created on a bridge, we must also create
/// a VLAN 1 entry with all existing bridge ports. Once VLAN filtering is active, any
/// port without an explicit VLAN entry loses all traffic.
fn ensure_vlan_filtering(cfgs: &mut Configs, bridge_name: &str) -> Result<(), Error> {
    let has_any = cfgs["network"]
        .sections
        .iter()
        .any(|s| {
            s.get::<NetworkBridgeVlan>()
                .map(|v| v.device == bridge_name)
                .unwrap_or(false)
        });
    if has_any {
        return Ok(());
    }

    // Get bridge ports
    let bridge_ports: Vec<String> = cfgs["network"]
        .sections
        .iter()
        .filter_map(|s| {
            s.get::<NetworkDevice>()
                .ok()
                .filter(|d| d.ty == Some(DeviceType::BRIDGE) && d.name == bridge_name)
                .map(|d| d.ports)
        })
        .next()
        .unwrap_or_default();

    // Create VLAN 1 with all bridge ports as PRIMARY (untagged + PVID)
    if !bridge_ports.is_empty() {
        cfgs["network"].append(
            &NetworkBridgeVlan {
                device: bridge_name.to_string(),
                vlan: 1,
                ports: bridge_ports
                    .into_iter()
                    .map(|port| NetworkVlanPort {
                        port,
                        tagging: Some(NetworkVlanPortTagging::PRIMARY),
                    })
                    .collect(),
            },
            None,
        )?;
    }

    // Update LAN interface from br-lan to br-lan.1
    for section in &mut cfgs["network"].sections {
        if let Ok(mut iface) = section.get::<NetworkInterface>() {
            if section.name().as_deref() == Some("lan") && iface.device == bridge_name {
                iface.device = format!("{bridge_name}.1");
                section.set(&iface)?;
            }
        }
    }
    Ok(())
}

pub fn set<C: CtrlContext>(
    ctx: C,
    DeserializeStdin(req): DeserializeStdin<ProfileSetRequest>,
) -> Result<ProfileId, Error> {
    let profile = req.profile;
    let force = req.force;
    let name = profile.id.fullname.clone().unwrap_or_default();
    let mut retries = 4;
    loop {
        let arena = Arena::new();
        let mut cfgs = parse_all(
            ctx.uci_root(),
            &arena,
            &["startwrt", "network", "firewall", "dhcp"],
        )?;

        // Capture old LAN IP before set_config modifies it
        let old_lan_ip = if profile.owns_lan {
            get_lan_ip(&cfgs)
        } else {
            None
        };

        let (out, old_state) = match set_config(ctx.clone(), &mut cfgs, &profile) {
            Ok(result) => result,
            Err(e) => {
                crate::activity::log("profile", "updated", false, &format!("Failed to update profile '{name}'"), Some(&e.to_string()));
                return Err(e);
            }
        };

        // Detect admin IP change and propagate block changes to sibling profiles
        let admin_ip_changed = profile.owns_lan
            && old_lan_ip.is_some()
            && old_lan_ip != Some(profile.gateway_ip);
        let block_changed = match (admin_ip_changed, old_lan_ip) {
            (true, Some(old)) => {
                old.octets()[0] != profile.gateway_ip.octets()[0]
                    || old.octets()[1] != profile.gateway_ip.octets()[1]
            }
            _ => false,
        };
        let restart_ifaces: Vec<String> = if block_changed {
            let pi: std::collections::HashSet<String> =
                list_config(ctx.clone(), &cfgs)?
                    .into_iter()
                    .filter(|p| p.interface != crate::lan::LAN_INTERFACE)
                    .map(|p| p.interface)
                    .collect();
            crate::lan::update_profile_ips_for_block_change(
                &mut cfgs,
                old_lan_ip.unwrap(),
                profile.gateway_ip,
                &pi,
            )?;
            let mut v = vec![crate::lan::LAN_INTERFACE.to_string()];
            v.extend(pi);
            v
        } else {
            vec![crate::lan::LAN_INTERFACE.to_string()]
        };

        // Guard: check if IP change would break VPN peers
        let ip_changed = old_state
            .as_ref()
            .and_then(|s| s.gateway_ip)
            .map(|old| old != profile.gateway_ip)
            .unwrap_or(false);
        let removed_vpns = if ip_changed || block_changed {
            let affected: Vec<&str> = if block_changed {
                restart_ifaces.iter().map(|s| s.as_str()).collect()
            } else if let Some(ref iface) = profile.id.interface {
                vec![iface.as_str()]
            } else {
                vec![]
            };
            crate::vpn_server::guard_vpn_peers(&mut cfgs, &affected, force)?
        } else {
            Default::default()
        };

        match dump_all(ctx.uci_root(), cfgs) {
            Err(uciedit::Error::Conflict { .. }) if retries > 0 => {
                retries -= 1;
                continue;
            }
            Err(err) => {
                crate::activity::log("profile", "updated", false, &format!("Failed to update profile '{name}'"), Some(&err.to_string()));
                return Err(err.into());
            }
            Ok(()) => {
                if ctx.effectful() {
                    // Regenerate SmartDNS config after UCI dump so it reflects
                    // any dns_override changes on this profile.
                    let arena2 = Arena::new();
                    let cfgs2 = parse_all(ctx.uci_root(), &arena2, &["startwrt"])?;
                    dns::regenerate_smartdns(&ctx, &cfgs2)?;

                    if admin_ip_changed {
                        crate::lan::restart_network_services(profile.gateway_ip, restart_ifaces);
                    } else {
                        reload_system()?;
                    }
                    removed_vpns.apply_post_reload();
                }
                let mut changes = Vec::new();
                if let Some(ref old) = old_state {
                    if old.outbound.as_deref() != Some(&profile.outbound) {
                        changes.push(format!("outbound: {}", profile.outbound));
                    }
                    let new_wan = wan_access_type_str(&profile.wan_access);
                    if old.wan_access.as_deref() != Some(&new_wan) {
                        changes.push(format!("WAN access: {}", new_wan));
                    }
                    if old.access_to_new_profiles != profile.access_to_new_profiles {
                        changes.push(format!(
                            "access to new profiles: {}",
                            profile.access_to_new_profiles
                        ));
                    }
                    if old.gateway_ip.is_some() && old.gateway_ip != Some(profile.gateway_ip) {
                        changes.push(format!("gateway: {}", profile.gateway_ip));
                    }
                    if old.dns_override
                        != dns::serialize_dns_server_list(&profile.dns_override)
                    {
                        changes.push("DNS".to_string());
                    }
                }
                let summary = if changes.is_empty() {
                    format!("Updated profile '{name}'")
                } else {
                    format!("Updated profile '{name}' — {}", changes.join(", "))
                };
                crate::activity::log("profile", "updated", true, &summary, None);
                return Ok(out);
            }
        }
    }
}

/// Read the current LAN interface IP from parsed configs.
fn get_lan_ip(cfgs: &uciedit::Configs) -> Option<Ipv4Addr> {
    for section in &cfgs["network"].sections {
        if section.name().as_deref() == Some(crate::lan::LAN_INTERFACE) {
            if let Ok(Some(iface)) = section.get_typed::<NetworkInterface>() {
                return iface.ipaddr;
            }
        }
    }
    None
}

fn set_config<C: CtrlContext>(
    ctx: C,
    cfgs: &mut Configs,
    profile: &Profile<ProfileIdOpt>,
) -> Result<(ProfileId, Option<OldProfileState>), Error> {
    let ipv6 = is_ipv6_enabled(cfgs)
        && outbound_supports_ipv6(cfgs, &profile.outbound);
    // Check fullname uniqueness before renaming
    if let Some(given_fullname) = &profile.id.fullname {
        let pre_lookup = Lookup::parse(ctx.clone(), cfgs)?;
        if let Some(existing) = pre_lookup.from_fullname(given_fullname) {
            // Only error if the name belongs to a different profile
            if !profile.id.matches(&existing.clone().into()) {
                return Err(ErrorKind::DuplicateFullname {
                    name: given_fullname.clone(),
                }
                .into());
            }
        }
    }
    let mut old_state: Option<OldProfileState> = None;
    for section in &mut cfgs["startwrt"].sections {
        if let Some(mut existing_profile) = section.get_typed::<UciProfile>()? {
            if let Some(given_fullname) = &profile.id.fullname {
                if &existing_profile.fullname != given_fullname
                    && Some(&existing_profile.vlan_tag) == profile.id.vlan_tag.as_ref()
                    && Some(&existing_profile.interface) == profile.id.interface.as_ref()
                {
                    // rename
                    existing_profile.fullname = given_fullname.clone();
                }
            }
            if profile.id.matches(&existing_profile.id().into()) {
                old_state = Some(OldProfileState {
                    outbound: existing_profile.outbound.clone(),
                    wan_access: existing_profile.wan_access.clone(),
                    access_to_new_profiles: existing_profile.access_to_new_profiles,
                    dns_override: existing_profile.dns_override.clone(),
                    gateway_ip: None, // filled from network config below
                });
                existing_profile.access_to_new_profiles = profile.access_to_new_profiles;
                existing_profile.outbound = Some(profile.outbound.clone());
                existing_profile.wan_access = Some(wan_access_type_str(&profile.wan_access));
                existing_profile.wan_access_list = wan_access_destinations(&profile.wan_access);
                existing_profile.dns_override =
                    dns::serialize_dns_server_list(&profile.dns_override);
                section.set(&existing_profile)?;
            }
        }
    }
    // only do lookup after potentially handling renames
    let lookup = Lookup::parse(ctx.clone(), &cfgs)?;
    let profile = Profile {
        id: lookup.resolve(&profile.id)?.clone(),
        gateway_ip: profile.gateway_ip,
        outbound: profile.outbound.clone(),
        lan_access: match &profile.lan_access {
            LanAccess::All => LanAccess::All,
            LanAccess::SameProfile => LanAccess::SameProfile,
            LanAccess::OtherProfiles(set) => LanAccess::OtherProfiles(
                set.into_iter()
                    .map(|p| lookup.resolve(&p).cloned())
                    .collect::<Result<_, _>>()?,
            ),
        },
        wan_access: profile.wan_access.clone(),
        dns_override: profile.dns_override.clone(),
        dns_source: String::new(),
        access_to_new_profiles: profile.access_to_new_profiles,
        owns_lan: profile.owns_lan,
    };
    let found_bridge = find_lan_bridge(cfgs)?.map(|dev| dev.name);
    let mut all_interfaces = BTreeSet::<String>::new();
    let mut found_interface = false;
    for section in &mut cfgs["network"].sections {
        if let Some(mut iface) = section.get_typed::<NetworkInterface>()? {
            let Some(name) = section.name() else { continue };
            if iface.proto == InterfaceProto::STATIC {
                all_interfaces.insert(name.to_string());
            }
            if name == profile.id.interface {
                if let Some(ref mut old) = old_state {
                    old.gateway_ip = iface.ipaddr;
                }
                iface.proto = InterfaceProto::STATIC;
                iface.ipaddr = Some(profile.gateway_ip);
                iface.netmask = Some(Ipv4Addr::new(255, 255, 255, 0));
                iface.ip6assign = if ipv6 { Some("64".into()) } else { None };
                section.set(&iface)?;
                found_interface = true;
            }
        }
    }
    if !found_interface {
        let found_bridge = found_bridge.clone().ok_or(ErrorKind::MissingLanBridge)?;
        cfgs["network"].append(
            &NetworkInterface {
                device: format!("{}.{}", found_bridge, profile.id.vlan_tag),
                proto: InterfaceProto::STATIC,
                ipaddr: Some(profile.gateway_ip),
                netmask: Some(Ipv4Addr::new(255, 255, 255, 0)),
                ip6assign: if ipv6 { Some("64".into()) } else { None },
                ..Default::default()
            },
            Some(&profile.id.interface),
        )?;
    }
    // Ensure a portless bridge-vlan exists for this profile's VLAN
    if !profile.owns_lan {
        let bridge_name = found_bridge.as_ref().ok_or(ErrorKind::MissingLanBridge)?;
        // Transition to VLAN filtering: ensure VLAN 1 exists for existing bridge ports
        ensure_vlan_filtering(cfgs, bridge_name)?;
        let has_bridge_vlan = cfgs["network"]
            .sections
            .iter()
            .any(|s| {
                s.get::<NetworkBridgeVlan>()
                    .map(|v| v.vlan == profile.id.vlan_tag && v.device == *bridge_name)
                    .unwrap_or(false)
            });
        if !has_bridge_vlan {
            cfgs["network"].append(
                &NetworkBridgeVlan {
                    device: bridge_name.clone(),
                    vlan: profile.id.vlan_tag,
                    ports: Vec::new(),
                },
                None,
            )?;
        }
    }
    rewrite_firewall(&ctx, cfgs, &profile, &all_interfaces, &[], false)?;
    rewrite_dhcp(&ctx, cfgs, &profile)?;
    rewrite_dns_forwarding(cfgs, &profile)?;
    rewrite_routing(&ctx, cfgs, &profile)?;
    sync_cross_subnet_routes(cfgs)?;
    cleanup_orphaned_wan_vpns(cfgs);
    Ok((profile.id, old_state))
}

pub fn create<C: CtrlContext>(
    ctx: C,
    DeserializeStdin(profile): DeserializeStdin<Profile<ProfileIdOpt>>,
) -> Result<ProfileId, Error> {
    let name = profile.id.fullname.clone().unwrap_or_default();
    let mut retries = 4;
    loop {
        let arena = Arena::new();
        let mut cfgs = parse_all(
            ctx.uci_root(),
            &arena,
            &["startwrt", "network", "firewall", "dhcp", "wireless"],
        )?;
        let out = match create_config(ctx.clone(), &mut cfgs, &profile) {
            Ok(out) => out,
            Err(e) => {
                crate::activity::log("profile", "created", false, &format!("Failed to create profile '{name}'"), Some(&e.to_string()));
                return Err(e);
            }
        };
        match dump_all(ctx.uci_root(), cfgs) {
            Err(uciedit::Error::Conflict { .. }) if retries > 0 => {
                retries -= 1;
                continue;
            }
            Err(err) => {
                crate::activity::log("profile", "created", false, &format!("Failed to create profile '{name}'"), Some(&err.to_string()));
                return Err(err.into());
            }
            Ok(()) => {
                if ctx.effectful() {
                    // Regenerate SmartDNS config after UCI dump so it reflects
                    // any dns_override on the new profile.
                    let arena2 = Arena::new();
                    let cfgs2 = parse_all(ctx.uci_root(), &arena2, &["startwrt"])?;
                    dns::regenerate_smartdns(&ctx, &cfgs2)?;

                    reload_system_and_wifi()?;
                }
                crate::activity::log("profile", "created", true, &format!("Created profile '{name}'"), None);
                return Ok(out);
            }
        }
    }
}

fn create_config(
    ctx: impl CtrlContext,
    cfgs: &mut Configs,
    profile: &Profile<ProfileIdOpt>,
) -> Result<ProfileId, Error> {
    let ipv6 = is_ipv6_enabled(cfgs)
        && outbound_supports_ipv6(cfgs, &profile.outbound);
    let interface = if profile.owns_lan {
        if Lookup::parse(ctx.clone(), cfgs)?.lan_owner.is_some() {
            return Err(ErrorKind::LanOwnerExists.into());
        }
        "lan".into()
    } else {
        allocate_interface_name(
            &ctx,
            profile
                .id
                .interface
                .as_deref()
                .or(profile.id.fullname.as_deref()),
        )?
    };
    let fullname = profile
        .id
        .fullname
        .clone()
        .unwrap_or_else(|| "Untitled".into());
    // Check fullname uniqueness
    {
        let lookup = Lookup::parse(ctx.clone(), cfgs)?;
        if lookup.from_fullname(&fullname).is_some() {
            return Err(ErrorKind::DuplicateFullname {
                name: fullname,
            }
            .into());
        }
    }
    let mut all_interfaces = BTreeSet::<String>::new();
    let mut existing_tags = BTreeSet::new();
    for section in &cfgs["network"].sections {
        match &*section.ty() {
            NetworkInterface::TY => {
                let Some(name) = section.name() else { continue };
                if name == interface && !profile.owns_lan {
                    return Err(ErrorKind::InterfaceNameConflict {
                        name: interface.clone(),
                    }
                    .into());
                }
                if let Ok(iface) = section.get::<NetworkInterface>() {
                    if iface.proto == InterfaceProto::STATIC {
                        all_interfaces.insert(name.to_string());
                    }
                }
            }
            NetworkBridgeVlan::TY => {
                if let Ok(vlan) = section.get::<NetworkBridgeVlan>() {
                    existing_tags.insert(vlan.vlan);
                }
            }
            _ => (),
        }
    }
    let found_bridge = find_lan_bridge(cfgs)?
        .map(|dev| dev.name)
        .ok_or(Error::from(ErrorKind::MissingLanBridge))?;
    let vlan_tag = match profile.id.vlan_tag {
        Some(chosen_tag) => {
            if existing_tags.contains(&chosen_tag) {
                return Err(ErrorKind::DuplicateVlanTag { tag: chosen_tag }.into());
            }
            chosen_tag
        }
        None => (101..2_u16.pow(12))
            .find(|t| !existing_tags.contains(t))
            .expect("we somehow exausted all vlan tags, that should be impossible"),
    };
    if profile.owns_lan {
        let mut found_lan_interface = false;
        for section in &mut cfgs["network"].sections {
            if let Ok(mut iface) = section.get::<NetworkInterface>() {
                if section.name().as_deref() != Some("lan") {
                    continue;
                }
                found_lan_interface = true;
                iface.device = format!("{found_bridge}.{vlan_tag}");
                section.set(&iface)?;
            }
        }
        if !found_lan_interface {
            return Err(ErrorKind::MissingLanInterface.into());
        }
        // Create bridge-vlan with all bridge ports so ethernet devices reach the LAN owner
        let bridge_ports: Vec<String> = cfgs["network"]
            .sections
            .iter()
            .filter_map(|s| {
                s.get::<NetworkDevice>()
                    .ok()
                    .filter(|d| d.ty == Some(DeviceType::BRIDGE) && d.name == found_bridge)
                    .map(|d| d.ports)
            })
            .next()
            .unwrap_or_default();
        cfgs["network"].append(
            &NetworkBridgeVlan {
                device: found_bridge.clone(),
                vlan: vlan_tag,
                ports: bridge_ports
                    .into_iter()
                    .map(|port| NetworkVlanPort {
                        port,
                        tagging: Some(NetworkVlanPortTagging::PRIMARY),
                    })
                    .collect(),
            },
            None,
        )?;
    } else {
        cfgs["network"].append(
            &NetworkInterface {
                device: format!("{found_bridge}.{vlan_tag}"),
                proto: InterfaceProto::STATIC,
                ipaddr: Some(profile.gateway_ip),
                netmask: Some(Ipv4Addr::new(255, 255, 255, 0)),
                ip6assign: if ipv6 { Some("64".into()) } else { None },
                ..Default::default()
            },
            Some(&interface),
        )?;
        // Transition to VLAN filtering: ensure VLAN 1 exists for existing bridge ports
        ensure_vlan_filtering(cfgs, &found_bridge)?;
        // Create a portless bridge-vlan so br-lan.<vlan> can exist
        cfgs["network"].append(
            &NetworkBridgeVlan {
                device: found_bridge.clone(),
                vlan: vlan_tag,
                ports: Vec::new(),
            },
            None,
        )?;
        // Create wifi-vlan sections so hostapd knows about this VLAN at boot
        for iface_name in crate::wifi::find_ap_interface_names(cfgs)? {
            cfgs["wireless"].append(
                &WifiVlan {
                    name: interface.clone(),
                    network: interface.clone(),
                    vid: vlan_tag,
                    iface: Some(iface_name),
                },
                None,
            )?;
        }
    }
    let mut wants_access = Vec::new();
    for section in &cfgs["startwrt"].sections {
        if let Some(other_profile) = section.get_typed::<UciProfile>()? {
            if other_profile.access_to_new_profiles {
                wants_access.push(other_profile.id());
            }
        }
    }
    cfgs["startwrt"].append(
        &UciProfile {
            fullname: fullname.clone(),
            interface: interface.clone(),
            vlan_tag,
            outbound: Some(profile.outbound.clone()),
            access_to_new_profiles: profile.access_to_new_profiles,
            wan_access: Some(wan_access_type_str(&profile.wan_access)),
            wan_access_list: wan_access_destinations(&profile.wan_access),
            dns_override: dns::serialize_dns_server_list(&profile.dns_override),
            wan_schedule: Vec::new(),
        },
        Some(&interface),
    )?;
    let lookup = Lookup::parse(ctx.clone(), &cfgs)?; // only lookup after pushing, in case there is a self-reference
    let profile = Profile {
        id: ProfileId {
            fullname,
            interface,
            vlan_tag,
        },
        gateway_ip: profile.gateway_ip,
        outbound: profile.outbound.clone(),
        lan_access: match &profile.lan_access {
            LanAccess::All => LanAccess::All,
            LanAccess::SameProfile => LanAccess::SameProfile,
            LanAccess::OtherProfiles(set) => LanAccess::OtherProfiles(
                set.into_iter()
                    .map(|p| lookup.resolve(&p).cloned())
                    .collect::<Result<_, _>>()?,
            ),
        },
        wan_access: profile.wan_access.clone(),
        dns_override: profile.dns_override.clone(),
        dns_source: String::new(),
        access_to_new_profiles: profile.access_to_new_profiles,
        owns_lan: profile.owns_lan,
    };
    rewrite_firewall(&ctx, cfgs, &profile, &all_interfaces, &wants_access, true)?;
    rewrite_dhcp(&ctx, cfgs, &profile)?;
    rewrite_dns_forwarding(cfgs, &profile)?;
    rewrite_routing(&ctx, cfgs, &profile)?;
    sync_cross_subnet_routes(cfgs)?;
    cleanup_orphaned_wan_vpns(cfgs);
    Ok(profile.id)
}

fn rewrite_firewall(
    _ctx: &impl CtrlContext,
    cfgs: &mut Configs,
    profile: &Profile,
    all_interfaces: &BTreeSet<String>,
    wants_access: &[ProfileId],
    remake_zone: bool,
) -> Result<(), Error> {
    // Make sure the required zones exist
    let mut found_wan = false;
    let mut this_zone_name = format!("vlan_{}", profile.id.interface);
    let mut all_zones = BTreeMap::new();
    let mut existing_zone_index = None;
    for (index, section) in cfgs["firewall"].sections.iter().enumerate() {
        let Ok(zone) = section.get::<FirewallZone>() else {
            continue;
        };
        if zone.name == DEFAULT_WAN_ZONE {
            found_wan = true;
        } else if zone.name == this_zone_name {
            existing_zone_index = Some(index);
        } else {
            for zone_interface in zone.network {
                if profile.id.interface == zone_interface {
                    this_zone_name = zone.name.clone();
                    existing_zone_index = Some(index);
                }
                if all_interfaces.contains(&zone_interface) {
                    all_zones.insert(zone_interface, zone.name.clone());
                }
            }
        }
    }
    if !found_wan {
        return Err(ErrorKind::MissingWanInterface.into());
    }
    if remake_zone || existing_zone_index.is_none() {
        if let Some(index) = existing_zone_index {
            let removed = cfgs["firewall"].sections.remove(index);
            assert!(removed.ty() == FirewallZone::TY);
        }
        cfgs["firewall"].append(
            &FirewallZone {
                name: this_zone_name.clone(),
                input: FirewallTarget::ACCEPT,
                output: FirewallTarget::ACCEPT,
                forward: FirewallTarget::ACCEPT,
                network: vec![profile.id.interface.clone()],
            },
            None,
        )?;
    }

    // Setup forwarding for DNS and DHCP
    let mut found_dhcp_dns_rule = false;
    cfgs["firewall"].sections.retain(|section| {
        let Ok(rule) = section.get::<FirewallRule>() else {
            return true;
        };
        if rule.src == this_zone_name && rule.name.contains("DHCP") && rule.name.contains("DNS") {
            if remake_zone {
                return false;
            } else {
                found_dhcp_dns_rule = true;
            }
        }
        true
    });
    if !found_dhcp_dns_rule {
        cfgs["firewall"].append(
            &FirewallRule {
                name: format!("Allow-DHCP-DNS-{}", profile.id.fullname.replace(" ", "-")),
                src: this_zone_name.clone(),
                dest_port: Some("53 67 68".into()),
                proto: vec!["tcp".into(), "udp".into(), "icmp".into()],
                target: FirewallTarget::ACCEPT,
                ..Default::default()
            },
            None,
        )?;
    }

    // Clean up old DNS override redirects for this profile
    cfgs["firewall"].sections.retain(|section| {
        let Ok(redir) = section.get::<FirewallRedirect>() else {
            return true;
        };
        !(redir.src == this_zone_name && redir.name.contains("DNS-Override"))
    });

    // DNS hijacking: redirect all port 53 traffic to the profile's gateway,
    // where per-profile dnsmasq handles .lan locally and forwards the rest
    // via SmartDNS (for custom/system DNS) or directly (for VPN DNS).
    if has_effective_dns(cfgs, profile) || !dns::get_system_dns_servers(cfgs).is_empty() {
        cfgs["firewall"].append(
            &FirewallRedirect {
                name: format!(
                    "DNS-Override-{}",
                    profile.id.fullname.replace(" ", "-"),
                ),
                src: this_zone_name.clone(),
                proto: vec!["tcp".into(), "udp".into()],
                src_dport: Some("53".into()),
                dest_ip: Some(profile.gateway_ip.to_string()),
                dest_port: Some("53".into()),
                target: "DNAT".into(),
                ..Default::default()
            },
            None,
        )?;
    }

    // Clean up old WAN access rules for this profile
    cfgs["firewall"].sections.retain(|section| {
        let Ok(rule) = section.get::<FirewallRule>() else {
            return true;
        };
        !(rule.src == this_zone_name
            && rule.dest.as_deref() == Some(DEFAULT_WAN_ZONE)
            && !rule.name.contains("DHCP"))
    });

    // Setup forwarding for lan access
    cfgs["firewall"].sections.retain(|section| {
        let Ok(fwd) = section.get::<FirewallForwarding>() else {
            return true;
        };
        fwd.src != this_zone_name
    });
    match &profile.lan_access {
        LanAccess::All => {
            for other_zone in all_zones.values() {
                if other_zone != &this_zone_name {
                    cfgs["firewall"].append(
                        &FirewallForwarding {
                            src: this_zone_name.clone(),
                            dest: other_zone.clone(),
                        },
                        None,
                    )?;
                }
            }
        }
        LanAccess::SameProfile => (),
        LanAccess::OtherProfiles(profile_ids) => {
            for other_profile in profile_ids {
                match all_zones.get(&other_profile.interface) {
                    Some(other_zone) => cfgs["firewall"].append(
                        &FirewallForwarding {
                            src: this_zone_name.clone(),
                            dest: other_zone.clone(),
                        },
                        None,
                    )?,
                    None => {
                        return Err(ErrorKind::MissingFirewallZone {
                            interface: other_profile.interface.clone(),
                        }
                        .into());
                    }
                }
            }
        }
    }
    for other_profile in wants_access {
        match all_zones.get(&other_profile.interface) {
            Some(other_zone) => cfgs["firewall"].append(
                &FirewallForwarding {
                    src: other_zone.clone(),
                    dest: this_zone_name.clone(),
                },
                None,
            )?,
            None => {
                return Err(ErrorKind::MissingFirewallZone {
                    interface: other_profile.interface.clone(),
                }
                .into());
            }
        }
    }

    // Setup forwarding for wan access
    match &profile.wan_access {
        WanAccess::All => cfgs["firewall"].append(
            &FirewallForwarding {
                src: this_zone_name.clone(),
                dest: DEFAULT_WAN_ZONE.into(),
            },
            None,
        )?,
        WanAccess::None => (),
        WanAccess::Whitelist(destinations) => {
            // Forwarding needed so rule matching works in the forward chain
            cfgs["firewall"].append(
                &FirewallForwarding {
                    src: this_zone_name.clone(),
                    dest: DEFAULT_WAN_ZONE.into(),
                },
                None,
            )?;
            // ACCEPT rules for each allowed destination
            for dest_ip in destinations {
                cfgs["firewall"].append(
                    &FirewallRule {
                        name: format!(
                            "WAN-WL-{}-{}",
                            profile.id.fullname.replace(" ", "-"),
                            dest_ip
                        ),
                        src: this_zone_name.clone(),
                        dest: Some(DEFAULT_WAN_ZONE.into()),
                        dest_ip: Some(dest_ip.clone()),
                        proto: vec!["all".into()],
                        target: FirewallTarget::ACCEPT,
                        ..Default::default()
                    },
                    None,
                )?;
            }
            // Catch-all REJECT for everything else
            cfgs["firewall"].append(
                &FirewallRule {
                    name: format!(
                        "WAN-WL-{}-reject",
                        profile.id.fullname.replace(" ", "-")
                    ),
                    src: this_zone_name.clone(),
                    dest: Some(DEFAULT_WAN_ZONE.into()),
                    proto: vec!["all".into()],
                    target: FirewallTarget::REJECT,
                    ..Default::default()
                },
                None,
            )?;
        }
        WanAccess::Blacklist(destinations) => {
            // Default allow via forwarding
            cfgs["firewall"].append(
                &FirewallForwarding {
                    src: this_zone_name.clone(),
                    dest: DEFAULT_WAN_ZONE.into(),
                },
                None,
            )?;
            // REJECT rules for each blocked destination
            for dest_ip in destinations {
                cfgs["firewall"].append(
                    &FirewallRule {
                        name: format!(
                            "WAN-BL-{}-{}",
                            profile.id.fullname.replace(" ", "-"),
                            dest_ip
                        ),
                        src: this_zone_name.clone(),
                        dest: Some(DEFAULT_WAN_ZONE.into()),
                        dest_ip: Some(dest_ip.clone()),
                        proto: vec!["all".into()],
                        target: FirewallTarget::REJECT,
                        ..Default::default()
                    },
                    None,
                )?;
            }
        }
    }

    Ok(())
}

pub(crate) fn is_ipv6_enabled(cfgs: &Configs) -> bool {
    cfgs["dhcp"].sections.iter().any(|s| {
        s.name().as_deref() == Some("lan")
            && s.get_typed::<Dhcp>()
                .ok()
                .flatten()
                .and_then(|d| d.ra)
                .map(|ra| ra == "server")
                .unwrap_or(false)
    })
}

/// Check whether an outbound VPN interface has IPv6 addresses configured.
/// If the outbound is "wan" or the VPN has at least one IPv6 address, returns true.
/// If the VPN has no IPv6 addresses, returns false (IPv6 would leak outside the tunnel).
pub(crate) fn outbound_supports_ipv6(cfgs: &Configs, outbound: &str) -> bool {
    use crate::vpn_server::WgInterface;
    if outbound == "wan" {
        return true;
    }
    cfgs["network"]
        .sections
        .iter()
        .find(|s| s.name().as_deref() == Some(outbound))
        .and_then(|s| s.get::<WgInterface>().ok())
        .filter(|wg| wg.is_wireguard())
        .map(|wg| wg.addresses.iter().any(|addr| addr.contains(':')))
        .unwrap_or(true) // if we can't find the interface, don't restrict
}

pub fn rewrite_dhcp(
    _ctx: &impl CtrlContext,
    cfgs: &mut Configs,
    profile: &Profile,
) -> Result<(), Error> {
    let ipv6 = is_ipv6_enabled(cfgs)
        && outbound_supports_ipv6(cfgs, &profile.outbound);
    let ra_value = if ipv6 { "server" } else { "disabled" }.to_string();
    let dhcpv6_value = if ipv6 { "server" } else { "disabled" }.to_string();

    let mut found_dhcp = false;
    for section in &mut cfgs["dhcp"].sections {
        let Ok(mut dhcp) = section.get::<Dhcp>() else {
            continue;
        };
        if dhcp.interface == profile.id.interface {
            let mut changed = false;
            if dhcp.ra.as_deref() != Some(&ra_value) || dhcp.dhcpv6.as_deref() != Some(&dhcpv6_value) {
                dhcp.ra = Some(ra_value.clone());
                dhcp.dhcpv6 = Some(dhcpv6_value.clone());
                changed = true;
            }
            // Enforce DHCP pool bounds (avoid overlap with VPN peer range .200-.253)
            if dhcp.start != 2 || dhcp.limit != 198 {
                dhcp.start = 2;
                dhcp.limit = 198;
                changed = true;
            }
            if changed {
                section.set(&dhcp)?;
            }
            found_dhcp = true;
        }
    }
    if !found_dhcp {
        cfgs["dhcp"].append(
            &Dhcp {
                interface: profile.id.interface.clone(),
                start: 2,
                limit: 198,
                leasetime: "12h".into(),
                ra: Some(ra_value),
                dhcpv6: Some(dhcpv6_value),
                ra_management: None,
            },
            Some(&profile.id.interface),
        )?;
    }
    Ok(())
}

pub(crate) fn rewrite_routing(
    _ctx: &impl CtrlContext,
    cfgs: &mut Configs,
    profile: &Profile,
) -> Result<(), Error> {
    let route_name = format!("prt_{}", profile.id.interface);
    let local_route_name = format!("plr_{}", profile.id.interface);
    let rule_name = format!("prr_{}", profile.id.interface);
    let mark_rule_name = format!("dnat_mark_{}", profile.id.interface);

    // 1. Remove old route/rule/mark for this profile
    cfgs["network"].sections.retain(|s| {
        let n = s.name();
        n.as_deref() != Some(route_name.as_str())
            && n.as_deref() != Some(local_route_name.as_str())
            && n.as_deref() != Some(rule_name.as_str())
    });
    cfgs["firewall"]
        .sections
        .retain(|s| s.name().as_deref() != Some(mark_rule_name.as_str()));

    // 2. If outbound is "wan", main table suffices — no policy routing needed
    if profile.outbound == "wan" {
        // Clean up any stale VPN peer routes from when this profile used a VPN outbound
        crate::vpn_server::sync_peer_policy_routes(cfgs, &profile.id.interface)?;
        return Ok(());
    }

    // 3. Create routing table entry:
    //    config route 'prt_<interface>'
    //      option interface '<outbound>'
    //      option target '0.0.0.0/0'
    //      option table '<vlan_tag>'
    cfgs["network"].append(
        &NetworkRoute {
            interface: profile.outbound.clone(),
            target: "0.0.0.0/0".to_string(),
            gateway: None,
            netmask: None,
            table: Some(profile.id.vlan_tag as u32),
        },
        Some(&route_name),
    )?;

    // 4. Create connected route for the profile's local subnet so LAN traffic
    //    stays local instead of being sent through the VPN tunnel:
    //    config route 'plr_<interface>'
    //      option interface '<interface>'
    //      option target '<network>/24'
    //      option table '<vlan_tag>'
    let octets = profile.gateway_ip.octets();
    let network_addr = format!("{}.{}.{}.0/24", octets[0], octets[1], octets[2]);
    cfgs["network"].append(
        &NetworkRoute {
            interface: profile.id.interface.clone(),
            target: network_addr.clone(),
            table: Some(profile.id.vlan_tag as u32),
            ..Default::default()
        },
        Some(&local_route_name),
    )?;

    // 5. Create ip rule:
    //    config rule 'prr_<interface>'
    //      option src '<network_addr>/24'
    //      option lookup '<vlan_tag>'
    //      option priority '200'
    cfgs["network"].append(
        &NetworkRule {
            src: Some(network_addr),
            lookup: profile.id.vlan_tag as u32,
            priority: Some(VPN_ROUTING_PRIORITY),
            ..Default::default()
        },
        Some(&rule_name),
    )?;

    // 6. Create mangle MARK rule so that DNAT reply traffic gets routed via
    //    the main table instead of the VPN tunnel.  fw3 places MARK rules
    //    with src=<zone> + dest='*' into mangle PREROUTING (rules.c:313).
    //    The matching ip rule (dnat_return) is created by ensure_dnat_return_rule().
    let zone_name = resolve_profile_zone(cfgs, &profile.id.interface);
    cfgs["firewall"].append(
        &FirewallRule {
            name: "Mark-DNAT-Return".to_string(),
            src: zone_name,
            dest: Some("*".to_string()),
            proto: vec!["all".into()],
            target: FirewallTarget::MARK,
            set_mark: Some(DNAT_RETURN_MARK.to_string()),
            extra: Some("-m conntrack --ctstate DNAT".to_string()),
            ..Default::default()
        },
        Some(&mark_rule_name),
    )?;
    ensure_dnat_return_rule(cfgs)?;

    // 7. Ensure VPN interface is in WAN zone (for masquerading)
    for section in &mut cfgs["firewall"].sections {
        if let Ok(mut zone) = section.get::<FirewallZone>() {
            if zone.name == DEFAULT_WAN_ZONE {
                if !zone.network.contains(&profile.outbound) {
                    zone.network.push(profile.outbound.clone());
                    section.set(&zone)?;
                }
                break;
            }
        }
    }

    // 8. Add /32 peer routes so locally-generated responses (DNS, HTTP) reach
    //    VPN clients via wg_X instead of being caught by the /24 subnet route
    crate::vpn_server::sync_peer_policy_routes(cfgs, &profile.id.interface)?;

    Ok(())
}

const DNAT_RETURN_RULE: &str = "dnat_return";
const DNAT_RETURN_MARK: &str = "0x80/0x80";
/// Must be lower (higher priority) than VPN_ROUTING_PRIORITY so that the
/// fwmark rule is evaluated before source-based VPN routing rules.
const DNAT_RETURN_PRIORITY: u32 = 100;
/// Explicit priority for source-based VPN ip rules, ensuring they don't
/// collide with DNAT_RETURN_PRIORITY through netifd auto-assignment.
const VPN_ROUTING_PRIORITY: u32 = 200;

/// Find the firewall zone name for a profile's interface.
fn resolve_profile_zone(cfgs: &Configs, interface: &str) -> String {
    for section in &cfgs["firewall"].sections {
        if let Ok(zone) = section.get::<FirewallZone>() {
            if zone.name == DEFAULT_WAN_ZONE {
                continue;
            }
            if zone.network.iter().any(|n| n == interface) {
                return zone.name;
            }
        }
    }
    format!("vlan_{interface}")
}

/// Ensure a single `ip rule` exists that routes fwmark 0x80 packets via the
/// main routing table.  This cooperates with the per-profile mangle MARK rules
/// (dnat_mark_<interface>) to prevent DNAT reply traffic from being captured by
/// source-based VPN policy routing rules.
fn ensure_dnat_return_rule(cfgs: &mut Configs) -> Result<(), Error> {
    let exists = cfgs["network"]
        .sections
        .iter()
        .any(|s| s.name().as_deref() == Some(DNAT_RETURN_RULE));
    if !exists {
        cfgs["network"].append(
            &NetworkRule {
                src: None,
                lookup: 254, // main table
                mark: Some(DNAT_RETURN_MARK.to_string()),
                priority: Some(DNAT_RETURN_PRIORITY),
            },
            Some(DNAT_RETURN_RULE),
        )?;
    }
    Ok(())
}

/// Ensure each VPN-routed profile's routing table has routes for all sibling
/// profile subnets, so cross-VLAN traffic to/from the router stays local
/// instead of being sent through the VPN tunnel's default route.
///
/// Without these routes, a response from the router's admin IP (e.g. 192.168.0.1)
/// to a guest device (e.g. 192.168.8.X) would match the admin profile's
/// source-based policy rule and exit through the VPN instead of br-lan.101.
pub(crate) fn sync_cross_subnet_routes(cfgs: &mut Configs) -> Result<(), Error> {
    // 1. Remove all old cross-subnet routes
    cfgs["network"]
        .sections
        .retain(|s| !s.name().as_deref().map_or(false, |n| n.starts_with("pxr_")));

    // 2. Collect profiles with VPN routing (outbound != "wan")
    let vpn_profiles: Vec<(String, u16)> = cfgs["startwrt"]
        .sections
        .iter()
        .filter_map(|s| {
            let p = s.get::<UciProfile>().ok()?;
            let outbound = p.outbound.as_deref().unwrap_or("wan");
            if outbound != "wan" {
                Some((p.interface.clone(), p.vlan_tag))
            } else {
                None
            }
        })
        .collect();

    if vpn_profiles.is_empty() {
        return Ok(());
    }

    // 3. Collect all profile subnets from static bridge network interfaces
    let subnets: Vec<(String, String)> = cfgs["network"]
        .sections
        .iter()
        .filter_map(|s| {
            let iface = s.get::<NetworkInterface>().ok()?;
            let name = s.name()?.to_string();
            if iface.proto != InterfaceProto::STATIC {
                return None;
            }
            // Only include profile interfaces (br-lan / br-lan.N), not loopback etc.
            if !iface.device.starts_with("br-lan") {
                return None;
            }
            let ip = iface.ipaddr?;
            let o = ip.octets();
            Some((name, format!("{}.{}.{}.0/24", o[0], o[1], o[2])))
        })
        .collect();

    // 4. For each VPN profile, add routes for all sibling subnets
    for (vpn_iface, vlan_tag) in &vpn_profiles {
        for (sibling_iface, subnet) in &subnets {
            if sibling_iface == vpn_iface {
                continue; // self-route already handled by rewrite_routing
            }
            cfgs["network"].append(
                &NetworkRoute {
                    interface: sibling_iface.clone(),
                    target: subnet.clone(),
                    table: Some(*vlan_tag as u32),
                    ..Default::default()
                },
                Some(&format!("pxr_{}_{}", vpn_iface, sibling_iface)),
            )?;
        }
    }

    Ok(())
}

/// After routing changes, remove VPN interfaces from the WAN zone that are no longer
/// referenced by any profile's outbound field.
pub(crate) fn cleanup_orphaned_wan_vpns(cfgs: &mut Configs) {
    // Collect all VPN interface names referenced by any profile's outbound
    let mut referenced_vpns = std::collections::HashSet::new();
    for section in &cfgs["startwrt"].sections {
        if section.ty() != "profile" {
            continue;
        }
        if let Ok(profile) = section.get::<UciProfile>() {
            let outbound = profile.outbound.unwrap_or_else(|| "wan".to_string());
            if outbound != "wan" {
                referenced_vpns.insert(outbound);
            }
        }
    }

    // Remove unreferenced VPN interfaces from WAN zone
    for section in &mut cfgs["firewall"].sections {
        if let Ok(mut zone) = section.get::<FirewallZone>() {
            if zone.name == DEFAULT_WAN_ZONE {
                let before_len = zone.network.len();
                zone.network.retain(|n| {
                    // Keep "wan" and any interface still referenced by a profile
                    !n.starts_with("wg_") || referenced_vpns.contains(n)
                });
                if zone.network.len() != before_len {
                    let _ = section.set(&zone);
                }
                break;
            }
        }
    }
}

pub fn allocate_interface_name(
    ctx: &impl CtrlContext,
    hint: Option<&str>,
) -> Result<String, Error> {
    fn random() -> String {
        String::from_iter([(); INTERFACE_NAME_LIMIT].map(|_| rand::random_range('a'..='z')))
    }
    let mut name = match hint {
        Some(hint) => {
            let mut hint = hint.trim().to_ascii_lowercase();
            hint.truncate(INTERFACE_NAME_LIMIT);
            match hint.split(|c: char| !c.is_ascii_alphanumeric()).next() {
                None | Some("") => random(),
                Some(hint) => hint.to_string(),
            }
        }
        None => random(),
    };
    if !ctx.effectful() {
        return Ok(name);
    }
    for _ in 0..100 {
        let ip = Command::new("ip")
            .arg("link")
            .arg("show")
            .arg(&name)
            .stdout(Stdio::piped())
            .stderr(Stdio::piped())
            .spawn()?;
        let mut ip_out = String::new();
        ip.stderr
            .ok_or_eyre("ip did not produce output")?
            .read_to_string(&mut ip_out)?;
        if ip_out.contains("can't find device") || ip_out.contains("does not exist") {
            return Ok(name);
        }
        name = random();
    }
    Err(eyre!("gave up looking for a new interface name").into())
}

pub struct Lookup {
    list: Vec<ProfileId>,
    lan_owner: Option<ProfileId>,
    from_vlan: OnceCell<HashMap<u16, ProfileId>>,
    from_interface: OnceCell<HashMap<String, ProfileId>>,
    from_fullname: OnceCell<HashMap<String, ProfileId>>,
}

impl Lookup {
    pub fn parse<C: CtrlContext>(ctx: C, cfgs: &Configs) -> Result<Self, Error> {
        let list = list_config(ctx, cfgs)?;
        let system = list.iter().find(|p| p.interface == "lan").cloned();
        Ok(Self {
            list,
            lan_owner: system,
            from_vlan: OnceCell::new(),
            from_interface: OnceCell::new(),
            from_fullname: OnceCell::new(),
        })
    }

    pub fn list(&self) -> &[ProfileId] {
        &self.list
    }

    pub fn resolve(
        &self,
        q @ ProfileIdOpt {
            fullname,
            interface,
            vlan_tag,
        }: &ProfileIdOpt,
    ) -> Result<&ProfileId, Error> {
        if let Some(q) = interface {
            if let Some(o) = self.from_interface(q) {
                return Ok(o);
            }
        }
        if let Some(q) = vlan_tag {
            if let Some(o) = self.from_vlan(*q) {
                return Ok(o);
            }
        }
        if let Some(q) = fullname {
            if let Some(o) = self.from_fullname(q) {
                return Ok(o);
            }
        }
        Err(ErrorKind::MissingProfile { id: q.clone() }.into())
    }

    pub fn from_vlan(&self, vlan_tag: u16) -> Option<&ProfileId> {
        self.from_vlan
            .get_or_init(|| {
                self.list
                    .iter()
                    .map(|id| (id.vlan_tag, id.clone()))
                    .collect()
            })
            .get(&vlan_tag)
    }

    pub fn from_interface(&self, interface: &str) -> Option<&ProfileId> {
        self.from_interface
            .get_or_init(|| {
                self.list
                    .iter()
                    .map(|id| (id.interface.clone(), id.clone()))
                    .collect()
            })
            .get(interface)
    }

    pub fn from_fullname(&self, fullname: &str) -> Option<&ProfileId> {
        self.from_fullname
            .get_or_init(|| {
                self.list
                    .iter()
                    .map(|id| (id.fullname.clone(), id.clone()))
                    .collect()
            })
            .get(fullname)
    }
}

/// Bootstrap the default Admin security profile on first boot.
///
/// Registers the existing LAN infrastructure (network interface, firewall zone,
/// DHCP server) as the "Admin" profile in the `startwrt` UCI config. This is a
/// lightweight operation — it only writes the `startwrt` config entry and does
/// not create VLANs, bridge-vlans, or modify network topology.
///
/// Idempotent: returns `Ok(())` immediately if a LAN owner profile already exists.
pub fn bootstrap_admin_profile(uci_root: &str) -> Result<(), Error> {
    let arena = Arena::new();
    let mut cfgs = parse_all(uci_root, &arena, &["startwrt"])?;

    // Check if a LAN owner profile already exists
    for section in &cfgs["startwrt"].sections {
        if let Ok(profile) = section.get::<UciProfile>() {
            if profile.interface == "lan" {
                return Ok(());
            }
        }
    }

    // Register the existing LAN as the Admin profile
    cfgs["startwrt"].append(
        &UciProfile {
            fullname: "Admin".into(),
            interface: "lan".into(),
            vlan_tag: 1,
            outbound: Some("wan".into()),
            access_to_new_profiles: true,
            wan_access: Some("all".into()),
            wan_access_list: Vec::new(),
            dns_override: Vec::new(),
            wan_schedule: Vec::new(),
        },
        Some("lan"),
    )?;

    // Write default preferences so the frontend has values on first load
    cfgs["startwrt"].append(
        &UciPreferences {
            language: "en_US".to_string(),
            theme: "system".to_string(),
            remote_access: "default".to_string(),
        },
        Some("preferences"),
    )?;

    dump_all(uci_root, cfgs)?;
    Ok(())
}

#[derive(Debug, Parser, Serialize, Deserialize)]
pub struct EditArgs {
    #[clap(flatten)]
    pub get: ProfileIdOpt,
    #[clap(long)]
    pub create: bool,
}

pub fn edit<C: CtrlContext>(ctx: C, args: EditArgs) -> Result<ProfileId, Error> {
    if args.create {
        let template = Profile {
            id: args.get.clone(),
            gateway_ip: std::net::Ipv4Addr::new(192, 168, 1, 1),
            outbound: "wan".to_string(),
            lan_access: LanAccess::All,
            wan_access: WanAccess::All,
            dns_override: Vec::new(),
            dns_source: String::new(),
            access_to_new_profiles: true,
            owns_lan: list(ctx.clone())?.is_empty(),
        };
        let modified_profile: Profile<ProfileIdOpt> = crate::utils::edit_in_editor(&template)?;
        create(ctx, DeserializeStdin(modified_profile))
    } else {
        // Edit mode: get existing profile, edit, then set
        let current_profile = get(ctx.clone(), args.get)?;
        let current_profile = Profile {
            id: current_profile.id.into(),
            gateway_ip: current_profile.gateway_ip,
            outbound: current_profile.outbound,
            lan_access: match current_profile.lan_access {
                LanAccess::All => LanAccess::All,
                LanAccess::SameProfile => LanAccess::SameProfile,
                LanAccess::OtherProfiles(set) => {
                    LanAccess::OtherProfiles(set.into_iter().map(Into::into).collect())
                }
            },
            wan_access: current_profile.wan_access,
            dns_override: current_profile.dns_override,
            dns_source: String::new(),
            access_to_new_profiles: current_profile.access_to_new_profiles,
            owns_lan: current_profile.owns_lan,
        };
        let modified_profile = crate::utils::edit_in_editor(&current_profile)?;
        set(ctx, DeserializeStdin(ProfileSetRequest {
            profile: modified_profile,
            force: false,
        }))
    }
}

// ── Schedule helpers ──────────────────────────────────────────────────

const SCHEDULE_TAG: &str = "# start-wrt-wan-schedule";
const CRONTAB_PATH: &str = "/etc/crontabs/root";
const SCHEDULE_RULE_PREFIX: &str = "sched_";

fn schedule_crontab_path(ctx: &impl CtrlContext) -> PathBuf {
    if ctx.effectful() {
        PathBuf::from(CRONTAB_PATH)
    } else {
        ctx.uci_root().join("crontab_root")
    }
}

fn parse_hhmm(s: &str) -> Result<(u32, u32), Error> {
    let parts: Vec<&str> = s.split(':').collect();
    if parts.len() != 2 {
        return Err(Error::other(format!("invalid time format: {s:?}")));
    }
    let h: u32 = parts[0]
        .parse()
        .map_err(|_| Error::other(format!("invalid hour: {}", parts[0])))?;
    let m: u32 = parts[1]
        .parse()
        .map_err(|_| Error::other(format!("invalid minute: {}", parts[1])))?;
    if h > 23 || m > 59 {
        return Err(Error::other(format!("time out of range: {s:?}")));
    }
    Ok((h, m))
}

fn serialize_schedule_windows(windows: &[ScheduleWindow]) -> Vec<String> {
    windows
        .iter()
        .map(|w| {
            let days: String = w
                .days
                .iter()
                .enumerate()
                .filter(|(_, &on)| on)
                .map(|(i, _)| i.to_string())
                .collect::<Vec<_>>()
                .join(",");
            format!("{}|{}|{}", w.start_time, w.end_time, days)
        })
        .collect()
}

fn parse_schedule_windows(raw: &[String]) -> Vec<ScheduleWindow> {
    raw.iter()
        .filter_map(|s| {
            let parts: Vec<&str> = s.splitn(3, '|').collect();
            if parts.len() != 3 {
                return None;
            }
            let mut days = [false; 7];
            if !parts[2].is_empty() {
                for d in parts[2].split(',') {
                    if let Ok(n) = d.parse::<usize>() {
                        if n < 7 {
                            days[n] = true;
                        }
                    }
                }
            }
            Some(ScheduleWindow {
                start_time: parts[0].to_string(),
                end_time: parts[1].to_string(),
                days,
            })
        })
        .collect()
}

#[derive(Debug, Deserialize, Serialize)]
struct ScheduleGetParams {
    interface: String,
}

/// Read schedule data for a single profile from UCI.
pub fn schedule_get<C: CtrlContext>(
    ctx: C,
    DeserializeStdin(params): DeserializeStdin<ScheduleGetParams>,
) -> Result<Vec<ScheduleWindow>, Error> {
    let arena = Arena::new();
    let cfgs = parse_all(ctx.uci_root(), &arena, &["startwrt"])?;

    for section in &cfgs["startwrt"].sections {
        if let Some(profile) = section.get_typed::<UciProfile>()? {
            if profile.interface == params.interface {
                return Ok(parse_schedule_windows(&profile.wan_schedule));
            }
        }
    }
    Err(ErrorKind::MissingProfile {
        id: ProfileIdOpt {
            interface: Some(params.interface),
            fullname: None,
            vlan_tag: None,
        },
    }
    .into())
}

/// Write schedule data for a single profile, regenerate crontab and iptables rules.
pub fn schedule_set<C: CtrlContext>(
    ctx: C,
    DeserializeStdin(params): DeserializeStdin<ScheduleWindows>,
) -> Result<(), Error> {
    // Validate all times before writing to UCI
    for window in &params.windows {
        parse_hhmm(&window.start_time)?;
        parse_hhmm(&window.end_time)?;
    }

    let serialized = serialize_schedule_windows(&params.windows);

    let mut retries = 4;
    loop {
        let arena = Arena::new();
        let mut cfgs = parse_all(ctx.uci_root(), &arena, &["startwrt"])?;

        let mut found = false;
        for section in &mut cfgs["startwrt"].sections {
            if let Some(profile) = section.get_typed::<UciProfile>()? {
                if profile.interface == params.interface {
                    // Remove existing wan_schedule list entries
                    section.lines.retain(|line| {
                        !matches!(line, Line::List { list, .. } if list.as_str() == "wan_schedule")
                    });
                    // Add new entries
                    let arena = section.arena;
                    for val in &serialized {
                        section.lines.push(Line::List {
                            list: Token::from_string("wan_schedule".to_string(), arena),
                            item: Token::from_string(val.clone(), arena),
                            comment: LineComment::None,
                        });
                    }
                    found = true;
                    break;
                }
            }
        }
        if !found {
            return Err(ErrorKind::MissingProfile {
                id: ProfileIdOpt {
                    interface: Some(params.interface.clone()),
                    fullname: None,
                    vlan_tag: None,
                },
            }
            .into());
        }

        match dump_all(ctx.uci_root(), cfgs) {
            Err(uciedit::Error::Conflict { .. }) if retries > 0 => {
                retries -= 1;
                continue;
            }
            Err(err) => {
                crate::activity::log(
                    "profile",
                    "schedule-updated",
                    false,
                    "Failed to update WAN schedule",
                    Some(&err.to_string()),
                );
                return Err(err.into());
            }
            Ok(()) => break,
        }
    }

    // Regenerate crontab entries for all profile schedules
    regenerate_schedule_crontab(&ctx)?;

    // Evaluate current schedule state and apply UCI firewall rules
    if ctx.effectful() {
        evaluate_and_apply_schedules(&ctx)?;
        crate::run_quiet(Command::new("/etc/init.d/cron").arg("restart"))
            .map_err(|e| Error::other(format!("restarting cron: {e}")))?;
    }

    crate::activity::log(
        "profile",
        "schedule-updated",
        true,
        &format!("Updated WAN schedule for profile {}", params.interface),
        None,
    );
    Ok(())
}

/// Regenerate all profile schedule crontab entries from UCI.
pub(crate) fn regenerate_schedule_crontab(ctx: &impl CtrlContext) -> Result<(), Error> {
    let path = schedule_crontab_path(ctx);
    let content = std::fs::read_to_string(&path).unwrap_or_default();

    // Remove old schedule entries
    let filtered: Vec<&str> = content
        .lines()
        .filter(|l| !l.contains(SCHEDULE_TAG))
        .collect();
    let mut new_content = filtered.join("\n");
    if !new_content.is_empty() && !new_content.ends_with('\n') {
        new_content.push('\n');
    }

    // Read profiles and firewall config (for zone name resolution)
    let arena = Arena::new();
    let cfgs = parse_all(ctx.uci_root(), &arena, &["startwrt", "firewall"])?;

    for section in &cfgs["startwrt"].sections {
        if let Some(profile) = section.get_typed::<UciProfile>()? {
            let windows = parse_schedule_windows(&profile.wan_schedule);
            if windows.is_empty() {
                continue;
            }
            let iface = &profile.interface;
            let zone = find_zone_for_interface(&cfgs, iface);
            let sec = format!("{SCHEDULE_RULE_PREFIX}{iface}");

            for window in &windows {
                let days_str: String = window
                    .days
                    .iter()
                    .enumerate()
                    .filter(|(_, &on)| on)
                    .map(|(i, _)| i.to_string())
                    .collect::<Vec<_>>()
                    .join(",");

                if days_str.is_empty() {
                    continue;
                }

                let (start_h, start_m) = parse_hhmm(&window.start_time)?;
                let (end_h, end_m) = parse_hhmm(&window.end_time)?;

                // Block at start_time: add UCI firewall rule + reload
                new_content.push_str(&format!(
                    "{start_m} {start_h} * * {days_str} \
                     uci set firewall.{sec}=rule; \
                     uci set firewall.{sec}.name='WAN-Schedule-{iface}'; \
                     uci set firewall.{sec}.src='{zone}'; \
                     uci set firewall.{sec}.dest='wan'; \
                     uci set firewall.{sec}.target='REJECT'; \
                     uci commit firewall; \
                     /etc/init.d/firewall reload \
                     {SCHEDULE_TAG}\n"
                ));
                // Unblock at end_time: remove UCI section + reload
                new_content.push_str(&format!(
                    "{end_m} {end_h} * * {days_str} \
                     uci -q delete firewall.{sec}; \
                     uci commit firewall; \
                     /etc/init.d/firewall reload \
                     {SCHEDULE_TAG}\n"
                ));
            }
        }
    }

    if let Some(parent) = path.parent() {
        std::fs::create_dir_all(parent)?;
    }
    std::fs::write(&path, &new_content)?;
    Ok(())
}

/// Find the firewall zone name for a profile's interface.
fn find_zone_for_interface(cfgs: &Configs, interface: &str) -> String {
    cfgs["firewall"]
        .sections
        .iter()
        .find_map(|s| {
            let zone = s.get::<FirewallZone>().ok()?;
            zone.network
                .iter()
                .any(|n| n == interface)
                .then(|| zone.name.clone())
        })
        .unwrap_or_else(|| format!("vlan_{interface}"))
}

/// Evaluate current time against all profile schedules and manage UCI firewall
/// REJECT rules for currently-blocked profiles. Adds/removes named `sched_*`
/// sections in the firewall config, then reloads fw3 if anything changed.
pub(crate) fn evaluate_and_apply_schedules(ctx: &impl CtrlContext) -> Result<(), Error> {
    let arena = Arena::new();
    let mut cfgs = parse_all(ctx.uci_root(), &arena, &["startwrt", "firewall"])?;

    let now = chrono_now();
    let current_day = now.0; // 0=Sun..6=Sat
    let current_minutes = now.1; // minutes since midnight

    // Collect interfaces that should be blocked right now
    let mut should_block: BTreeSet<String> = BTreeSet::new();

    for section in &cfgs["startwrt"].sections {
        if let Some(profile) = section.get_typed::<UciProfile>()? {
            let windows = parse_schedule_windows(&profile.wan_schedule);

            for window in &windows {
                if !window.days[current_day] {
                    continue;
                }
                if let (Ok((sh, sm)), Ok((eh, em))) =
                    (parse_hhmm(&window.start_time), parse_hhmm(&window.end_time))
                {
                    let start_min = sh * 60 + sm;
                    let end_min = eh * 60 + em;
                    let is_blocked = if start_min <= end_min {
                        current_minutes >= start_min && current_minutes < end_min
                    } else {
                        current_minutes >= start_min || current_minutes < end_min
                    };
                    if is_blocked {
                        should_block.insert(profile.interface.clone());
                        break;
                    }
                }
            }
        }
    }

    // Determine which sched_ sections already exist
    let existing: BTreeSet<String> = cfgs["firewall"]
        .sections
        .iter()
        .filter_map(|s| {
            let name = s.name()?;
            name.strip_prefix(SCHEDULE_RULE_PREFIX)
                .map(|iface| iface.to_string())
        })
        .collect();

    let to_add: BTreeSet<&String> = should_block.difference(&existing).collect();
    let to_remove: BTreeSet<&String> = existing.difference(&should_block).collect();

    if to_add.is_empty() && to_remove.is_empty() {
        return Ok(());
    }

    // Remove stale schedule rules
    if !to_remove.is_empty() {
        cfgs["firewall"].sections.retain(|s| {
            s.name()
                .and_then(|n| n.strip_prefix(SCHEDULE_RULE_PREFIX).map(String::from))
                .map(|iface| !to_remove.contains(&iface))
                .unwrap_or(true)
        });
    }

    // Add missing schedule rules
    for iface in &to_add {
        let zone = find_zone_for_interface(&cfgs, iface);
        let section_name = format!("{SCHEDULE_RULE_PREFIX}{iface}");
        cfgs["firewall"].append(
            &FirewallRule {
                name: format!("WAN-Schedule-{iface}"),
                src: zone,
                dest: Some("wan".into()),
                target: FirewallTarget::REJECT,
                ..Default::default()
            },
            Some(&section_name),
        )?;
    }

    dump_all(ctx.uci_root(), cfgs)?;

    if ctx.effectful() {
        crate::run_quiet(Command::new("/etc/init.d/firewall").arg("reload"))
            .map_err(|e| Error::other(format!("reloading firewall: {e}")))?;
    }

    Ok(())
}

/// Returns (day_of_week 0=Sun..6=Sat, minutes_since_midnight).
fn chrono_now() -> (usize, u32) {
    // Use the `date` command for portability on OpenWrt (no chrono crate).
    let output = Command::new("date")
        .args(["+%w %H %M"])
        .output()
        .ok()
        .and_then(|o| String::from_utf8(o.stdout).ok())
        .unwrap_or_default();
    let parts: Vec<&str> = output.trim().split_whitespace().collect();
    if parts.len() == 3 {
        let dow = parts[0].parse::<usize>().unwrap_or(0);
        let h = parts[1].parse::<u32>().unwrap_or(0);
        let m = parts[2].parse::<u32>().unwrap_or(0);
        (dow, h * 60 + m)
    } else {
        tracing::warn!("Failed to parse current time from `date` command");
        (0, 0)
    }
}

#[cfg(test)]
mod tests {
    use super::*;
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

    /// Write all five UCI configs used by profile delete.
    /// Two profiles: Admin (lan, vlan 99) and Guest (guest, vlan 101).
    /// The Guest profile has a WiFi station + VLAN section in wireless.
    fn setup_configs(dir: &std::path::Path) {
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

config bridge-vlan
\toption device 'br-lan'
\toption vlan '99'

config bridge-vlan
\toption device 'br-lan'
\toption vlan '101'
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

config rule
\toption name 'Allow-DHCP-DNS-Guest'
\toption src 'vlan_guest'
\toption dest_port '53 67 68'
\tlist proto 'tcp'
\tlist proto 'udp'
\tlist proto 'icmp'
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

        std::fs::write(
            dir.join("wireless"),
            "\
config wifi-device 'radio0'
\toption type 'mac80211'
\toption band '2g'
\toption channel '1'

config wifi-iface 'default_radio0'
\toption device 'radio0'
\toption mode 'ap'
\toption ssid 'TestNet'
\toption encryption 'psk2'
\toption key 'adminpass1'
\toption dynamic_vlan '1'

config wifi-station
\toption key 'guestpass1'
\toption vid '101'
\toption iface 'default_radio0'
\toption label 'Guest WiFi'

config wifi-vlan
\toption name 'guest'
\toption network 'guest'
\toption vid '101'
\toption iface 'default_radio0'
",
        )
        .unwrap();
    }

    #[test]
    fn test_delete_removes_wireless_sections() {
        let dir = tempfile::tempdir().unwrap();
        let ctx = TestContext(dir.path().to_path_buf());
        setup_configs(dir.path());

        let arena = Arena::new();
        let mut cfgs = parse_all(
            ctx.uci_root(),
            &arena,
            &["startwrt", "network", "firewall", "dhcp", "wireless"],
        )
        .unwrap();

        // Verify wireless sections exist before delete
        let mut station_count = 0;
        let mut vlan_count = 0;
        for section in &cfgs["wireless"].sections {
            if let Ok(s) = section.get::<WifiStation>() {
                if s.vid == Some(101) {
                    station_count += 1;
                }
            }
            if let Ok(v) = section.get::<WifiVlan>() {
                if v.vid == 101 {
                    vlan_count += 1;
                }
            }
        }
        assert_eq!(station_count, 1, "expected 1 WifiStation with vid=101 before delete");
        assert_eq!(vlan_count, 1, "expected 1 WifiVlan with vid=101 before delete");

        // Delete the guest profile
        delete_config(
            ctx,
            &mut cfgs,
            &ProfileIdOpt {
                fullname: None,
                interface: Some("guest".into()),
                vlan_tag: None,
            },
        )
        .unwrap();

        // WifiStation and WifiVlan for vid=101 must be gone
        for section in &cfgs["wireless"].sections {
            if let Ok(s) = section.get::<WifiStation>() {
                assert_ne!(
                    s.vid,
                    Some(101),
                    "WifiStation with vid=101 should have been removed"
                );
            }
            if let Ok(v) = section.get::<WifiVlan>() {
                assert_ne!(v.vid, 101, "WifiVlan with vid=101 should have been removed");
            }
        }

        // wifi-device and wifi-iface sections must survive
        let remaining_types: Vec<String> = cfgs["wireless"]
            .sections
            .iter()
            .map(|s| s.ty().to_string())
            .collect();
        assert!(
            remaining_types.contains(&"wifi-device".to_string()),
            "wifi-device should survive delete"
        );
        assert!(
            remaining_types.contains(&"wifi-iface".to_string()),
            "wifi-iface should survive delete"
        );
    }

    #[test]
    fn test_delete_preserves_other_profile_wireless() {
        let dir = tempfile::tempdir().unwrap();
        let ctx = TestContext(dir.path().to_path_buf());
        setup_configs(dir.path());

        // Add a second profile "kids" with vlan 102 and its own wireless sections
        std::fs::write(
            dir.path().join("startwrt"),
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

config profile kids
\toption fullname 'Kids'
\toption interface 'kids'
\toption vlan_tag '102'
",
        )
        .unwrap();

        // Append kids wireless sections alongside guest's
        std::fs::write(
            dir.path().join("wireless"),
            "\
config wifi-device 'radio0'
\toption type 'mac80211'
\toption band '2g'
\toption channel '1'

config wifi-iface 'default_radio0'
\toption device 'radio0'
\toption mode 'ap'
\toption ssid 'TestNet'
\toption encryption 'psk2'
\toption key 'adminpass1'
\toption dynamic_vlan '1'

config wifi-station
\toption key 'guestpass1'
\toption vid '101'
\toption iface 'default_radio0'

config wifi-vlan
\toption name 'guest'
\toption network 'guest'
\toption vid '101'
\toption iface 'default_radio0'

config wifi-station
\toption key 'kidspass1'
\toption vid '102'
\toption iface 'default_radio0'

config wifi-vlan
\toption name 'kids'
\toption network 'kids'
\toption vid '102'
\toption iface 'default_radio0'
",
        )
        .unwrap();

        // Also need network/firewall/dhcp entries for kids so Lookup doesn't fail
        std::fs::write(
            dir.path().join("network"),
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

config interface 'kids'
\toption device 'br-lan.102'
\toption proto 'static'
\toption ipaddr '192.168.102.1'
\toption netmask '255.255.255.0'
",
        )
        .unwrap();

        let arena = Arena::new();
        let mut cfgs = parse_all(
            ctx.uci_root(),
            &arena,
            &["startwrt", "network", "firewall", "dhcp", "wireless"],
        )
        .unwrap();

        // Delete guest (vlan 101)
        delete_config(
            ctx,
            &mut cfgs,
            &ProfileIdOpt {
                fullname: None,
                interface: Some("guest".into()),
                vlan_tag: None,
            },
        )
        .unwrap();

        // Kids wireless sections (vid=102) must survive
        let mut found_kids_station = false;
        let mut found_kids_vlan = false;
        for section in &cfgs["wireless"].sections {
            if let Ok(s) = section.get::<WifiStation>() {
                assert_ne!(s.vid, Some(101), "guest station should be gone");
                if s.vid == Some(102) {
                    found_kids_station = true;
                }
            }
            if let Ok(v) = section.get::<WifiVlan>() {
                assert_ne!(v.vid, 101, "guest vlan should be gone");
                if v.vid == 102 {
                    found_kids_vlan = true;
                }
            }
        }
        assert!(found_kids_station, "kids WifiStation (vid=102) should survive");
        assert!(found_kids_vlan, "kids WifiVlan (vid=102) should survive");
    }

    #[test]
    fn test_delete_cannot_delete_lan_owner() {
        let dir = tempfile::tempdir().unwrap();
        let ctx = TestContext(dir.path().to_path_buf());
        setup_configs(dir.path());

        let arena = Arena::new();
        let mut cfgs = parse_all(
            ctx.uci_root(),
            &arena,
            &["startwrt", "network", "firewall", "dhcp", "wireless"],
        )
        .unwrap();

        let result = delete_config(
            ctx,
            &mut cfgs,
            &ProfileIdOpt {
                fullname: None,
                interface: Some("lan".into()),
                vlan_tag: None,
            },
        );
        assert!(result.is_err(), "deleting lan owner should fail");
    }

    #[test]
    fn test_set_config_prefers_br_lan_bridge() {
        let dir = tempfile::tempdir().unwrap();
        let ctx = TestContext(dir.path().to_path_buf());

        // Network with two bridges: br-guest first, then br-lan
        std::fs::write(
            dir.path().join("network"),
            "\
config device
\toption name 'br-guest'
\toption type 'bridge'
\tlist ports 'eth1'

config device
\toption name 'br-lan'
\toption type 'bridge'
\tlist ports 'eth0'

config interface 'lan'
\toption device 'br-lan.99'
\toption proto 'static'
\toption ipaddr '192.168.1.1'
\toption netmask '255.255.255.0'
",
        )
        .unwrap();

        std::fs::write(
            dir.path().join("startwrt"),
            "\
config profile lan
\toption fullname 'Admin'
\toption interface 'lan'
\toption vlan_tag '99'
\toption access_to_new_profiles '1'

config profile newp
\toption fullname 'NewProfile'
\toption interface 'newp'
\toption vlan_tag '200'
",
        )
        .unwrap();

        std::fs::write(
            dir.path().join("firewall"),
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

config forwarding
\toption src 'lan'
\toption dest 'wan'
",
        )
        .unwrap();

        std::fs::write(
            dir.path().join("dhcp"),
            "\
config dhcp 'lan'
\toption interface 'lan'
\toption start '100'
\toption limit '150'
\toption leasetime '12h'
",
        )
        .unwrap();

        let arena = Arena::new();
        let mut cfgs = parse_all(
            ctx.uci_root(),
            &arena,
            &["startwrt", "network", "firewall", "dhcp"],
        )
        .unwrap();

        // set_config for "newp" — interface doesn't exist yet, so it must
        // pick a bridge. With the fix, it should prefer br-lan over br-guest.
        let profile = Profile {
            id: ProfileIdOpt {
                fullname: Some("NewProfile".into()),
                interface: Some("newp".into()),
                vlan_tag: Some(200),
            },
            gateway_ip: Ipv4Addr::new(192, 168, 200, 1),
            outbound: "wan".into(),
            lan_access: LanAccess::SameProfile,
            wan_access: WanAccess::All,
            dns_override: Vec::new(),
            dns_source: String::new(),
            access_to_new_profiles: false,
            owns_lan: false,
        };

        set_config(ctx.clone(), &mut cfgs, &profile).unwrap();

        // Read the newly created interface directly — no dump+parse round-trip needed.
        let found_device = cfgs["network"]
            .sections
            .iter()
            .find(|s| s.name().as_deref() == Some("newp"))
            .and_then(|s| s.get::<NetworkInterface>().ok())
            .map(|iface| iface.device);
        assert_eq!(
            found_device.as_deref(),
            Some("br-lan.200"),
            "set_config should prefer br-lan even when br-guest appears first"
        );
    }

    #[test]
    fn test_whitelist_firewall_rules() {
        let dir = tempfile::tempdir().unwrap();
        let ctx = TestContext(dir.path().to_path_buf());
        setup_configs(dir.path());

        let arena = Arena::new();
        let mut cfgs = parse_all(
            ctx.uci_root(),
            &arena,
            &["startwrt", "network", "firewall", "dhcp"],
        )
        .unwrap();

        let profile = Profile {
            id: ProfileIdOpt {
                fullname: Some("Guest".into()),
                interface: Some("guest".into()),
                vlan_tag: Some(101),
            },
            gateway_ip: Ipv4Addr::new(192, 168, 101, 1),
            outbound: "wan".into(),
            lan_access: LanAccess::SameProfile,
            wan_access: WanAccess::Whitelist(vec!["1.1.1.1/32".into(), "8.8.8.8/32".into()]),
            dns_override: Vec::new(),
            dns_source: String::new(),
            access_to_new_profiles: false,
            owns_lan: false,
        };

        set_config(ctx, &mut cfgs, &profile).unwrap();

        // Should have a forwarding to wan (needed for rule matching)
        let has_wan_fwd = cfgs["firewall"].sections.iter().any(|s| {
            s.get::<FirewallForwarding>()
                .map(|f| f.src == "vlan_guest" && f.dest == DEFAULT_WAN_ZONE)
                .unwrap_or(false)
        });
        assert!(has_wan_fwd, "whitelist should create forwarding to wan");

        // Should have ACCEPT rules for each destination
        let accept_rules: Vec<_> = cfgs["firewall"]
            .sections
            .iter()
            .filter_map(|s| s.get::<FirewallRule>().ok())
            .filter(|r| {
                r.src == "vlan_guest"
                    && r.dest.as_deref() == Some(DEFAULT_WAN_ZONE)
                    && r.target == FirewallTarget::ACCEPT
                    && r.dest_ip.is_some()
            })
            .collect();
        assert_eq!(accept_rules.len(), 2, "should have 2 ACCEPT rules for whitelisted IPs");
        assert_eq!(accept_rules[0].dest_ip.as_deref(), Some("1.1.1.1/32"));
        assert_eq!(accept_rules[1].dest_ip.as_deref(), Some("8.8.8.8/32"));

        // Should have a catch-all REJECT rule
        let reject_rule = cfgs["firewall"]
            .sections
            .iter()
            .filter_map(|s| s.get::<FirewallRule>().ok())
            .find(|r| {
                r.src == "vlan_guest"
                    && r.dest.as_deref() == Some(DEFAULT_WAN_ZONE)
                    && r.target == FirewallTarget::REJECT
                    && r.dest_ip.is_none()
            });
        assert!(reject_rule.is_some(), "whitelist should have a catch-all REJECT rule");
    }

    #[test]
    fn test_blacklist_firewall_rules() {
        let dir = tempfile::tempdir().unwrap();
        let ctx = TestContext(dir.path().to_path_buf());
        setup_configs(dir.path());

        let arena = Arena::new();
        let mut cfgs = parse_all(
            ctx.uci_root(),
            &arena,
            &["startwrt", "network", "firewall", "dhcp"],
        )
        .unwrap();

        let profile = Profile {
            id: ProfileIdOpt {
                fullname: Some("Guest".into()),
                interface: Some("guest".into()),
                vlan_tag: Some(101),
            },
            gateway_ip: Ipv4Addr::new(192, 168, 101, 1),
            outbound: "wan".into(),
            lan_access: LanAccess::SameProfile,
            wan_access: WanAccess::Blacklist(vec!["10.0.0.0/8".into()]),
            dns_override: Vec::new(),
            dns_source: String::new(),
            access_to_new_profiles: false,
            owns_lan: false,
        };

        set_config(ctx, &mut cfgs, &profile).unwrap();

        // Should have forwarding to wan (default allow)
        let has_wan_fwd = cfgs["firewall"].sections.iter().any(|s| {
            s.get::<FirewallForwarding>()
                .map(|f| f.src == "vlan_guest" && f.dest == DEFAULT_WAN_ZONE)
                .unwrap_or(false)
        });
        assert!(has_wan_fwd, "blacklist should create forwarding to wan");

        // Should have REJECT rules for blocked destinations
        let reject_rules: Vec<_> = cfgs["firewall"]
            .sections
            .iter()
            .filter_map(|s| s.get::<FirewallRule>().ok())
            .filter(|r| {
                r.src == "vlan_guest"
                    && r.dest.as_deref() == Some(DEFAULT_WAN_ZONE)
                    && r.target == FirewallTarget::REJECT
                    && r.dest_ip.is_some()
            })
            .collect();
        assert_eq!(reject_rules.len(), 1, "should have 1 REJECT rule for blacklisted IP");
        assert_eq!(reject_rules[0].dest_ip.as_deref(), Some("10.0.0.0/8"));

        // Should NOT have a catch-all reject (blacklist allows everything else)
        let catchall_reject = cfgs["firewall"]
            .sections
            .iter()
            .filter_map(|s| s.get::<FirewallRule>().ok())
            .find(|r| {
                r.src == "vlan_guest"
                    && r.dest.as_deref() == Some(DEFAULT_WAN_ZONE)
                    && r.target == FirewallTarget::REJECT
                    && r.dest_ip.is_none()
            });
        assert!(catchall_reject.is_none(), "blacklist should NOT have a catch-all REJECT");
    }

    #[test]
    fn test_wan_access_round_trip() {
        let dir = tempfile::tempdir().unwrap();
        let ctx = TestContext(dir.path().to_path_buf());
        setup_configs(dir.path());

        let arena = Arena::new();
        let mut cfgs = parse_all(
            ctx.uci_root(),
            &arena,
            &["startwrt", "network", "firewall", "dhcp"],
        )
        .unwrap();

        // Set guest profile with whitelist
        let profile = Profile {
            id: ProfileIdOpt {
                fullname: Some("Guest".into()),
                interface: Some("guest".into()),
                vlan_tag: Some(101),
            },
            gateway_ip: Ipv4Addr::new(192, 168, 101, 1),
            outbound: "wan".into(),
            lan_access: LanAccess::SameProfile,
            wan_access: WanAccess::Whitelist(vec!["1.1.1.1/32".into(), "8.8.8.8/32".into()]),
            dns_override: Vec::new(),
            dns_source: String::new(),
            access_to_new_profiles: false,
            owns_lan: false,
        };

        set_config(ctx.clone(), &mut cfgs, &profile).unwrap();

        // Dump and re-parse to simulate a real round-trip
        dump_all(ctx.uci_root(), cfgs).unwrap();
        let arena2 = Arena::new();
        let cfgs2 = parse_all(
            ctx.uci_root(),
            &arena2,
            &["startwrt", "network", "firewall"],
        )
        .unwrap();

        let result = get_config(
            ctx,
            &cfgs2,
            ProfileIdOpt {
                fullname: None,
                interface: Some("guest".into()),
                vlan_tag: None,
            },
        )
        .unwrap();

        match &result.wan_access {
            WanAccess::Whitelist(dests) => {
                assert_eq!(dests, &["1.1.1.1/32", "8.8.8.8/32"]);
            }
            other => panic!("expected Whitelist, got {:?}", other),
        }
    }

    #[test]
    fn test_set_persists_outbound() {
        let dir = tempfile::tempdir().unwrap();
        let ctx = TestContext(dir.path().to_path_buf());
        setup_configs(dir.path());

        let arena = Arena::new();
        let mut cfgs = parse_all(
            ctx.uci_root(),
            &arena,
            &["startwrt", "network", "firewall", "dhcp"],
        )
        .unwrap();

        // Set guest profile with non-default outbound
        let profile = Profile {
            id: ProfileIdOpt {
                fullname: Some("Guest".into()),
                interface: Some("guest".into()),
                vlan_tag: Some(101),
            },
            gateway_ip: Ipv4Addr::new(192, 168, 101, 1),
            outbound: "wg0".into(),
            lan_access: LanAccess::SameProfile,
            wan_access: WanAccess::All,
            dns_override: Vec::new(),
            dns_source: String::new(),
            access_to_new_profiles: false,
            owns_lan: false,
        };

        set_config(ctx.clone(), &mut cfgs, &profile).unwrap();

        // Dump and re-parse
        dump_all(ctx.uci_root(), cfgs).unwrap();
        let arena2 = Arena::new();
        let cfgs2 = parse_all(
            ctx.uci_root(),
            &arena2,
            &["startwrt", "network", "firewall"],
        )
        .unwrap();

        let result = get_config(
            ctx,
            &cfgs2,
            ProfileIdOpt {
                fullname: None,
                interface: Some("guest".into()),
                vlan_tag: None,
            },
        )
        .unwrap();

        assert_eq!(result.outbound, "wg0", "outbound should persist through set_config");
    }

    #[test]
    fn test_outbound_vpn_creates_routing() {
        let dir = tempfile::tempdir().unwrap();
        let ctx = TestContext(dir.path().to_path_buf());
        setup_configs(dir.path());

        let arena = Arena::new();
        let mut cfgs = parse_all(
            ctx.uci_root(),
            &arena,
            &["startwrt", "network", "firewall", "dhcp"],
        )
        .unwrap();

        // Set guest profile with VPN outbound
        let profile = Profile {
            id: ProfileIdOpt {
                fullname: Some("Guest".into()),
                interface: Some("guest".into()),
                vlan_tag: Some(101),
            },
            gateway_ip: Ipv4Addr::new(192, 168, 101, 1),
            outbound: "wg_test".into(),
            lan_access: LanAccess::SameProfile,
            wan_access: WanAccess::All,
            dns_override: Vec::new(),
            dns_source: String::new(),
            access_to_new_profiles: false,
            owns_lan: false,
        };

        set_config(ctx, &mut cfgs, &profile).unwrap();

        // Verify route section was created
        let route = cfgs["network"]
            .sections
            .iter()
            .find(|s| s.name().as_deref() == Some("prt_guest"))
            .expect("route section prt_guest should exist");
        let route_data = route.get::<NetworkRoute>().unwrap();
        assert_eq!(route_data.interface, "wg_test");
        assert_eq!(route_data.target, "0.0.0.0/0");
        assert_eq!(route_data.table, Some(101));

        // Verify rule section was created
        let rule = cfgs["network"]
            .sections
            .iter()
            .find(|s| s.name().as_deref() == Some("prr_guest"))
            .expect("rule section prr_guest should exist");
        let rule_data = rule.get::<NetworkRule>().unwrap();
        assert_eq!(rule_data.src.as_deref(), Some("192.168.101.0/24"));
        assert_eq!(rule_data.lookup, 101);
        assert_eq!(rule_data.priority, Some(200));

        // Verify dnat_return ip rule was created for port-forward reply routing
        let dnat_rule = cfgs["network"]
            .sections
            .iter()
            .find(|s| s.name().as_deref() == Some("dnat_return"))
            .expect("dnat_return rule should exist when VPN routing is active");
        let dnat_data = dnat_rule.get::<NetworkRule>().unwrap();
        assert_eq!(dnat_data.mark.as_deref(), Some("0x80/0x80"));
        assert_eq!(dnat_data.lookup, 254);
        assert_eq!(dnat_data.priority, Some(100));
        assert!(dnat_data.src.is_none());

        // Verify mangle MARK rule was created for the profile's zone
        let mark_rule = cfgs["firewall"]
            .sections
            .iter()
            .find(|s| s.name().as_deref() == Some("dnat_mark_guest"))
            .expect("dnat_mark_guest firewall rule should exist");
        let mark_data = mark_rule.get::<FirewallRule>().unwrap();
        assert_eq!(mark_data.src, "vlan_guest");
        assert_eq!(mark_data.dest.as_deref(), Some("*"));
        assert_eq!(mark_data.target, FirewallTarget::MARK);
        assert_eq!(mark_data.set_mark.as_deref(), Some("0x80/0x80"));
        assert_eq!(mark_data.extra.as_deref(), Some("-m conntrack --ctstate DNAT"));
    }

    #[test]
    fn test_outbound_wan_no_routing() {
        let dir = tempfile::tempdir().unwrap();
        let ctx = TestContext(dir.path().to_path_buf());
        setup_configs(dir.path());

        let arena = Arena::new();
        let mut cfgs = parse_all(
            ctx.uci_root(),
            &arena,
            &["startwrt", "network", "firewall", "dhcp"],
        )
        .unwrap();

        // Set guest profile with WAN outbound
        let profile = Profile {
            id: ProfileIdOpt {
                fullname: Some("Guest".into()),
                interface: Some("guest".into()),
                vlan_tag: Some(101),
            },
            gateway_ip: Ipv4Addr::new(192, 168, 101, 1),
            outbound: "wan".into(),
            lan_access: LanAccess::SameProfile,
            wan_access: WanAccess::All,
            dns_override: Vec::new(),
            dns_source: String::new(),
            access_to_new_profiles: false,
            owns_lan: false,
        };

        set_config(ctx, &mut cfgs, &profile).unwrap();

        // No route or rule sections should exist
        let has_route = cfgs["network"]
            .sections
            .iter()
            .any(|s| s.name().as_deref() == Some("prt_guest"));
        let has_rule = cfgs["network"]
            .sections
            .iter()
            .any(|s| s.name().as_deref() == Some("prr_guest"));
        assert!(!has_route, "WAN outbound should not create route");
        assert!(!has_rule, "WAN outbound should not create rule");
    }

    #[test]
    fn test_outbound_change_cleans_up() {
        let dir = tempfile::tempdir().unwrap();
        let ctx = TestContext(dir.path().to_path_buf());
        setup_configs(dir.path());

        let arena = Arena::new();
        let mut cfgs = parse_all(
            ctx.uci_root(),
            &arena,
            &["startwrt", "network", "firewall", "dhcp"],
        )
        .unwrap();

        // First: set guest to VPN
        let profile_vpn = Profile {
            id: ProfileIdOpt {
                fullname: Some("Guest".into()),
                interface: Some("guest".into()),
                vlan_tag: Some(101),
            },
            gateway_ip: Ipv4Addr::new(192, 168, 101, 1),
            outbound: "wg_test".into(),
            lan_access: LanAccess::SameProfile,
            wan_access: WanAccess::All,
            dns_override: Vec::new(),
            dns_source: String::new(),
            access_to_new_profiles: false,
            owns_lan: false,
        };
        set_config(ctx.clone(), &mut cfgs, &profile_vpn).unwrap();

        // Verify route exists
        assert!(
            cfgs["network"]
                .sections
                .iter()
                .any(|s| s.name().as_deref() == Some("prt_guest")),
            "route should exist after VPN assignment"
        );

        // Now switch back to WAN
        let profile_wan = Profile {
            id: ProfileIdOpt {
                fullname: Some("Guest".into()),
                interface: Some("guest".into()),
                vlan_tag: Some(101),
            },
            gateway_ip: Ipv4Addr::new(192, 168, 101, 1),
            outbound: "wan".into(),
            lan_access: LanAccess::SameProfile,
            wan_access: WanAccess::All,
            dns_override: Vec::new(),
            dns_source: String::new(),
            access_to_new_profiles: false,
            owns_lan: false,
        };
        set_config(ctx, &mut cfgs, &profile_wan).unwrap();

        // Route and rule should be cleaned up
        assert!(
            !cfgs["network"]
                .sections
                .iter()
                .any(|s| s.name().as_deref() == Some("prt_guest")),
            "route should be removed after switching to WAN"
        );
        assert!(
            !cfgs["network"]
                .sections
                .iter()
                .any(|s| s.name().as_deref() == Some("prr_guest")),
            "rule should be removed after switching to WAN"
        );
    }

    #[test]
    fn test_outbound_vpn_adds_to_wan_zone() {
        let dir = tempfile::tempdir().unwrap();
        let ctx = TestContext(dir.path().to_path_buf());
        setup_configs(dir.path());

        let arena = Arena::new();
        let mut cfgs = parse_all(
            ctx.uci_root(),
            &arena,
            &["startwrt", "network", "firewall", "dhcp"],
        )
        .unwrap();

        let profile = Profile {
            id: ProfileIdOpt {
                fullname: Some("Guest".into()),
                interface: Some("guest".into()),
                vlan_tag: Some(101),
            },
            gateway_ip: Ipv4Addr::new(192, 168, 101, 1),
            outbound: "wg_mullvad".into(),
            lan_access: LanAccess::SameProfile,
            wan_access: WanAccess::All,
            dns_override: Vec::new(),
            dns_source: String::new(),
            access_to_new_profiles: false,
            owns_lan: false,
        };

        set_config(ctx, &mut cfgs, &profile).unwrap();

        // WAN zone should contain the VPN interface
        let wan_zone = cfgs["firewall"]
            .sections
            .iter()
            .filter_map(|s| s.get::<FirewallZone>().ok())
            .find(|z| z.name == DEFAULT_WAN_ZONE)
            .expect("WAN zone should exist");
        assert!(
            wan_zone.network.contains(&"wg_mullvad".to_string()),
            "VPN interface should be added to WAN zone, got: {:?}",
            wan_zone.network
        );
    }

    #[test]
    fn test_delete_profile_removes_routing() {
        let dir = tempfile::tempdir().unwrap();
        let ctx = TestContext(dir.path().to_path_buf());
        setup_configs(dir.path());

        // Set guest outbound to VPN first
        let arena = Arena::new();
        let mut cfgs = parse_all(
            ctx.uci_root(),
            &arena,
            &["startwrt", "network", "firewall", "dhcp"],
        )
        .unwrap();

        let profile = Profile {
            id: ProfileIdOpt {
                fullname: Some("Guest".into()),
                interface: Some("guest".into()),
                vlan_tag: Some(101),
            },
            gateway_ip: Ipv4Addr::new(192, 168, 101, 1),
            outbound: "wg_test".into(),
            lan_access: LanAccess::SameProfile,
            wan_access: WanAccess::All,
            dns_override: Vec::new(),
            dns_source: String::new(),
            access_to_new_profiles: false,
            owns_lan: false,
        };
        set_config(ctx.clone(), &mut cfgs, &profile).unwrap();
        dump_all(ctx.uci_root(), cfgs).unwrap();

        // Now delete the guest profile
        let arena2 = Arena::new();
        let mut cfgs2 = parse_all(
            ctx.uci_root(),
            &arena2,
            &["startwrt", "network", "firewall", "dhcp", "wireless"],
        )
        .unwrap();

        delete_config(
            ctx,
            &mut cfgs2,
            &ProfileIdOpt {
                fullname: None,
                interface: Some("guest".into()),
                vlan_tag: None,
            },
        )
        .unwrap();

        // Route and rule should be gone
        assert!(
            !cfgs2["network"]
                .sections
                .iter()
                .any(|s| s.name().as_deref() == Some("prt_guest")),
            "route should be removed on profile delete"
        );
        assert!(
            !cfgs2["network"]
                .sections
                .iter()
                .any(|s| s.name().as_deref() == Some("prr_guest")),
            "rule should be removed on profile delete"
        );
    }

    #[test]
    fn test_dns_override_creates_redirect() {
        let dir = tempfile::tempdir().unwrap();
        let ctx = TestContext(dir.path().to_path_buf());
        setup_configs(dir.path());

        let arena = Arena::new();
        let mut cfgs = parse_all(
            ctx.uci_root(),
            &arena,
            &["startwrt", "network", "firewall", "dhcp"],
        )
        .unwrap();

        let profile = Profile {
            id: ProfileIdOpt {
                fullname: Some("Guest".into()),
                interface: Some("guest".into()),
                vlan_tag: Some(101),
            },
            gateway_ip: Ipv4Addr::new(192, 168, 101, 1),
            outbound: "wan".into(),
            lan_access: LanAccess::SameProfile,
            wan_access: WanAccess::All,
            dns_override: vec![DnsServer { address: "1.1.1.1".into(), ssl: false }],
            dns_source: String::new(),
            access_to_new_profiles: false,
            owns_lan: false,
        };

        set_config(ctx, &mut cfgs, &profile).unwrap();

        let redirects: Vec<_> = cfgs["firewall"]
            .sections
            .iter()
            .filter_map(|s| s.get::<FirewallRedirect>().ok())
            .filter(|r| r.name.contains("DNS-Override") && r.src == "vlan_guest")
            .collect();
        assert_eq!(redirects.len(), 1, "should have 1 DNS override redirect");
        assert_eq!(redirects[0].dest.as_deref(), None);
        assert_eq!(redirects[0].dest_ip.as_deref(), Some("192.168.101.1"));
        assert_eq!(redirects[0].dest_port.as_deref(), Some("53"));
        assert_eq!(redirects[0].src_dport.as_deref(), Some("53"));
        assert_eq!(redirects[0].target, "DNAT");
        assert_eq!(redirects[0].proto, vec!["tcp", "udp"]);
    }

    #[test]
    fn test_dns_override_round_trip() {
        let dir = tempfile::tempdir().unwrap();
        let ctx = TestContext(dir.path().to_path_buf());
        setup_configs(dir.path());

        let arena = Arena::new();
        let mut cfgs = parse_all(
            ctx.uci_root(),
            &arena,
            &["startwrt", "network", "firewall", "dhcp"],
        )
        .unwrap();

        let profile = Profile {
            id: ProfileIdOpt {
                fullname: Some("Guest".into()),
                interface: Some("guest".into()),
                vlan_tag: Some(101),
            },
            gateway_ip: Ipv4Addr::new(192, 168, 101, 1),
            outbound: "wan".into(),
            lan_access: LanAccess::SameProfile,
            wan_access: WanAccess::All,
            dns_override: vec![
                DnsServer { address: "1.1.1.1".into(), ssl: false },
                DnsServer { address: "8.8.8.8".into(), ssl: false },
            ],
            dns_source: String::new(),
            access_to_new_profiles: false,
            owns_lan: false,
        };

        set_config(ctx.clone(), &mut cfgs, &profile).unwrap();
        dump_all(ctx.uci_root(), cfgs).unwrap();

        // Re-parse and read back via get_config
        let arena2 = Arena::new();
        let cfgs2 = parse_all(
            ctx.uci_root(),
            &arena2,
            &["startwrt", "network", "firewall"],
        )
        .unwrap();

        let got = get_config(
            ctx,
            &cfgs2,
            ProfileIdOpt {
                fullname: None,
                interface: Some("guest".into()),
                vlan_tag: None,
            },
        )
        .unwrap();

        assert_eq!(
            got.dns_override,
            vec![
                DnsServer { address: "1.1.1.1".into(), ssl: false },
                DnsServer { address: "8.8.8.8".into(), ssl: false },
            ],
            "dns_override should round-trip through UCI"
        );
    }

    #[test]
    fn test_dns_override_empty_no_redirect() {
        let dir = tempfile::tempdir().unwrap();
        let ctx = TestContext(dir.path().to_path_buf());
        setup_configs(dir.path());

        let arena = Arena::new();
        let mut cfgs = parse_all(
            ctx.uci_root(),
            &arena,
            &["startwrt", "network", "firewall", "dhcp"],
        )
        .unwrap();

        let profile = Profile {
            id: ProfileIdOpt {
                fullname: Some("Guest".into()),
                interface: Some("guest".into()),
                vlan_tag: Some(101),
            },
            gateway_ip: Ipv4Addr::new(192, 168, 101, 1),
            outbound: "wan".into(),
            lan_access: LanAccess::SameProfile,
            wan_access: WanAccess::All,
            dns_override: Vec::new(),
            dns_source: String::new(),
            access_to_new_profiles: false,
            owns_lan: false,
        };

        set_config(ctx, &mut cfgs, &profile).unwrap();

        let redirect_count = cfgs["firewall"]
            .sections
            .iter()
            .filter(|s| {
                s.get::<FirewallRedirect>()
                    .map(|r| r.name.contains("DNS-Override"))
                    .unwrap_or(false)
            })
            .count();
        assert_eq!(
            redirect_count, 0,
            "empty dns_override should not create any redirect sections"
        );
    }

    #[test]
    fn test_delete_removes_vpn_server_resources() {
        let dir = tempfile::tempdir().unwrap();
        let ctx = TestContext(dir.path().to_path_buf());

        use crate::wg::{Base64, WgKey};
        let server_key = Base64::new(WgKey::generate()).to_base64();
        let peer_key = Base64::new(WgKey::generate()).to_base64();

        // Write configs with VPN server resources for Guest profile
        std::fs::write(
            dir.path().join("startwrt"),
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
",
        )
        .unwrap();

        std::fs::write(
            dir.path().join("network"),
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

config bridge-vlan
\toption device 'br-lan'
\toption vlan '99'

config bridge-vlan
\toption device 'br-lan'
\toption vlan '101'

config interface 'wg_guest'
\toption proto 'wireguard'
\toption private_key '{server_key}'
\toption listen_port '51820'
\tlist addresses '192.168.101.254/32'

config wireguard_wg_guest 'wg_guest_0'
\toption public_key '{peer_key}'
\toption description 'Phone'
\toption persistent_keepalive '25'
\toption route_allowed_ips '1'
\tlist allowed_ips '192.168.101.200/32'
"
            ),
        )
        .unwrap();

        std::fs::write(
            dir.path().join("firewall"),
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

config rule
\toption name 'Allow-DHCP-DNS-Guest'
\toption src 'vlan_guest'
\toption dest_port '53 67 68'
\tlist proto 'tcp'
\tlist proto 'udp'
\tlist proto 'icmp'
\toption target 'ACCEPT'

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
            dir.path().join("dhcp"),
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

        std::fs::write(
            dir.path().join("wireless"),
            "\
config wifi-device 'radio0'
\toption type 'mac80211'
\toption band '2g'
\toption channel '1'

config wifi-iface 'default_radio0'
\toption device 'radio0'
\toption mode 'ap'
\toption ssid 'TestNet'
\toption encryption 'psk2'
\toption key 'adminpass1'
\toption dynamic_vlan '1'
",
        )
        .unwrap();

        let arena = Arena::new();
        let mut cfgs = parse_all(
            ctx.uci_root(),
            &arena,
            &["startwrt", "network", "firewall", "dhcp", "wireless"],
        )
        .unwrap();

        // Delete Guest profile
        delete_config(
            ctx,
            &mut cfgs,
            &ProfileIdOpt {
                fullname: None,
                interface: Some("guest".into()),
                vlan_tag: None,
            },
        )
        .unwrap();

        // 1. No vpn_server section with interface=wg_guest in startwrt
        let has_vpn_meta = cfgs["startwrt"]
            .sections
            .iter()
            .any(|s| s.ty() == "vpn_server");
        assert!(!has_vpn_meta, "vpn_server metadata should be removed");

        // 2. No wg_guest interface section in network
        let has_wg_iface = cfgs["network"]
            .sections
            .iter()
            .any(|s| s.name().as_deref() == Some("wg_guest"));
        assert!(!has_wg_iface, "wg_guest interface should be removed");

        // 3. No wireguard_wg_guest peer sections in network
        let has_peers = cfgs["network"]
            .sections
            .iter()
            .any(|s| s.ty() == "wireguard_wg_guest");
        assert!(!has_peers, "wireguard peer sections should be removed");

        // 4. No allow_wireguard_wg_guest rule in firewall (the specific bug that was fixed)
        let has_wg_rule = cfgs["firewall"]
            .sections
            .iter()
            .filter_map(|s| s.get::<FirewallRule>().ok())
            .any(|r| r.name == "Allow-WireGuard-wg_guest");
        assert!(
            !has_wg_rule,
            "WireGuard WAN firewall rule should be removed on profile deletion"
        );

        // 5. Admin profile resources untouched
        let admin_exists = cfgs["startwrt"]
            .sections
            .iter()
            .any(|s| s.ty() == "profile");
        assert!(admin_exists, "Admin profile should survive");

        let admin_iface = cfgs["network"]
            .sections
            .iter()
            .any(|s| s.name().as_deref() == Some("lan"));
        assert!(admin_iface, "Admin network interface should survive");
    }

    #[test]
    fn test_collect_smartdns_groups_with_profile_dns() {
        let dir = tempfile::tempdir().unwrap();
        // Create startwrt config with a profile that has dns_override
        std::fs::write(
            dir.path().join("startwrt"),
            "\
config profile lan
\toption fullname 'Admin'
\toption interface 'lan'
\toption vlan_tag '1'

config profile guest
\toption fullname 'Guest'
\toption interface 'guest'
\toption vlan_tag '101'
\tlist dns_override '{\"address\":\"1.1.1.1\",\"ssl\":false}'
\tlist dns_override '{\"address\":\"8.8.8.8\",\"ssl\":true}'

config system_dns system_dns
\tlist servers '{\"address\":\"9.9.9.9\",\"ssl\":false}'
",
        )
        .unwrap();

        let arena = Arena::new();
        let cfgs = parse_all(dir.path(), &arena, &["startwrt"]).unwrap();
        let groups = dns::collect_smartdns_groups(&cfgs);

        // Should have both system and profile groups
        assert_eq!(groups.len(), 2);
        assert_eq!(groups[0].name, "system");
        assert_eq!(groups[0].port, dns::SMARTDNS_SYSTEM_PORT);
        assert_eq!(groups[1].name, "profile_guest");
        assert_eq!(groups[1].port, dns::smartdns_port_for_vlan(101));
        assert_eq!(groups[1].servers.len(), 2);
    }

    #[test]
    fn test_collect_smartdns_groups_no_profile_dns() {
        let dir = tempfile::tempdir().unwrap();
        // Profile without dns_override should not produce a SmartDNS group
        std::fs::write(
            dir.path().join("startwrt"),
            "\
config profile lan
\toption fullname 'Admin'
\toption interface 'lan'
\toption vlan_tag '1'
",
        )
        .unwrap();

        let arena = Arena::new();
        let cfgs = parse_all(dir.path(), &arena, &["startwrt"]).unwrap();
        let groups = dns::collect_smartdns_groups(&cfgs);

        assert!(groups.is_empty(), "No groups when no system or profile DNS");
    }

    #[test]
    fn test_rewrite_all_dns_forwarding_creates_dnsmasq_for_system_dns() {
        let dir = tempfile::tempdir().unwrap();
        std::fs::write(
            dir.path().join("startwrt"),
            "\
config profile lan
\toption fullname 'Admin'
\toption interface 'lan'
\toption vlan_tag '1'

config profile guest
\toption fullname 'Guest'
\toption interface 'guest'
\toption vlan_tag '101'

config system_dns system_dns
\tlist servers '{\"address\":\"1.1.1.1\",\"ssl\":false}'
",
        )
        .unwrap();

        std::fs::write(
            dir.path().join("network"),
            "\
config interface 'lan'
\toption proto 'static'
\toption ipaddr '192.168.1.1'
\toption device 'br-lan.1'
\toption netmask '255.255.255.0'

config interface 'guest'
\toption proto 'static'
\toption ipaddr '192.168.101.1'
\toption device 'br-lan.101'
\toption netmask '255.255.255.0'
",
        )
        .unwrap();

        std::fs::write(
            dir.path().join("dhcp"),
            "\
config dnsmasq
\toption domainneeded '1'
",
        )
        .unwrap();

        std::fs::write(
            dir.path().join("firewall"),
            "\
config zone
\toption name 'wan'
\tlist network 'wan'
\toption input 'REJECT'
\toption output 'ACCEPT'
\toption forward 'REJECT'

config zone
\toption name 'lan'
\tlist network 'lan'
\toption input 'ACCEPT'
\toption output 'ACCEPT'
\toption forward 'ACCEPT'

config zone
\toption name 'vlan_guest'
\tlist network 'guest'
\toption input 'ACCEPT'
\toption output 'ACCEPT'
\toption forward 'ACCEPT'
",
        )
        .unwrap();

        let arena = Arena::new();
        let mut cfgs =
            parse_all(dir.path(), &arena, &["startwrt", "network", "dhcp", "firewall"]).unwrap();
        rewrite_all_dns_forwarding(&mut cfgs).unwrap();

        // Both profiles should get per-profile dnsmasq sections pointing to SmartDNS
        let dns_lan = cfgs["dhcp"]
            .sections
            .iter()
            .any(|s| s.name().as_deref() == Some("dns_lan"));
        let dns_guest = cfgs["dhcp"]
            .sections
            .iter()
            .any(|s| s.name().as_deref() == Some("dns_guest"));
        assert!(dns_lan, "Admin profile should get per-profile dnsmasq");
        assert!(dns_guest, "Guest profile should get per-profile dnsmasq");
    }
}