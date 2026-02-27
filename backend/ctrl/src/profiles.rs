use crate::ethernet::DEFAULT_LAN_BRIDGE;
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
use std::process::{Command, Stdio};
use uciedit::openwrt::{
    DeviceType, Dhcp, FirewallForwarding, FirewallRule, FirewallTarget, FirewallZone,
    InterfaceProto, NetworkBridgeVlan, NetworkDevice, NetworkInterface, NetworkVlanPort,
    NetworkVlanPortTagging, WifiStation, WifiVlan,
};
use uciedit::{dump_all, parse_all, Arena, Configs, TypedSection};

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

#[derive(Debug, Serialize, Deserialize)]
pub struct Profile<Id: Ord = ProfileId> {
    #[serde(flatten)]
    pub id: Id,
    pub gateway_ip: Ipv4Addr,
    pub outbound: String,             // 'wan' for default WAN, or VPN interface name. TODO: Implement routing logic
    pub lan_access: LanAccess<Id>,    // TODO
    pub wan_access: WanAccess,        // TODO
    pub access_to_new_profiles: bool, // TODO
    pub owns_lan: bool,
}

pub fn profiles<C: CtrlContext>() -> ParentHandler<C> {
    ParentHandler::new()
        .subcommand("get", from_fn(get::<C>).with_display_serializable())
        .subcommand("set", from_fn(set::<C>).with_display_serializable())
        .subcommand("delete", from_fn(delete::<C>).no_display())
        .subcommand("list", from_fn(list::<C>).with_display_serializable())
        .subcommand("create", from_fn(create::<C>).with_display_serializable())
        .subcommand("edit", from_fn(edit::<C>).with_display_serializable())
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
struct UciProfile {
    pub fullname: String,
    pub interface: String,
    pub vlan_tag: u16,
    pub outbound: Option<String>,
    #[uci(default_value = "false")]
    pub access_to_new_profiles: bool,
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
    let mut forwarding = BTreeSet::new();
    cfgs["firewall"].each::<FirewallForwarding, Error>(|_, FirewallForwarding { src, dest }| {
        if src == this_zone_name && src != dest {
            forwarding.insert(dest);
        }
    })?;
    if forwarding.contains(DEFAULT_WAN_ZONE) {
        wip_profile.wan_access = WanAccess::All;
    } else {
        wip_profile.wan_access = WanAccess::None;
    }
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
    } else if (other_profiles.len() + 1) >= lookup.list().len()
        && wip_profile.access_to_new_profiles
    {
        wip_profile.lan_access = LanAccess::All;
    } else {
        wip_profile.lan_access = LanAccess::OtherProfiles(other_profiles);
    }
    Ok(wip_profile)
}

pub fn delete<C: CtrlContext>(ctx: C, id: ProfileIdOpt) -> Result<(), Error> {
    let mut retries = 4;
    loop {
        let arena = Arena::new();
        let mut cfgs = parse_all(
            ctx.uci_root(),
            &arena,
            &["startwrt", "network", "firewall", "dhcp", "wireless"],
        )?;
        delete_config(ctx.clone(), &mut cfgs, &id)?;
        match dump_all(ctx.uci_root(), cfgs) {
            Err(uciedit::Error::Conflict { .. }) if retries > 0 => {
                retries -= 1;
                continue;
            }
            Err(err) => return Err(err.into()),
            Ok(()) => {
                if ctx.effectful() {
                    reload_system()?;
                    restart_wifi();
                }
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

    // Remove profile from startwrt config
    cfgs["startwrt"].sections.retain(|section| {
        let Ok(profile) = section.get::<UciProfile>() else {
            return true;
        };
        profile.id() != id
    });

    // Remove network interface and bridge-vlan for this VLAN tag
    cfgs["network"].sections.retain(|section| {
        if section.get::<NetworkInterface>().is_ok() {
            if section.name().as_deref() == Some(&id.interface) {
                return false;
            }
        }
        if let Ok(vlan) = section.get::<NetworkBridgeVlan>() {
            if vlan.vlan == id.vlan_tag {
                return false;
            }
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

    Ok(())
}

pub fn list<C: CtrlContext>(ctx: C) -> Result<Vec<ProfileId>, Error> {
    let arena = Arena::new();
    let cfgs = parse_all(ctx.uci_root(), &arena, &["startwrt"])?;
    list_config(ctx, &cfgs)
}

fn list_config(_ctx: impl CtrlContext, cfgs: &Configs) -> Result<Vec<ProfileId>, Error> {
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
    std::thread::spawn(|| {
        let _ = Command::new("/etc/init.d/network")
            .arg("reload")
            .spawn()
            .and_then(|mut c| c.wait());
        let _ = Command::new("/etc/init.d/firewall")
            .arg("reload")
            .spawn()
            .and_then(|mut c| c.wait());
        let _ = Command::new("/etc/init.d/dnsmasq")
            .arg("reload")
            .spawn()
            .and_then(|mut c| c.wait());
    });
    Ok(())
}

/// Full wifi restart so hostapd re-reads wpa_psk_file and vlan_file.
/// Only needed when profile VLANs are created or deleted — not on every profile update.
pub fn restart_wifi() {
    std::thread::spawn(|| {
        let _ = Command::new("wifi")
            .spawn()
            .and_then(|mut c| c.wait());
    });
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
    DeserializeStdin(profile): DeserializeStdin<Profile<ProfileIdOpt>>,
) -> Result<ProfileId, Error> {
    let mut retries = 4;
    loop {
        let arena = Arena::new();
        let mut cfgs = parse_all(
            ctx.uci_root(),
            &arena,
            &["startwrt", "network", "firewall", "dhcp"],
        )?;
        let out = set_config(ctx.clone(), &mut cfgs, &profile)?;
        match dump_all(ctx.uci_root(), cfgs) {
            Err(uciedit::Error::Conflict { .. }) if retries > 0 => {
                retries -= 1;
                continue;
            }
            Err(err) => return Err(err.into()),
            Ok(()) => {
                if ctx.effectful() {
                    reload_system()?;
                }
                return Ok(out);
            }
        }
    }
}

fn set_config<C: CtrlContext>(
    ctx: C,
    cfgs: &mut Configs,
    profile: &Profile<ProfileIdOpt>,
) -> Result<ProfileId, Error> {
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
                existing_profile.access_to_new_profiles = profile.access_to_new_profiles;
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
        access_to_new_profiles: profile.access_to_new_profiles,
        owns_lan: profile.owns_lan,
    };
    let mut all_interfaces = BTreeSet::<String>::new();
    let mut found_bridge = None;
    let mut found_interface = false;
    for section in &mut cfgs["network"].sections {
        if let Some(mut iface) = section.get_typed::<NetworkInterface>()? {
            let Some(name) = section.name() else { continue };
            if iface.proto == InterfaceProto::STATIC {
                all_interfaces.insert(name.to_string());
            }
            if name == profile.id.interface {
                iface.proto = InterfaceProto::STATIC;
                iface.ipaddr = Some(profile.gateway_ip);
                iface.netmask = Some(Ipv4Addr::new(255, 255, 255, 0));
                found_bridge = Some(
                    iface
                        .device
                        .rsplit_once('.')
                        .map(|(bridge, _)| bridge.to_string())
                        .unwrap_or_else(|| iface.device.clone()),
                );
                section.set(&iface)?;
                found_interface = true;
            }
        }
        if let Some(dev) = section.get_typed::<NetworkDevice>()? {
            if dev.ty == Some(DeviceType::BRIDGE)
                && (found_bridge.is_none() || dev.name == DEFAULT_LAN_BRIDGE)
            {
                found_bridge = Some(dev.name.to_string());
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
    Ok(profile.id)
}

pub fn create<C: CtrlContext>(
    ctx: C,
    DeserializeStdin(profile): DeserializeStdin<Profile<ProfileIdOpt>>,
) -> Result<ProfileId, Error> {
    let mut retries = 4;
    loop {
        let arena = Arena::new();
        let mut cfgs = parse_all(
            ctx.uci_root(),
            &arena,
            &["startwrt", "network", "firewall", "dhcp", "wireless"],
        )?;
        let out = create_config(ctx.clone(), &mut cfgs, &profile)?;
        match dump_all(ctx.uci_root(), cfgs) {
            Err(uciedit::Error::Conflict { .. }) if retries > 0 => {
                retries -= 1;
                continue;
            }
            Err(err) => return Err(err.into()),
            Ok(()) => {
                if ctx.effectful() {
                    reload_system()?;
                    restart_wifi();
                }
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
    let mut all_interfaces = BTreeSet::<String>::new();
    let mut existing_tags = BTreeSet::new();
    let mut found_bridge = None;
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
            NetworkDevice::TY => {
                if let Ok(dev) = section.get::<NetworkDevice>() {
                    if dev.ty == Some(DeviceType::BRIDGE)
                        && (found_bridge.is_none() || dev.name == DEFAULT_LAN_BRIDGE)
                    {
                        found_bridge = Some(dev.name);
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
    let found_bridge = found_bridge.ok_or(ErrorKind::MissingLanBridge)?;
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
        access_to_new_profiles: profile.access_to_new_profiles,
        owns_lan: profile.owns_lan,
    };
    rewrite_firewall(&ctx, cfgs, &profile, &all_interfaces, &wants_access, true)?;
    rewrite_dhcp(&ctx, cfgs, &profile)?;
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
        WanAccess::Whitelist(_destinations) => {
            // TODO: Implement proper whitelist firewall rules
            // For now, add basic WAN forwarding (behaves like All)
            cfgs["firewall"].append(
                &FirewallForwarding {
                    src: this_zone_name.clone(),
                    dest: DEFAULT_WAN_ZONE.into(),
                },
                None,
            )?;
        }
        WanAccess::Blacklist(_destinations) => {
            // TODO: Implement proper blacklist firewall rules
            // For now, add basic WAN forwarding (behaves like All)
            cfgs["firewall"].append(
                &FirewallForwarding {
                    src: this_zone_name.clone(),
                    dest: DEFAULT_WAN_ZONE.into(),
                },
                None,
            )?;
        }
    }

    Ok(())
}

pub fn rewrite_dhcp(
    _ctx: &impl CtrlContext,
    cfgs: &mut Configs,
    profile: &Profile,
) -> Result<(), Error> {
    let mut found_dhcp = false;
    for section in &cfgs["dhcp"].sections {
        let Ok(dhcp) = section.get::<Dhcp>() else {
            continue;
        };
        if dhcp.interface == profile.id.interface {
            found_dhcp = true;
        }
    }
    if !found_dhcp {
        cfgs["dhcp"].append(
            &Dhcp {
                interface: profile.id.interface.clone(),
                start: 100, // default in the openwrt docs
                limit: 150,
                leasetime: "12h".into(),
            },
            Some(&profile.id.interface),
        )?;
    }
    Ok(())
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
        },
        Some("lan"),
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
            access_to_new_profiles: current_profile.access_to_new_profiles,
            owns_lan: current_profile.owns_lan,
        };
        let modified_profile = crate::utils::edit_in_editor(&current_profile)?;
        set(ctx, DeserializeStdin(modified_profile))
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
}