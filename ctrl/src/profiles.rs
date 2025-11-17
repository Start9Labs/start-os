use crate::ethernet::{self, DEFAULT_LAN_BRIDGE};
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
use uciedit::openwrt::NetworkVlanPortTagging::PRIMARY;
use uciedit::openwrt::{
    DeviceType, Dhcp, FirewallForwarding, FirewallRule, FirewallTarget, FirewallZone,
    InterfaceProto, NetworkBridgeVlan, NetworkDevice, NetworkInterface,
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
}

#[derive(Debug, Serialize, Deserialize)]
pub struct Profile<Id: Ord = ProfileId> {
    #[serde(flatten)]
    pub id: Id,
    pub gateway_ip: Ipv4Addr,
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
        lan_access: LanAccess::SameProfile,
        wan_access: WanAccess::None,
        access_to_new_profiles: match uciprofile {
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
    todo!();
    if ctx.effectful() {
        reload_system()?;
    }
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
    let _ = Command::new("/etc/init.d/network")
        .arg("reload")
        .spawn()?
        .wait();
    let _ = Command::new("/etc/init.d/firewall")
        .arg("reload")
        .spawn()?
        .wait();
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
    let mut found_vlan = false;
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
                        .ok_or_eyre("profile interface is not bound to a vlan device")?
                        .0
                        .to_string(),
                );
                section.set(&iface)?;
                found_interface = true;
            }
        }
        if let Some(dev) = section.get_typed::<NetworkDevice>()? {
            if dev.ty == Some(DeviceType::BRIDGE) && found_bridge.is_none() {
                found_bridge = Some(dev.name.to_string());
            }
        }
        if let Some(vlan) = section.get_typed::<NetworkBridgeVlan>()? {
            if vlan.vlan == profile.id.vlan_tag {
                found_vlan = true;
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
    if !found_vlan {
        let found_bridge = found_bridge.ok_or(ErrorKind::MissingLanBridge)?;
        cfgs["network"].append(
            &NetworkBridgeVlan {
                device: found_bridge,
                vlan: profile.id.vlan_tag,
                ports: Vec::new(),
            },
            None,
        )?;
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
            &["startwrt", "network", "firewall", "dhcp"],
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
    let mut ports = Vec::new();
    let interface = if profile.owns_lan {
        if Lookup::parse(ctx.clone(), cfgs)?.lan_owner.is_some() {
            return Err(ErrorKind::LanOwnerExists.into());
        }
        let ethernet = ethernet::get(ctx.clone())?;
        // by default connect the profile to all ethernet ports
        ports.extend(
            ethernet
                .ports
                .iter()
                .filter(|(id, port)| {
                    Some(id.as_str()) != ethernet.wan_port.as_deref() && port.profile.is_none()
                })
                .map(|(id, _)| uciedit::openwrt::NetworkVlanPort {
                    port: id.clone(),
                    tagging: Some(PRIMARY),
                }),
        );
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
    }
    cfgs["network"].append(
        &NetworkBridgeVlan {
            device: found_bridge,
            vlan: vlan_tag,
            ports,
        },
        None,
    )?;
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
        if ip_out.contains("can't find device") {
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
