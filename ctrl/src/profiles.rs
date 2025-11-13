use crate::ethernet::{self, DEFAULT_LAN_BRIDGE};
use crate::utils::DeserializeStdin;
use crate::CtrlContext;
use crate::{utils::HandlerExtSerde, Error, ErrorKind};
use clap::Parser;
use color_eyre::eyre::{eyre, OptionExt};
use rpc_toolkit::{from_fn, Context, HandlerExt as _, ParentHandler};
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
use uciedit::UciSection;
use uciedit::{parse_config, rewrite_config};

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

#[derive(Debug, Serialize, Deserialize)]
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

#[derive(Debug, UciSection)]
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
    let lookup = Lookup::parse(ctx.clone())?;
    let id = lookup.resolve(&query)?;
    let mut uciprofile = None;
    parse_config(ctx.uci_path("startwrt"), |mut cfg| {
        cfg.each(|_, profile: UciProfile| {
            if &profile.id() == id {
                uciprofile = Some(profile);
            }
        })?;
        Ok::<_, Error>(())
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
    parse_config(ctx.uci_path("network"), |mut cfg| {
        while cfg.step() {
            if let Ok(iface) = cfg.get::<NetworkInterface>() {
                let Some(name) = cfg.name() else { continue };
                if name == id.interface {
                    if iface.proto != InterfaceProto::STATIC {
                        return Err(ErrorKind::CorruptedProfile { id: query.clone() }.into());
                    }
                    if let Some(ip) = iface.ipaddr {
                        wip_profile.gateway_ip = ip;
                    } else {
                        return Err(ErrorKind::CorruptedProfile { id: query.clone() }.into());
                    }
                    return Ok::<_, Error>(());
                }
            }
        }
        Err(ErrorKind::CorruptedProfile { id: query.clone() }.into())
    })?;
    parse_config(ctx.uci_path("firewall"), |mut cfg| {
        let mut this_zone_name = format!("vlan_{}", id.interface);
        cfg.each(|_, zone: FirewallZone| {
            for zone_interface in zone.network {
                if id.interface == zone_interface {
                    this_zone_name = zone.name.clone();
                }
            }
        })?;
        let mut forwarding = BTreeSet::new();
        cfg.each(|_, FirewallForwarding { src, dest }| {
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
        cfg.each(|_, FirewallZone { name, network, .. }| {
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
        Ok::<_, Error>(())
    })?;
    Ok(wip_profile)
}

pub fn delete<C: CtrlContext>(ctx: C, id: ProfileIdOpt) -> Result<(), Error> {
    todo!();
    reload_system()?;
    Ok(())
}

pub fn list<C: CtrlContext>(ctx: C) -> Result<Vec<ProfileId>, Error> {
    parse_config(ctx.uci_path("startwrt"), |mut cfg| {
        let mut found = Vec::new();
        while cfg.step() {
            let Ok(UciProfile {
                fullname,
                interface,
                vlan_tag,
                ..
            }) = cfg.get()
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
    })
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
    rewrite_config(ctx.uci_path("startwrt"), |mut cfg| {
        while cfg.step() {
            if let Some(mut existing_profile) = cfg.get_typed::<UciProfile>()? {
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
                    cfg.set(&existing_profile)?;
                }
            }
        }
        Ok::<_, Error>(())
    })?;
    // only do lookup after potentially handling renames
    let lookup = Lookup::parse(ctx.clone())?;
    let profile = Profile {
        id: lookup.resolve(&profile.id)?.clone(),
        gateway_ip: profile.gateway_ip,
        lan_access: match profile.lan_access {
            LanAccess::All => LanAccess::All,
            LanAccess::SameProfile => LanAccess::SameProfile,
            LanAccess::OtherProfiles(set) => LanAccess::OtherProfiles(
                set.into_iter()
                    .map(|p| lookup.resolve(&p).cloned())
                    .collect::<Result<_, _>>()?,
            ),
        },
        wan_access: profile.wan_access,
        access_to_new_profiles: profile.access_to_new_profiles,
        owns_lan: profile.owns_lan,
    };
    let mut all_interfaces = BTreeSet::<String>::new();
    rewrite_config(ctx.uci_path("network"), |mut cfg| {
        let mut found_bridge = None;
        let mut found_vlan = false;
        let mut found_interface = false;
        while cfg.step() {
            if let Some(mut iface) = cfg.get_typed::<NetworkInterface>()? {
                let Some(name) = cfg.name() else { continue };
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
                    cfg.set(&iface)?;
                    found_interface = true;
                }
            }
            if let Some(dev) = cfg.get_typed::<NetworkDevice>()? {
                if dev.ty == Some(DeviceType::BRIDGE) && found_bridge.is_none() {
                    found_bridge = Some(dev.name.to_string());
                }
            }
            if let Some(vlan) = cfg.get_typed::<NetworkBridgeVlan>()? {
                if vlan.vlan == profile.id.vlan_tag {
                    found_vlan = true;
                }
            }
        }
        if !found_interface {
            let found_bridge = found_bridge.clone().ok_or(ErrorKind::MissingLanBridge)?;
            cfg.push(
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
            cfg.push(
                &NetworkBridgeVlan {
                    device: found_bridge,
                    vlan: profile.id.vlan_tag,
                    ports: Vec::new(),
                },
                None,
            )?;
        }
        Ok::<_, Error>(())
    })?;
    rewrite_firewall(&ctx, &profile, &all_interfaces, &[], false)?;
    rewrite_dhcp(&ctx, &profile)?;
    reload_system()?;
    Ok(profile.id)
}

pub fn create<C: CtrlContext>(
    ctx: C,
    DeserializeStdin(profile): DeserializeStdin<Profile<ProfileIdOpt>>,
) -> Result<ProfileId, Error> {
    let mut ports = Vec::new();
    let interface = if profile.owns_lan {
        if Lookup::parse(ctx.clone())?.lan_owner.is_some() {
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
    // FIXME: feature to stage both config rewrites until we are sure about them
    let vlan_tag = rewrite_config(ctx.uci_path("network"), |mut cfg| {
        let mut existing_tags = BTreeSet::new();
        let mut found_bridge = None;
        while cfg.step() {
            match &*cfg.ty() {
                NetworkInterface::TY => {
                    let Some(name) = cfg.name() else { continue };
                    if name == interface && !profile.owns_lan {
                        return Err(ErrorKind::InterfaceNameConflict {
                            name: interface.clone(),
                        }
                        .into());
                    }
                    if let Ok(iface) = cfg.get::<NetworkInterface>() {
                        if iface.proto == InterfaceProto::STATIC {
                            all_interfaces.insert(name.to_string());
                        }
                    }
                }
                NetworkDevice::TY => {
                    if let Ok(dev) = cfg.get::<NetworkDevice>() {
                        if dev.ty == Some(DeviceType::BRIDGE)
                            && (found_bridge.is_none() || dev.name == DEFAULT_LAN_BRIDGE)
                        {
                            found_bridge = Some(dev.name);
                        }
                    }
                }
                NetworkBridgeVlan::TY => {
                    if let Ok(vlan) = cfg.get::<NetworkBridgeVlan>() {
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
            cfg.restart();
            while cfg.step() {
                if let Ok(mut iface) = cfg.get::<NetworkInterface>() {
                    if cfg.name().as_deref() != Some("lan") {
                        continue;
                    }
                    found_lan_interface = true;
                    iface.device = format!("{found_bridge}.{vlan_tag}");
                    cfg.set(&iface)?;
                }
            }
            if !found_lan_interface {
                return Err(ErrorKind::MissingLanInterface.into());
            }
        } else {
            cfg.push(
                &NetworkInterface {
                    device: format!("{found_bridge}.{vlan_tag}"),
                    proto: InterfaceProto::STATIC,
                    ipaddr: Some(profile.gateway_ip),
                    netmask: Some(Ipv4Addr::new(255, 255, 255, 0)),
                },
                Some(&interface),
            )?;
        }
        cfg.push(
            &NetworkBridgeVlan {
                device: found_bridge,
                vlan: vlan_tag,
                ports,
            },
            None,
        )?;
        Ok::<_, Error>(vlan_tag)
    })?;
    let mut wants_access = Vec::new();
    rewrite_config(ctx.uci_path("startwrt"), |mut cfg| {
        while cfg.step() {
            if let Some(other_profile) = cfg.get_typed::<UciProfile>()? {
                if other_profile.access_to_new_profiles {
                    wants_access.push(other_profile.id());
                }
            }
        }
        cfg.push(
            &UciProfile {
                fullname: fullname.clone(),
                interface: interface.clone(),
                vlan_tag,
                access_to_new_profiles: profile.access_to_new_profiles,
            },
            Some(&interface),
        )?;
        Ok::<_, Error>(())
    })?;
    let lookup = Lookup::parse(ctx.clone())?; // only lookup after pushing, in case there is a self-reference
    let profile = Profile {
        id: ProfileId {
            fullname,
            interface,
            vlan_tag,
        },
        gateway_ip: profile.gateway_ip,
        lan_access: match profile.lan_access {
            LanAccess::All => LanAccess::All,
            LanAccess::SameProfile => LanAccess::SameProfile,
            LanAccess::OtherProfiles(set) => LanAccess::OtherProfiles(
                set.into_iter()
                    .map(|p| lookup.resolve(&p).cloned())
                    .collect::<Result<_, _>>()?,
            ),
        },
        wan_access: profile.wan_access,
        access_to_new_profiles: profile.access_to_new_profiles,
        owns_lan: profile.owns_lan,
    };
    rewrite_firewall(&ctx, &profile, &all_interfaces, &wants_access, true)?;
    rewrite_dhcp(&ctx, &profile)?;
    reload_system()?;
    Ok(profile.id)
}

fn rewrite_firewall(
    ctx: &impl CtrlContext,
    profile: &Profile,
    all_interfaces: &BTreeSet<String>,
    wants_access: &[ProfileId],
    remake_zone: bool,
) -> Result<(), Error> {
    rewrite_config(ctx.uci_path("firewall"), |mut cfg| {
        let mut found_wan = false;
        let mut this_zone_name = format!("vlan_{}", profile.id.interface);
        let mut all_zones = BTreeMap::new();
        while cfg.step() {
            let Ok(zone) = cfg.get::<FirewallZone>() else {
                continue;
            };
            if zone.name == DEFAULT_WAN_ZONE {
                found_wan = true;
            } else if zone.name == this_zone_name {
                if remake_zone {
                    cfg.remove();
                }
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
        let mut found_dhcp_dns_rule = false;
        while cfg.step() {
            let Ok(rule) = cfg.get::<FirewallRule>() else {
                continue;
            };
            if rule.src == this_zone_name && rule.name.contains("DHCP") && rule.name.contains("DNS")
            {
                if remake_zone {
                    cfg.remove();
                } else {
                    found_dhcp_dns_rule = true;
                }
            }
        }
        cfg.restart();
        while cfg.step() {
            let Ok(fwd) = cfg.get::<FirewallForwarding>() else {
                continue;
            };
            if fwd.src == this_zone_name {
                cfg.remove();
            }
        }
        if !found_wan {
            return Err(ErrorKind::MissingWanInterface.into());
        }
        if remake_zone {
            cfg.push(
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
        if !found_dhcp_dns_rule {
            cfg.push(
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
        match &profile.lan_access {
            LanAccess::All => {
                for other_zone in all_zones.values() {
                    if other_zone != &this_zone_name {
                        cfg.push(
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
                        Some(other_zone) => cfg.push(
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
                Some(other_zone) => cfg.push(
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
        match &profile.wan_access {
            WanAccess::All => cfg.push(
                &FirewallForwarding {
                    src: this_zone_name.clone(),
                    dest: DEFAULT_WAN_ZONE.into(),
                },
                None,
            )?,
            WanAccess::None => (),
        }
        Ok(())
    })
}

pub fn rewrite_dhcp(ctx: &impl CtrlContext, profile: &Profile) -> Result<(), Error> {
    rewrite_config(ctx.uci_path("dhcp"), |mut cfg| {
        let mut found_dhcp = false;
        while cfg.step() {
            let Ok(dhcp) = cfg.get::<Dhcp>() else {
                continue;
            };
            if dhcp.interface == profile.id.interface {
                found_dhcp = true;
            }
        }
        if !found_dhcp {
            cfg.push(
                &Dhcp {
                    interface: profile.id.interface.clone(),
                    start: 100, // default in the openwrt docs
                    limit: 150,
                    leasetime: "12h".into(),
                },
                Some(&profile.id.interface),
            )?;
        }
        Ok::<_, Error>(())
    })
}

pub fn allocate_interface_name(hint: Option<&str>) -> Result<String, Error> {
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
    pub fn parse<C: CtrlContext>(ctx: C) -> Result<Self, Error> {
        let list = list(ctx)?;
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
