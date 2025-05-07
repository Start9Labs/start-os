use crate::ethernet::DEFAULT_LAN_BRIDGE;
use crate::utils::DeserializeStdin;
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
use uciedit::openwrt::{
    DeviceType, FirewallForwarding, FirewallTarget, FirewallZone, InterfaceProto,
    NetworkBridgeVlan, NetworkDevice, NetworkInterface,
};
use uciedit::UciSection;
use uciedit::{parse_config, rewrite_config};

pub const DEFAULT_WAN_ZONE: &str = "wan";
pub const INTERFACE_NAME_LIMIT: usize = 5;

#[derive(Debug, Serialize, Deserialize)]
pub enum LanAccess {
    #[serde(rename = "ALL")]
    All,
    #[serde(rename = "SAME_PROFILE")]
    SameProfile,
    #[serde(rename = "other_profiles")]
    OtherProfiles(BTreeSet<ProfileId>),
}

#[derive(Debug, Serialize, Deserialize)]
pub enum WanAccess {
    #[serde(rename = "ALL")]
    All,
    #[serde(rename = "NONE")]
    None,
}

#[derive(Debug, Serialize, Deserialize)]
pub struct Profile {
    pub fullname: String,
    pub interface: Option<String>,
    pub gateway_ip: Ipv4Addr,
    pub lan_access: LanAccess,        // TODO
    pub wan_access: WanAccess,        // TODO
    pub access_to_new_profiles: bool, // TODO
    pub vlan_tag: Option<u16>,
}

pub fn profiles<C: Context>() -> ParentHandler<C> {
    ParentHandler::new()
        .subcommand("get", from_fn(get::<C>).with_display_serializable())
        .subcommand("delete", from_fn(delete::<C>).no_display())
        .subcommand("list", from_fn(list_rpc::<C>).with_display_serializable())
        .subcommand("create", from_fn(create::<C>).with_display_serializable())
        .subcommand("update", from_fn(update::<C>).with_display_serializable())
}

#[derive(Debug, Deserialize, Serialize, Clone, PartialEq, Eq, PartialOrd, Ord, Hash, Parser)]
pub struct ProfileId {
    pub interface: String,
    pub vlan_tag: u16,
}

#[derive(Debug, Deserialize, Serialize, Clone, PartialEq, Eq, PartialOrd, Ord, Hash, Parser)]
pub struct ProfileIdAndName {
    pub fullname: Option<String>,
    pub interface: Option<String>,
    pub vlan_tag: Option<u16>,
}

impl ProfileIdAndName {
    pub fn interface(interface: &str) -> Self {
        ProfileIdAndName {
            fullname: None,
            interface: Some(interface.into()),
            vlan_tag: None,
        }
    }

    pub fn matches(&self, other: &ProfileIdAndName) -> bool {
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
struct ProfileConfig {
    pub fullname: String,
    pub interface: String,
    pub vlan_tag: u16,
    #[uci(default_value = "false")]
    pub access_to_new_profiles: bool,
}

pub fn get<C: Context>(_ctx: C, query: ProfileIdAndName) -> Result<Profile, Error> {
    let mut wip_profile = Profile {
        fullname: String::new(),
        interface: None,
        gateway_ip: Ipv4Addr::new(0, 0, 0, 0),
        lan_access: LanAccess::SameProfile,
        wan_access: WanAccess::None,
        access_to_new_profiles: false,
        vlan_tag: None,
    };
    let mut lookup_other_vlan_tag = HashMap::new();
    let (interface, vlan_tag) = parse_config("./etc/config/startwrt", |mut cfg| {
        let mut found = None;
        while cfg.step() {
            if cfg.ty() != ProfileConfig::TY {
                continue;
            };
            let ProfileConfig {
                fullname,
                interface,
                vlan_tag,
                access_to_new_profiles,
            } = cfg.get()?;
            if match (&query.fullname, &query.interface, query.vlan_tag) {
                (_, Some(i), Some(v)) => &interface == i && vlan_tag == v,
                (_, None, Some(v)) => vlan_tag == v,
                (_, Some(i), None) => &interface == i,
                (Some(n), None, None) => &fullname == n,
                _ => false,
            } {
                wip_profile.interface = Some(interface.clone());
                wip_profile.vlan_tag = Some(vlan_tag);
                wip_profile.fullname = fullname;
                wip_profile.access_to_new_profiles = access_to_new_profiles;
                found = Some((interface, vlan_tag));
            } else {
                lookup_other_vlan_tag.insert(interface.clone(), vlan_tag);
            }
        }
        found.ok_or(ErrorKind::MissingProfile(query))
    })?;
    let query = ProfileIdAndName {
        fullname: Some(wip_profile.fullname.clone()),
        interface: Some(interface.clone()),
        vlan_tag: Some(vlan_tag),
    };
    parse_config("./etc/config/network", |mut cfg| {
        while cfg.step() {
            if let Ok(iface) = cfg.get::<NetworkInterface>() {
                let Some(name) = cfg.name() else { continue };
                if name == interface {
                    if iface.proto != InterfaceProto::STATIC {
                        return Err(ErrorKind::CorruptedProfile(query.clone()).into());
                    }
                    if let Some(ip) = iface.ipaddr {
                        wip_profile.gateway_ip = ip;
                    } else {
                        return Err(ErrorKind::CorruptedProfile(query.clone()).into());
                    }
                    return Ok::<_, Error>(());
                }
            }
        }
        Err(ErrorKind::CorruptedProfile(query.clone()).into())
    })?;
    parse_config("./etc/config/firewall", |mut cfg| {
        let mut forwarding = BTreeSet::new();
        while cfg.step() {
            let Ok(FirewallForwarding { src, dest }) = cfg.get() else {
                continue;
            };
            if src == interface {
                forwarding.insert(dest);
            }
        }
        if forwarding.contains(DEFAULT_WAN_ZONE) {
            wip_profile.wan_access = WanAccess::All;
        } else {
            wip_profile.wan_access = WanAccess::None;
        }
        cfg.restart();
        let mut other_profiles = BTreeSet::new();
        while cfg.step() {
            let Ok(FirewallZone { name, network, .. }) = cfg.get() else {
                continue;
            };
            if forwarding.contains(&name) {
                for other_interface in network {
                    if let Some(other_vlan_tag) = lookup_other_vlan_tag.get(&other_interface) {
                        other_profiles.insert(ProfileId {
                            interface: other_interface,
                            vlan_tag: *other_vlan_tag,
                        });
                    }
                }
            }
        }
        if other_profiles.is_empty() {
            wip_profile.lan_access = LanAccess::SameProfile;
        } else if other_profiles.len() == lookup_other_vlan_tag.len()
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

pub fn delete<C: Context>(
    _ctx: C,
    ProfileId {
        interface,
        vlan_tag,
    }: ProfileId,
) -> Result<(), Error> {
    todo!()
}

pub fn list() -> Result<Vec<ProfileIdAndName>, Error> {
    parse_config("./etc/config/startwrt", |mut cfg| {
        let mut found = Vec::new();
        while cfg.step() {
            let Ok(ProfileConfig {
                fullname,
                interface,
                vlan_tag,
                ..
            }) = cfg.get()
            else {
                continue;
            };
            found.push(ProfileIdAndName {
                fullname: Some(fullname),
                interface: Some(interface),
                vlan_tag: Some(vlan_tag),
            })
        }
        Ok(found)
    })
}

pub fn list_rpc<C: Context>(_ctx: C) -> Result<Vec<ProfileIdAndName>, Error> {
    list()
}

pub fn update<C: Context>(
    _ctx: C,
    DeserializeStdin(profile): DeserializeStdin<Profile>,
) -> Result<ProfileId, Error> {
    let (interface, vlan_tag) = rewrite_config("./etc/config/startwrt", |mut cfg| {
        while cfg.step() {
            let Ok(mut this_cfg) = cfg.get::<ProfileConfig>() else {
                continue;
            };
            if match (&profile.interface, &profile.vlan_tag) {
                (None, None) => profile.fullname == this_cfg.fullname,
                (None, Some(vlan_tag)) => &this_cfg.vlan_tag == vlan_tag,
                (Some(interface), None) => &this_cfg.interface == interface,
                (Some(interface), Some(vlan_tag)) => {
                    &this_cfg.interface == interface && &this_cfg.vlan_tag == vlan_tag
                }
            } {
                this_cfg.fullname = profile.fullname.clone();
                this_cfg.access_to_new_profiles = profile.access_to_new_profiles;
                let found = (this_cfg.interface.clone(), this_cfg.vlan_tag);
                cfg.set(&this_cfg)?;
                return Ok(found);
            }
        }
        Err(Error::from(ErrorKind::MissingProfile(ProfileIdAndName {
            interface: profile.interface.clone(),
            vlan_tag: profile.vlan_tag,
            fullname: if profile.interface.is_none() && profile.vlan_tag.is_none() {
                Some(profile.fullname.clone())
            } else {
                None
            },
        })))
    })?;
    let mut all_interfaces = BTreeSet::<String>::new();
    rewrite_config("./etc/config/network", |mut cfg| {
        let mut found_bridge = None;
        let mut found_vlan = false;
        let mut found_interface = false;
        while cfg.step() {
            match &*cfg.ty() {
                NetworkInterface::TY => {
                    if let Ok(mut iface) = cfg.get::<NetworkInterface>() {
                        let Some(name) = cfg.name() else { continue };
                        if iface.proto == InterfaceProto::STATIC {
                            all_interfaces.insert(name.to_string());
                        }
                        if name == interface {
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
                }
                NetworkDevice::TY => {
                    if let Ok(dev) = cfg.get::<NetworkDevice>() {
                        if dev.ty == Some(DeviceType::BRIDGE) && found_bridge.is_none() {
                            found_bridge = Some(dev.name.to_string());
                        }
                    }
                }
                NetworkBridgeVlan::TY => {
                    if let Ok(vlan) = cfg.get::<NetworkBridgeVlan>() {
                        if vlan.vlan == vlan_tag {
                            found_vlan = true;
                        }
                    }
                }
                _ => (),
            }
        }
        if !found_interface {
            let found_bridge = found_bridge.clone().ok_or(ErrorKind::MissingLanBridge)?;
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
        if !found_vlan {
            let found_bridge = found_bridge.ok_or(ErrorKind::MissingLanBridge)?;
            cfg.push(
                &NetworkBridgeVlan {
                    device: found_bridge,
                    vlan: vlan_tag,
                    ports: Vec::new(),
                },
                None,
            )?;
        }
        Ok::<_, Error>(vlan_tag)
    })?;
    rewrite_firewall(&interface, &all_interfaces, &[], &profile, false)?;
    Ok(ProfileId {
        interface,
        vlan_tag,
    })
}

pub fn create<C: Context>(
    _ctx: C,
    DeserializeStdin(profile): DeserializeStdin<Profile>,
) -> Result<ProfileId, Error> {
    let interface = allocate_interface_name(match &profile.interface {
        Some(name) => name,
        None => &profile.fullname,
    })?;
    let mut all_interfaces = BTreeSet::<String>::new();
    // FIXME: feature to stage both config rewrites until we are sure about them
    let vlan_tag = rewrite_config("./etc/config/network", |mut cfg| {
        let mut existing_tags = BTreeSet::new();
        let mut found_bridge = None;
        while cfg.step() {
            match &*cfg.ty() {
                NetworkInterface::TY => {
                    let Some(name) = cfg.name() else { continue };
                    if name == interface {
                        return Err(ErrorKind::InterfaceNameConflict(interface.clone()).into());
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
        let vlan_tag = match profile.vlan_tag {
            Some(chosen_tag) => {
                if existing_tags.contains(&chosen_tag) {
                    return Err(ErrorKind::DuplicateVlanTag(chosen_tag).into());
                }
                chosen_tag
            }
            None => (101..2_u16.pow(12))
                .find(|t| !existing_tags.contains(t))
                .expect("we somehow exausted all vlan tags, that should be impossible"),
        };
        cfg.push(
            &NetworkInterface {
                device: format!("{found_bridge}.{vlan_tag}"),
                proto: InterfaceProto::STATIC,
                ipaddr: Some(profile.gateway_ip),
                netmask: Some(Ipv4Addr::new(255, 255, 255, 0)),
            },
            Some(&interface),
        )?;
        cfg.push(
            &NetworkBridgeVlan {
                device: found_bridge,
                vlan: vlan_tag,
                ports: Vec::new(),
            },
            None,
        )?;
        Ok::<_, Error>(vlan_tag)
    })?;
    let mut wants_access = Vec::new();
    rewrite_config("./etc/config/startwrt", |mut cfg| {
        while cfg.step() {
            if let Ok(ProfileConfig {
                interface,
                vlan_tag,
                access_to_new_profiles,
                ..
            }) = cfg.get()
            {
                if access_to_new_profiles {
                    wants_access.push(ProfileId {
                        interface,
                        vlan_tag,
                    });
                }
            }
        }
        cfg.push(
            &ProfileConfig {
                fullname: profile.fullname.clone(),
                interface: interface.clone(),
                vlan_tag,
                access_to_new_profiles: profile.access_to_new_profiles,
            },
            Some(&interface),
        )?;
        Ok::<_, Error>(())
    })?;
    rewrite_firewall(&interface, &all_interfaces, &wants_access, &profile, true)?;
    Ok(ProfileId {
        interface,
        vlan_tag,
    })
}

fn rewrite_firewall(
    interface: &str,
    all_interfaces: &BTreeSet<String>,
    wants_access: &[ProfileId],
    profile: &Profile,
    remake_zone: bool,
) -> Result<(), Error> {
    if let Some(interface_in_profile) = &profile.interface {
        assert_eq!(interface_in_profile, interface);
    }
    rewrite_config("./etc/config/firewall", |mut cfg| {
        let mut found_wan = false;
        let mut this_zone_name = format!("vlan_{interface}");
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
                    if interface == zone_interface {
                        this_zone_name = zone.name.clone();
                    }
                    if all_interfaces.contains(&zone_interface) {
                        all_zones.insert(zone_interface, zone.name.clone());
                    }
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
                    network: vec![interface.to_string()],
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
                            return Err(ErrorKind::MissingProfile(ProfileIdAndName::interface(
                                &other_profile.interface,
                            ))
                            .into())
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
                    return Err(ErrorKind::MissingProfile(ProfileIdAndName::interface(
                        &other_profile.interface,
                    ))
                    .into())
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

pub fn allocate_interface_name(hint: &str) -> Result<String, Error> {
    fn random() -> String {
        String::from_iter([(); INTERFACE_NAME_LIMIT].map(|_| rand::random_range('a'..='z')))
    }
    let mut hint = hint.trim().to_ascii_lowercase();
    hint.truncate(INTERFACE_NAME_LIMIT);
    let hint = hint.split(|c: char| !c.is_ascii_alphanumeric()).next();
    let mut name = match hint {
        None | Some("") => random(),
        Some(hint) => hint.to_string(),
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
        if ip_out.contains("does not exist") {
            return Ok(name);
        }
        name = random();
    }
    Err(eyre!("gave up looking for a new interface name").into())
}

pub struct Lookup {
    list: Vec<ProfileIdAndName>,
    from_vlan: OnceCell<HashMap<u16, ProfileIdAndName>>,
    from_interface: OnceCell<HashMap<String, ProfileIdAndName>>,
    from_fullname: OnceCell<HashMap<String, ProfileIdAndName>>,
}

impl Lookup {
    pub fn parse() -> Result<Self, Error> {
        Ok(Self {
            list: list()?,
            from_vlan: OnceCell::new(),
            from_interface: OnceCell::new(),
            from_fullname: OnceCell::new(),
        })
    }

    pub fn resolve(
        &self,
        q @ ProfileIdAndName {
            fullname,
            interface,
            vlan_tag,
        }: &ProfileIdAndName,
    ) -> Result<&ProfileIdAndName, Error> {
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
        Err(ErrorKind::MissingProfile(q.clone()).into())
    }

    pub fn from_vlan(&self, vlan_tag: u16) -> Option<&ProfileIdAndName> {
        self.from_vlan
            .get_or_init(|| {
                self.list
                    .iter()
                    .map(|id| {
                        (
                            id.vlan_tag.expect("list should provide vlan tags"),
                            id.clone(),
                        )
                    })
                    .collect()
            })
            .get(&vlan_tag)
    }

    pub fn from_interface(&self, interface: &str) -> Option<&ProfileIdAndName> {
        self.from_interface
            .get_or_init(|| {
                self.list
                    .iter()
                    .map(|id| {
                        (
                            id.interface.clone().expect("list should provide interface"),
                            id.clone(),
                        )
                    })
                    .collect()
            })
            .get(interface)
    }

    pub fn from_fullname(&self, fullname: &str) -> Option<&ProfileIdAndName> {
        self.from_fullname
            .get_or_init(|| {
                self.list
                    .iter()
                    .map(|id| {
                        (
                            id.fullname.clone().expect("list should provide fullname"),
                            id.clone(),
                        )
                    })
                    .collect()
            })
            .get(fullname)
    }
}
