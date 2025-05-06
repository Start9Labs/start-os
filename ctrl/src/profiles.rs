use crate::{
    utils::{DeserializeStdin, HandlerExtSerde},
    Error, ErrorKind,
};
use clap::Parser;
use color_eyre::eyre::{eyre, OptionExt};
use rpc_toolkit::{from_fn, Context, HandlerExt as _, ParentHandler};
use serde::{Deserialize, Serialize};
use std::{
    collections::{BTreeMap, BTreeSet},
    net::Ipv4Addr,
};
use std::{
    io::Read,
    process::{Command, Stdio},
};
use uciedit::openwrt::{
    DeviceType, FirewallForwarding, FirewallTarget, FirewallZone, InterfaceProto,
    NetworkBridgeVlan, NetworkDevice, NetworkInterface,
};
use uciedit::{parse_config, rewrite_config};

const DEFAULT_LAN_BRIDGE: &str = "br-lan";
const DEFAULT_WAN_ZONE: &str = "wan";
const INTERFACE_NAME_LIMIT: usize = 5;

#[derive(Debug, Serialize, Deserialize)]
pub enum LanAccess {
    #[serde(rename = "ALL")]
    All,
    #[serde(rename = "SAME_PROFILE")]
    SameProfile,
    #[serde(rename = "other_profiles")]
    OtherProfiles(Vec<ProfileId>),
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
    pub name: String,
    pub interface: Option<String>,
    pub gateway_ip: Ipv4Addr,
    pub subnet_ip: Ipv4Addr,
    pub lan_access: LanAccess,    // TODO
    pub wan_access: WanAccess,    // TODO
    pub block_new_profiles: bool, // TODO
    pub vlan_tag: Option<u16>,
}

pub fn profiles<C: Context>() -> ParentHandler<C> {
    ParentHandler::new()
        .subcommand("get", from_fn(get::<C>).with_display_serializable())
        .subcommand("delete", from_fn(delete::<C>).no_display())
        .subcommand("list", from_fn(list::<C>).with_display_serializable())
        .subcommand("create", from_fn(create::<C>).no_display())
        .subcommand("update", from_fn(update::<C>).no_display())
}

#[derive(Debug, Deserialize, Serialize, Clone, Parser)]
pub struct ProfileId {
    pub interface: String,
    pub vlan_tag: u16,
}

pub fn get<C: Context>(
    _ctx: C,
    ProfileId {
        interface,
        vlan_tag,
    }: ProfileId,
) -> Result<Profile, Error> {
    todo!()
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

pub fn list<C: Context>(_ctx: C) -> Result<Vec<Profile>, Error> {
    todo!()
}

#[derive(Debug, Deserialize, Serialize)]
pub struct ProfileValueArgs {
    profile: Profile,
}

pub fn create<C: Context>(
    _ctx: C,
    DeserializeStdin(profile): DeserializeStdin<Profile>,
) -> Result<ProfileId, Error> {
    let interface = allocate_interface_name(match &profile.interface {
        Some(name) => name,
        None => &profile.name,
    })?;
    let mut all_interfaces = BTreeSet::<String>::new();
    // FIXME: feature to stage both config rewrites until we are sure about them
    let vlan_tag = rewrite_config("./etc/config/network", |mut cfg| {
        let mut existing_tags = BTreeSet::new();
        let mut found_bridge = None;
        while cfg.step() {
            let name = cfg.name();
            match &*cfg.ty() {
                NetworkInterface::TY => {
                    let Some(name) = name else { continue };
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
            NetworkInterface {
                device: format!("{found_bridge}.{vlan_tag}"),
                proto: InterfaceProto::STATIC,
                ipaddr: Some(profile.gateway_ip),
                netmask: Some(Ipv4Addr::new(255, 255, 255, 0)),
            },
            Some(&interface),
        )?;
        cfg.push(
            NetworkBridgeVlan {
                device: found_bridge,
                vlan: vlan_tag,
                ports: Vec::new(),
            },
            None,
        )?;
        Ok::<_, Error>(vlan_tag)
    })?;
    rewrite_firewall(&interface, &all_interfaces, &profile, true)?;
    Ok(ProfileId {
        interface,
        vlan_tag,
    })
}

fn rewrite_firewall(
    interface: &str,
    all_interfaces: &BTreeSet<String>,
    profile: &Profile,
    remake_zone: bool,
) -> Result<(), Error> {
    if let Some(interface_in_profile) = &profile.interface {
        assert_eq!(interface_in_profile, interface);
    }
    rewrite_config("./etc/config/firewall", |mut cfg| {
        let mut found_wan = false;
        let this_zone_name = format!("vlan_{interface}");
        let mut all_zones = BTreeMap::new();
        while cfg.step() {
            match &*cfg.ty() {
                FirewallZone::TY => {
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
                        for iface in zone.network {
                            if all_interfaces.contains(&iface) {
                                all_zones.insert(iface, zone.name.clone());
                            }
                        }
                    }
                }
                FirewallForwarding::TY => {
                    let Ok(fwd) = cfg.get::<FirewallForwarding>() else {
                        continue;
                    };
                    if fwd.src == this_zone_name {
                        cfg.remove();
                    }
                }
                _ => (),
            }
        }
        if !found_wan {
            return Err(ErrorKind::MissingWanInterface.into());
        }
        if remake_zone {
            cfg.push(
                FirewallZone {
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
                            FirewallForwarding {
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
                            FirewallForwarding {
                                src: this_zone_name.clone(),
                                dest: other_zone.clone(),
                            },
                            None,
                        )?,
                        None => {
                            return Err(ErrorKind::InvalidProfileId(other_profile.clone()).into())
                        }
                    }
                }
            }
        }
        Ok(())
    })
}

pub fn update<C: Context>(
    ctx: C,
    DeserializeStdin(profile): DeserializeStdin<Profile>,
) -> Result<ProfileId, Error> {
    let interface = match profile.interface {
        Some(i) => i,
        None => return Err(eyre!("provide interface when updating").into()),
    };
    let vlan_tag = match profile.vlan_tag {
        Some(i) => i,
        None => return Err(eyre!("provide vlan_tag when updating").into()),
    };
    todo!()
}

pub fn validate_interface_name(name: &str) -> Result<(), Error> {
    match name {
        "lan" | "wan" | "wan6" => Err(ErrorKind::InterfaceNameConflict(name.into()).into()),
        _ if !name.chars().any(|c| c.is_ascii_alphanumeric()) => {
            Err(ErrorKind::InterfaceNameInvalid(name.into()).into())
        }
        _ if name.len() > INTERFACE_NAME_LIMIT => {
            Err(ErrorKind::InterfaceNameTooLong(name.into()).into())
        }
        _ => Ok(()),
    }
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
