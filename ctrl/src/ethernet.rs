use crate::profiles::{self, ProfileId, ProfileIdOpt};
use crate::utils::DeserializeStdin;
use crate::{utils::HandlerExtSerde, Error, ErrorKind};
use rpc_toolkit::{from_fn, Context, ParentHandler};
use serde::{Deserialize, Serialize};
use std::collections::{BTreeMap, HashMap, HashSet};
use std::process::Command;
use uciedit::openwrt::{
    DeviceType, InterfaceProto, NetworkBridgeVlan, NetworkDevice, NetworkInterface,
    NetworkVlanPortTagging,
};
use uciedit::{parse_config, rewrite_config};

pub const DEFAULT_LAN_BRIDGE: &str = "br-lan";
pub const DEFAULT_WAN_INTERFACE: &str = "wan";
pub const DEFAULT_WAN6_INTERFACE: &str = "wan6";

#[derive(Debug, Serialize, Deserialize)]
pub struct Port<Id: Ord = ProfileId> {
    pub profile: Option<Id>,
}

#[derive(Debug, Serialize, Deserialize)]
pub struct Ethernet<Id: Ord = ProfileId> {
    pub wan_ipv6: bool,
    pub wan_port: Option<String>,
    pub ports: BTreeMap<String, Port<Id>>,
}

pub fn ethernet<C: Context>() -> ParentHandler<C> {
    ParentHandler::new()
        .subcommand("get", from_fn(get::<C>).with_display_serializable())
        .subcommand("update", from_fn(update::<C>).with_display_serializable())
}

pub fn get<C: Context>(ctx: C) -> Result<Ethernet, Error> {
    let lookup = profiles::Lookup::parse()?;
    parse_config("./etc/config/network", |mut cfg| {
        // TODO: avoid duplicating this "find the bridge" logic so much
        let mut found_bridge = None;
        cfg.each(|_, dev: NetworkDevice| {
            if dev.ty == Some(DeviceType::BRIDGE)
                && (found_bridge.is_none() || dev.name == DEFAULT_LAN_BRIDGE)
            {
                found_bridge = Some(dev);
            }
        })?;
        cfg.restart();
        let Some(found_bridge) = found_bridge else {
            return Err(ErrorKind::MissingLanBridge.into());
        };

        let mut wan_ipv6 = false;
        let mut wan_port = None;
        let mut all_ports: HashSet<String> = found_bridge.ports.into_iter().collect();
        let mut vlan_ports = HashMap::new();
        cfg.restart();
        while cfg.step() {
            if let Some(vlan) = cfg.get_typed::<NetworkBridgeVlan>()? {
                for port in vlan.ports {
                    match port.tagging {
                        None | Some(NetworkVlanPortTagging::TAGGED) => (),
                        Some(NetworkVlanPortTagging::UNTAGGED) => {
                            vlan_ports.entry(port.port).or_insert((vlan.vlan, false));
                        }
                        Some(NetworkVlanPortTagging::PRIMARY) => {
                            let entry = vlan_ports.entry(port.port).or_insert((vlan.vlan, true));
                            if !entry.1 {
                                entry.0 = vlan.vlan; // primary overrides untagged
                            }
                        }
                    }
                }
            }
            if let Some(iface) = cfg.get_typed::<NetworkInterface>()? {
                if iface.proto == InterfaceProto::DHCP
                    && cfg.name().as_deref() == Some(DEFAULT_WAN_INTERFACE)
                {
                    // TODO: better check to see if this is actually an ethernet port
                    all_ports.insert(iface.device.clone());
                    wan_port = Some(iface.device.clone());
                }
            }
        }
        wan_port = wan_port.filter(|s| all_ports.contains(s));
        let ports = all_ports
            .iter()
            .map(|name| {
                (
                    name.clone(),
                    Port {
                        profile: vlan_ports
                            .get(name)
                            .and_then(|(tag, _)| lookup.from_vlan(*tag))
                            .cloned(),
                    },
                )
            })
            .collect();
        cfg.each(|_, iface: NetworkInterface| {
            if iface.proto == InterfaceProto::DHCPV6 && Some(&iface.device) == wan_port.as_ref() {
                wan_ipv6 = true;
            }
        })?;
        Ok(Ethernet {
            wan_ipv6,
            wan_port,
            ports,
        })
    })
}

pub fn update<C: Context>(
    _ctx: C,
    DeserializeStdin(ethernet): DeserializeStdin<Ethernet<ProfileIdOpt>>,
) -> Result<(), Error> {
    let lookup = profiles::Lookup::parse()?;
    let ethernet = Ethernet::<ProfileId> {
        wan_ipv6: ethernet.wan_ipv6,
        wan_port: ethernet.wan_port,
        ports: ethernet
            .ports
            .into_iter()
            .map(|(k, v)| {
                Ok((
                    k,
                    Port {
                        profile: match v.profile {
                            Some(id) => Some(lookup.resolve(&id)?.clone()),
                            None => None,
                        },
                    },
                ))
            })
            .collect::<Result<_, Error>>()?,
    };

    rewrite_config("./etc/config/network", |mut cfg| {
        // TODO: avoid duplicating this "find the bridge" logic so much
        let mut found_bridge = None;
        cfg.readonly().each(|_, dev: NetworkDevice| {
            if dev.ty == Some(DeviceType::BRIDGE)
                && (found_bridge.is_none() || dev.name == DEFAULT_LAN_BRIDGE)
            {
                found_bridge = Some(dev);
            }
        })?;
        let mut bridge = match found_bridge {
            Some(br) => br,
            None => NetworkDevice {
                name: DEFAULT_LAN_BRIDGE.into(),
                ty: Some(DeviceType::BRIDGE),
                ports: Vec::new(),
            },
        };
        bridge.ports.clear();
        for (port_name, port) in &ethernet.ports {
            if Some(port_name) == ethernet.wan_port.as_ref() {
                if port.profile.is_some() {
                    return Err(ErrorKind::WanPortWithProfile {
                        port: port_name.clone(),
                    }
                    .into());
                }
            } else {
                bridge.ports.push(port_name.clone());
            }
        }

        let mut pending_bridge = true;
        let mut pending_ipv4 = ethernet.wan_port.is_some();
        let mut pending_ipv6 = ethernet.wan_ipv6 && ethernet.wan_port.is_some();
        cfg.restart();
        while cfg.step() {
            if let Some(vlan) = cfg.get_typed::<NetworkBridgeVlan>()? {
                if vlan.device == bridge.name {
                    // we'll re-add these later
                    // TODO: is there any use-case for preserving non-primary untagged
                    // or tagged ethernet ports?
                    cfg.remove();
                }
            }
            if let Some(dev) = cfg.get_typed::<NetworkDevice>()? {
                if dev.ty == Some(DeviceType::BRIDGE) && dev.name == bridge.name {
                    cfg.set(&bridge)?;
                    pending_bridge = false;
                }
            }
            if let Some(mut iface) = cfg.get_typed::<NetworkInterface>()? {
                if iface.proto == InterfaceProto::DHCP
                    && cfg.name().as_deref() == Some(DEFAULT_WAN_INTERFACE)
                {
                    pending_ipv4 = false;
                    if let Some(wan_port) = &ethernet.wan_port {
                        iface.device = wan_port.clone();
                        cfg.set(&iface)?;
                    } else {
                        cfg.remove();
                    }
                }
                if iface.proto == InterfaceProto::DHCPV6
                    && cfg.name().as_deref() == Some(DEFAULT_WAN6_INTERFACE)
                {
                    pending_ipv6 = false;
                    if let Some(wan_port) = &ethernet.wan_port {
                        if ethernet.wan_ipv6 {
                            iface.device = wan_port.clone();
                            cfg.set(&iface)?;
                        } else {
                            cfg.remove();
                        }
                    } else {
                        cfg.remove();
                    }
                }
            }
        }

        if let Some(wan_port) = &ethernet.wan_port {
            if pending_ipv4 {
                cfg.push(
                    &NetworkInterface {
                        device: wan_port.clone(),
                        proto: InterfaceProto::DHCP,
                        ipaddr: None,
                        netmask: None,
                    },
                    Some(DEFAULT_WAN6_INTERFACE),
                )?;
            }
            if pending_ipv6 {
                cfg.push(
                    &NetworkInterface {
                        device: wan_port.clone(),
                        proto: InterfaceProto::DHCPV6,
                        ipaddr: None,
                        netmask: None,
                    },
                    Some(DEFAULT_WAN6_INTERFACE),
                )?;
            }
        }
        if pending_bridge {
            cfg.push(&bridge, None)?;
        }

        for profile in lookup.list() {
            let mut ports = Vec::new();
            for (port_name, port) in &ethernet.ports {
                let Some(port_profile) = &port.profile else {
                    continue;
                };
                if port_profile != profile {
                    continue;
                };
                ports.push(uciedit::openwrt::NetworkVlanPort {
                    port: port_name.clone(),
                    tagging: Some(NetworkVlanPortTagging::PRIMARY),
                });
            }

            let vlan = profile.vlan_tag;
            cfg.push(
                &NetworkBridgeVlan {
                    device: bridge.name.clone(),
                    vlan,
                    ports,
                },
                None,
            )?;
        }
        Ok::<_, Error>(())
    })?;
    let _ = Command::new("/etc/init.d/network")
        .arg("reload")
        .spawn()?
        .wait();
    Ok(())
}
