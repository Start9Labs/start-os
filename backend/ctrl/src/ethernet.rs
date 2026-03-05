use crate::profiles::{self, ProfileId, ProfileIdOpt};
use crate::utils::DeserializeStdin;
use crate::utils::HandlerExtSerde;
use crate::CtrlContext;
use crate::{Error, ErrorKind};
use rpc_toolkit::{from_fn, ParentHandler};
use serde::{Deserialize, Serialize};
use std::collections::{BTreeMap, HashMap, HashSet};
use std::process::Command;
use uciedit::openwrt::{
    DeviceType, InterfaceProto, NetworkBridgeVlan, NetworkDevice, NetworkInterface,
    NetworkVlanPort, NetworkVlanPortTagging,
};
use uciedit::{dump_all, parse_all, Arena, Configs};

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

pub fn ethernet<C: CtrlContext + Clone>() -> ParentHandler<C> {
    ParentHandler::new()
        .subcommand("get", from_fn(get::<C>).with_display_serializable())
        .subcommand("set", from_fn(set::<C>).with_display_serializable())
        .subcommand("edit", from_fn(edit::<C>).with_display_serializable())
}

pub fn get<C: CtrlContext>(ctx: C) -> Result<Ethernet, Error> {
    let arena = Arena::new();
    let cfgs = parse_all(ctx.uci_root(), &arena, &["network", "startwrt", "firewall"])?;
    get_config(ctx, &cfgs)
}

fn get_config(ctx: impl CtrlContext, cfgs: &Configs) -> Result<Ethernet, Error> {
    let lookup = profiles::Lookup::parse(ctx.clone(), cfgs)?;
    // TODO: avoid duplicating this "find the bridge" logic so much
    let mut found_bridge = None;
    cfgs["network"].try_each(|_, dev: NetworkDevice| {
        if dev.ty == Some(DeviceType::BRIDGE)
            && (found_bridge.is_none() || dev.name == DEFAULT_LAN_BRIDGE)
        {
            found_bridge = Some(dev);
        }
        Ok::<_, Error>(())
    })?;
    let Some(found_bridge) = found_bridge else {
        return Err(ErrorKind::MissingLanBridge.into());
    };

    let mut wan_ipv6 = false;
    let mut wan_port = None;
    let mut all_ports: HashSet<String> = found_bridge.ports.into_iter().collect();
    let mut vlan_ports = HashMap::new();
    for section in &cfgs["network"].sections {
        if let Some(vlan) = section.get_typed::<NetworkBridgeVlan>()? {
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
        if let Some(iface) = section.get_typed::<NetworkInterface>()? {
            if iface.proto == InterfaceProto::DHCP
                && section.name().as_deref() == Some(DEFAULT_WAN_INTERFACE)
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
    cfgs["network"].try_each(|_, iface: NetworkInterface| {
        if iface.proto == InterfaceProto::DHCPV6 && Some(&iface.device) == wan_port.as_ref() {
            wan_ipv6 = true;
        }
        Ok::<_, Error>(())
    })?;
    Ok(Ethernet {
        wan_ipv6,
        wan_port,
        ports,
    })
}

pub fn set<C: CtrlContext>(
    ctx: C,
    DeserializeStdin(ethernet): DeserializeStdin<Ethernet<ProfileIdOpt>>,
) -> Result<(), Error> {
    let mut retries = 4;
    loop {
        let arena = Arena::new();
        let mut cfgs = parse_all(ctx.uci_root(), &arena, &["network", "startwrt", "firewall"])?;
        let lookup = profiles::Lookup::parse(ctx.clone(), &cfgs)?;
        let ethernet = Ethernet::<ProfileId> {
            wan_ipv6: ethernet.wan_ipv6,
            wan_port: ethernet.wan_port.clone(),
            ports: ethernet
                .ports
                .iter()
                .map(|(k, v)| {
                    Ok((
                        k.clone(),
                        Port {
                            profile: match &v.profile {
                                Some(id) => Some(lookup.resolve(id)?.clone()),
                                None => None,
                            },
                        },
                    ))
                })
                .collect::<Result<_, Error>>()?,
        };
        set_config(&ctx, &mut cfgs, &ethernet, &lookup)?;
        match dump_all(ctx.uci_root(), cfgs) {
            Err(uciedit::Error::Conflict { .. }) if retries > 0 => {
                retries -= 1;
                continue;
            }
            Err(err) => return Err(err.into()),
            Ok(()) => {
                if ctx.effectful() {
                    let _ = Command::new("/etc/init.d/network")
                        .arg("reload")
                        .spawn()?
                        .wait();
                }
                return Ok(());
            }
        }
    }
}

fn set_config(
    _ctx: &impl CtrlContext,
    cfgs: &mut Configs,
    ethernet: &Ethernet,
    lookup: &profiles::Lookup,
) -> Result<(), Error> {
    // TODO: avoid duplicating this "find the bridge" logic so much
    let mut found_bridge = None;
    cfgs["network"].try_each(|_, dev: NetworkDevice| {
        if dev.ty == Some(DeviceType::BRIDGE)
            && (found_bridge.is_none() || dev.name == DEFAULT_LAN_BRIDGE)
        {
            found_bridge = Some(dev);
        }
        Ok::<_, Error>(())
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

    // Remove vlans for this bridge, and update interfaces/devices
    cfgs["network"].sections.retain(|section| {
        if let Some(vlan) = section.get::<NetworkBridgeVlan>().ok() {
            if vlan.device == bridge.name {
                return false; // we'll re-add these later
            }
        }
        true
    });

    // Update WAN interfaces, if they exist
    for section in &mut cfgs["network"].sections {
        if let Some(dev) = section.get_typed::<NetworkDevice>()? {
            if dev.ty == Some(DeviceType::BRIDGE) && dev.name == bridge.name {
                section.set(&bridge)?;
                pending_bridge = false;
            }
        }
        if let Some(mut iface) = section.get_typed::<NetworkInterface>()? {
            if iface.proto == InterfaceProto::DHCP
                && section.name().as_deref() == Some(DEFAULT_WAN_INTERFACE)
            {
                pending_ipv4 = false;
                if let Some(wan_port) = &ethernet.wan_port {
                    iface.device = wan_port.clone();
                    section.set(&iface)?;
                }
            }
            if iface.proto == InterfaceProto::DHCPV6
                && section.name().as_deref() == Some(DEFAULT_WAN6_INTERFACE)
            {
                pending_ipv6 = false;
                if let Some(wan_port) = &ethernet.wan_port {
                    if ethernet.wan_ipv6 {
                        iface.device = wan_port.clone();
                        section.set(&iface)?;
                    }
                }
            }
        }
    }

    // Remove WAN interfaces that are no longer needed
    cfgs["network"].sections.retain(|section| {
        if let Ok(iface) = section.get::<NetworkInterface>() {
            if iface.proto == InterfaceProto::DHCP
                && section.name().as_deref() == Some(DEFAULT_WAN_INTERFACE)
                && ethernet.wan_port.is_none()
            {
                return false;
            }
            if iface.proto == InterfaceProto::DHCPV6
                && section.name().as_deref() == Some(DEFAULT_WAN6_INTERFACE)
                && (ethernet.wan_port.is_none() || !ethernet.wan_ipv6)
            {
                return false;
            }
        }
        true
    });

    // Add WAN interfaces if still needed
    if let Some(wan_port) = &ethernet.wan_port {
        if pending_ipv4 {
            cfgs["network"].append(
                &NetworkInterface {
                    device: wan_port.clone(),
                    proto: InterfaceProto::DHCP,
                    ..Default::default()
                },
                Some(DEFAULT_WAN_INTERFACE),
            )?;
        }
        if pending_ipv6 {
            cfgs["network"].append(
                &NetworkInterface {
                    device: wan_port.clone(),
                    proto: InterfaceProto::DHCPV6,
                    ..Default::default()
                },
                Some(DEFAULT_WAN6_INTERFACE),
            )?;
        }
    }

    if pending_bridge {
        cfgs["network"].append(&bridge, None)?;
    }

    let needs_vlan_filtering = lookup.list().iter().any(|p| p.vlan_tag != 1);

    for profile in lookup.list() {
        let mut ports = Vec::new();
        for (port_name, port) in &ethernet.ports {
            if Some(port_name) == ethernet.wan_port.as_ref() {
                continue;
            }
            let assigned = port.profile.as_ref() == Some(profile);
            let default_to_admin = needs_vlan_filtering
                && port.profile.is_none()
                && profile.vlan_tag == 1;
            if assigned || default_to_admin {
                ports.push(NetworkVlanPort {
                    port: port_name.clone(),
                    tagging: Some(NetworkVlanPortTagging::PRIMARY),
                });
            }
        }

        let vlan = profile.vlan_tag;
        if ports.is_empty() && vlan == 1 {
            continue;
        }
        cfgs["network"].append(
            &NetworkBridgeVlan {
                device: bridge.name.clone(),
                vlan,
                ports,
            },
            None,
        )?;
    }
    Ok(())
}

pub fn edit<C: CtrlContext + Clone>(ctx: C) -> Result<(), Error> {
    let current_ethernet = get(ctx.clone())?;
    let current_ethernet = Ethernet {
        wan_ipv6: current_ethernet.wan_ipv6,
        wan_port: current_ethernet.wan_port,
        ports: current_ethernet
            .ports
            .into_iter()
            .map(|(k, v)| {
                (
                    k,
                    Port {
                        profile: v.profile.map(Into::into),
                    },
                )
            })
            .collect(),
    };
    let modified_ethernet = crate::utils::edit_in_editor(&current_ethernet)?;
    set(ctx, DeserializeStdin(modified_ethernet))
}
