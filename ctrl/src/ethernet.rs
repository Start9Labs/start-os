use crate::profiles::{self, ProfileIdAndName};
use crate::utils::DeserializeStdin;
use crate::{utils::HandlerExtSerde, Error, ErrorKind};
use clap::Parser;
use color_eyre::eyre::{eyre, OptionExt};
use rpc_toolkit::{from_fn, Context, HandlerExt as _, ParentHandler};
use serde::{Deserialize, Serialize};
use std::collections::{BTreeMap, BTreeSet, HashMap, HashSet};
use std::io::Read;
use std::net::Ipv4Addr;
use std::process::{Command, Stdio};
use uciedit::openwrt::{
    DeviceType, FirewallForwarding, FirewallTarget, FirewallZone, InterfaceProto,
    NetworkBridgeVlan, NetworkDevice, NetworkInterface, NetworkVlanPortTagging,
};
use uciedit::UciSection;
use uciedit::{parse_config, rewrite_config};

pub const DEFAULT_LAN_BRIDGE: &str = "br-lan";

#[derive(Debug, Serialize, Deserialize)]
pub struct Port {
    pub profile: Option<ProfileIdAndName>,
}

#[derive(Debug, Serialize, Deserialize)]
pub struct Ethernet {
    pub wan_port: Option<String>,
    pub ports: HashMap<String, Port>,
}

pub fn ethernet<C: Context>() -> ParentHandler<C> {
    ParentHandler::new()
        .subcommand("get", from_fn(get::<C>).with_display_serializable())
        .subcommand("update", from_fn(update::<C>).with_display_serializable())
}

pub fn get<C: Context>(ctx: C) -> Result<Ethernet, Error> {
    let profile_list = profiles::list(ctx)?;
    let lookup: HashMap<u16, ProfileIdAndName> = profile_list
        .into_iter()
        .map(|id| (id.vlan_tag.expect("list should provide vlan_tags"), id))
        .collect();
    parse_config("./etc/config/network", |mut cfg| {
        // TODO: avoid duplicating this "find the bridge" logic so much
        let mut found_bridge = None;
        while cfg.step() {
            if let Ok(dev) = cfg.get::<NetworkDevice>() {
                if dev.ty == Some(DeviceType::BRIDGE)
                    && (found_bridge.is_none() || dev.name == DEFAULT_LAN_BRIDGE)
                {
                    found_bridge = Some(dev);
                }
            }
        }
        cfg.reset();
        let Some(found_bridge) = found_bridge else {
            return Err(ErrorKind::MissingLanBridge.into());
        };

        let mut wan_port = None;
        let mut all_ports: HashSet<String> = found_bridge.ports.into_iter().collect();
        let mut vlan_ports = HashMap::new();
        while cfg.step() {
            match &*cfg.ty() {
                NetworkBridgeVlan::TY => {
                    if let Ok(vlan) = cfg.get::<NetworkBridgeVlan>() {
                        for port in vlan.ports {
                            match port.tagging {
                                None | Some(NetworkVlanPortTagging::TAGGED) => (),
                                Some(NetworkVlanPortTagging::UNTAGGED) => {
                                    vlan_ports.entry(port.port).or_insert((vlan.vlan, false));
                                }
                                Some(NetworkVlanPortTagging::PRIMARY) => {
                                    let entry =
                                        vlan_ports.entry(port.port).or_insert((vlan.vlan, true));
                                    if !entry.1 {
                                        entry.0 = vlan.vlan; // primary overrides untagged
                                    }
                                }
                            }
                        }
                    }
                }
                NetworkInterface::TY => {
                    if let Ok(iface) = cfg.get::<NetworkInterface>() {
                        if iface.proto == InterfaceProto::DHCP {
                            // TODO: better check to see if this is actually an ethernet port
                            all_ports.insert(iface.device.clone());
                            wan_port = Some(iface.device.clone());
                        }
                    }
                }
                _ => (),
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
                            .and_then(|(tag, _)| lookup.get(tag))
                            .cloned(),
                    },
                )
            })
            .collect();
        Ok(Ethernet { wan_port, ports })
    })
}

pub fn update<C: Context>(
    _ctx: C,
    DeserializeStdin(ethernet): DeserializeStdin<Ethernet>,
) -> Result<(), Error> {
    todo!();
    rewrite_config("./etc/config/network", |mut cfg| {
        let mut found_bridge = None;
        while cfg.step() {
            if let Ok(dev) = cfg.get::<NetworkDevice>() {
                if dev.ty == Some(DeviceType::BRIDGE)
                    && (found_bridge.is_none() || dev.name == DEFAULT_LAN_BRIDGE)
                {
                    found_bridge = Some(dev.name);
                }
            }
        }
        cfg.reset();
        Ok(())
    })
}
