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
    NetworkBridgeVlan, NetworkDevice, NetworkInterface, NetworkVlanPortTagging, WifiDevice,
    WifiDynamicVlan, WifiInterface, WifiMode, WifiStation,
};
use uciedit::UciSection;
use uciedit::{parse_config, rewrite_config};

pub const DEFAULT_LAN_BRIDGE: &str = "br-lan";

#[derive(Debug, Serialize, Deserialize, PartialEq, Eq, Hash)]
pub struct Password {
    pub profile: Option<ProfileIdAndName>,
    pub password: String,
}

#[derive(Debug, Serialize, Deserialize)]
pub struct Wifi {
    pub ssid: String,
    pub bands: Vec<String>,
    pub enabled: bool,
    pub broadcast: bool,
    pub passwords: HashSet<Password>,
}

pub fn wifi<C: Context>() -> ParentHandler<C> {
    ParentHandler::new()
        .subcommand("get", from_fn(get::<C>).with_display_serializable())
        .subcommand("update", from_fn(update::<C>).with_display_serializable())
}

pub fn get<C: Context>(ctx: C) -> Result<Wifi, Error> {
    let profile_list = profiles::list(ctx)?;
    let lookup: HashMap<u16, ProfileIdAndName> = profile_list
        .into_iter()
        .map(|id| (id.vlan_tag.expect("list should provide vlan_tags"), id))
        .collect();
    parse_config("./etc/config/wireless", |mut ctx| {
        let mut devices = HashMap::new();
        while ctx.step() {
            if ctx.ty() != WifiDevice::TY {
                continue;
            }
            let dev = ctx.get::<WifiDevice>()?;
            devices.insert(
                ctx.name()
                    .ok_or_eyre("wifi devices should all be named")?
                    .into_owned(),
                dev,
            );
        }
        let mut relevant_interfaces: Vec<(WifiInterface, &WifiDevice)> = Vec::new();
        ctx.reset();
        while ctx.step() {
            if ctx.ty() != WifiInterface::TY {
                continue;
            }
            let iface = ctx.get::<WifiInterface>()?;
            if iface.mode != WifiMode::AP {
                continue;
            }
            let Some(device) = devices.get(&iface.device) else {
                continue;
            };
            if let Some(first_interface) = relevant_interfaces.first() {
                if first_interface.0.ssid != iface.ssid {
                    continue;
                }
            }
            relevant_interfaces.push((iface, device));
        }
        let Some(first_interface) = relevant_interfaces.first() else {
            return Err(ErrorKind::CorruptedWifi.into());
        };
        let mut passwords = HashSet::new();
        ctx.reset();
        while ctx.step() {
            if ctx.ty() != WifiStation::TY {
                continue;
            }
            let station = ctx.get::<WifiStation>()?;
            let profile = station.vid.and_then(|vid| lookup.get(&vid)).cloned();
            passwords.insert(Password {
                profile,
                password: station.key,
            });
        }
        for (iface, _) in &relevant_interfaces {
            if iface.dynamic_vlan == WifiDynamicVlan::REQUIRED {
                continue;
            }
            let Some(key) = &iface.key else { continue };
            passwords.insert(Password {
                profile: None,
                password: key.clone(),
            });
        }
        Ok(Wifi {
            ssid: first_interface.0.ssid.clone(),
            bands: relevant_interfaces
                .iter()
                .filter(|(_, d)| !d.disabled)
                .map(|(_, dev)| dev.band.clone())
                .collect(),
            enabled: relevant_interfaces.iter().any(|(_, d)| !d.disabled),
            broadcast: relevant_interfaces
                .iter()
                .any(|(i, d)| !d.disabled && !i.hidden),
            passwords,
        })
    })
}

pub fn update<C: Context>(
    _ctx: C,
    DeserializeStdin(wifi): DeserializeStdin<Wifi>,
) -> Result<(), Error> {
    todo!();
}
