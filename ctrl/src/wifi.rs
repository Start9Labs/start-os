use crate::profiles::{self, ProfileId, ProfileIdOpt};
use crate::utils::{DeserializeStdin, HandlerExtSerde as _};
use crate::{CtrlContext, Error, ErrorKind};
use color_eyre::eyre::Context;
use rpc_toolkit::{from_fn, ParentHandler};
use serde::{Deserialize, Serialize};
use std::collections::{BTreeSet, HashMap};
use std::process::Command;
use uciedit::openwrt::{
    WifiChannel, WifiDevice, WifiDynamicVlan, WifiInterface, WifiMode, WifiStation, WifiVlan,
};
use uciedit::{parse_config, rewrite_config, Sections};

pub const DEFAULT_LAN_BRIDGE: &str = "br-lan";

#[derive(Debug, Serialize, Deserialize, PartialEq, Eq, PartialOrd, Ord)]
pub struct Password<Id: Ord> {
    pub profile: Option<Id>,
    pub password: String,
}

#[derive(Debug, Serialize, Deserialize)]
pub struct Wifi<Id: Ord = ProfileId> {
    pub ssid: String,
    pub bands: BTreeSet<String>,
    pub enabled: bool,
    pub broadcast: bool,
    pub passwords: BTreeSet<Password<Id>>,
}

pub fn wifi<C: CtrlContext + Clone>() -> ParentHandler<C> {
    ParentHandler::new()
        .subcommand("get", from_fn(get::<C>).with_display_serializable())
        .subcommand("set", from_fn(set::<C>).with_display_serializable())
        .subcommand("edit", from_fn(edit::<C>).with_display_serializable())
}

fn find_relevant(cfg: &mut Sections) -> Result<Vec<(String, WifiInterface, WifiDevice)>, Error> {
    let mut devices = HashMap::new();
    cfg.try_each(|name, device: WifiDevice| {
        devices.insert(
            name.ok_or(ErrorKind::UnnamedWirelessDevice)?.to_string(),
            device,
        );
        Ok::<_, Error>(())
    })?;
    let mut relevant_interfaces: Vec<(String, WifiInterface, WifiDevice)> = Vec::new();
    cfg.try_each(|name, iface: WifiInterface| {
        if iface.mode != WifiMode::AP {
            return Ok(());
        }
        let Some(device) = devices.get(&*iface.device) else {
            return Ok(());
        };
        if let Some(first_interface) = relevant_interfaces.first() {
            if first_interface.1.ssid != iface.ssid {
                return Ok(());
            }
        }
        relevant_interfaces.push((
            name.ok_or(ErrorKind::UnnamedWirelessInterface)?.to_string(),
            iface,
            device.clone(),
        ));
        Ok::<_, Error>(())
    })?;
    return Ok(relevant_interfaces);
}

pub fn get<C: CtrlContext>(ctx: C) -> Result<Wifi, Error> {
    let lookup = profiles::Lookup::parse(ctx.clone())?;
    parse_config(ctx.uci_path("wireless"), |mut ctx| {
        let relevant_interfaces = find_relevant(&mut ctx)?;
        let Some(first_interface) = relevant_interfaces.first() else {
            return Err(ErrorKind::CorruptedWifi.into());
        };
        let mut passwords = BTreeSet::new();
        ctx.each(|_, station: WifiStation| {
            if let Some(iface) = &station.iface {
                if !relevant_interfaces.iter().any(|(n, _, _)| n == iface) {
                    return;
                }
            }
            let profile = station.vid.and_then(|vid| lookup.from_vlan(vid)).cloned();
            passwords.insert(Password {
                profile,
                password: station.key,
            });
        })?;
        for (_, iface, _) in &relevant_interfaces {
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
            ssid: first_interface.1.ssid.clone(),
            bands: relevant_interfaces
                .iter()
                .filter(|(_, _, d)| !d.disabled)
                .map(|(_, _, dev)| dev.band.clone())
                .collect(),
            enabled: relevant_interfaces.iter().any(|(_, _, d)| !d.disabled),
            broadcast: relevant_interfaces
                .iter()
                .any(|(_, i, d)| !d.disabled && !i.hidden),
            passwords,
        })
    })
}

fn update_inner(
    ctx: &impl CtrlContext,
    wifi: &Wifi,
    lookup: &profiles::Lookup,
) -> Result<(), Error> {
    let mut pending_bands = wifi.bands.clone();
    let mut remaining_admin_passwords = wifi.passwords.iter().filter(|p| p.profile.is_none());
    let first_admin_password = remaining_admin_passwords.next();
    rewrite_config(ctx.uci_path("wireless"), |mut ctx| {
        let mut relevant_interfaces = find_relevant(&mut ctx.readonly())?;
        let Some((first_interface_name, first_interface, first_device)) =
            relevant_interfaces.first().cloned()
        else {
            return Err(ErrorKind::CorruptedWifi.into());
        };
        ctx.restart();
        while ctx.step() {
            if let Some(mut device) = ctx.get_typed::<WifiDevice>()? {
                let name = ctx.name().ok_or(ErrorKind::UnnamedWirelessDevice)?;
                if !relevant_interfaces.iter().any(|(_, i, _)| i.device == name) {
                    continue;
                }
                if pending_bands.remove(&device.band) {
                    device.disabled = !wifi.enabled;
                } else {
                    device.disabled = true;
                }
                ctx.set(&device)?;
            }
            if let Some(mut iface) = ctx.get_typed::<WifiInterface>()? {
                let name = ctx.name().ok_or(ErrorKind::UnnamedWirelessInterface)?;
                if !relevant_interfaces.iter().any(|(n, _, _)| n == &name) {
                    continue;
                }
                iface.encryption = "psk2".into();
                iface.hidden = !wifi.broadcast;
                iface.ssid = wifi.ssid.clone();
                if let Some(key) = first_admin_password {
                    iface.key = Some(key.password.clone());
                    iface.dynamic_vlan = WifiDynamicVlan::ALLOWED;
                } else {
                    iface.key = None;
                    iface.dynamic_vlan = WifiDynamicVlan::REQUIRED;
                }
                ctx.set(&iface)?;
            }
        }
        for band in pending_bands {
            let device_name = format!(
                "{}{}",
                first_interface
                    .device
                    .trim_end_matches(|c: char| c.is_numeric()),
                band
            );
            let interface_name = format!(
                "{}{}",
                first_interface_name.trim_end_matches(|c: char| c.is_numeric()),
                band
            );
            let device = WifiDevice {
                band,
                channel: WifiChannel::Auto,
                ..first_device.clone()
            };
            ctx.push(&device, Some(&device_name))?;
            let interface = WifiInterface {
                device: device_name,
                ..first_interface.clone()
            };
            ctx.push(&interface, Some(&interface_name))?;
            relevant_interfaces.push((interface_name, interface, device));
        }
        ctx.restart();
        while ctx.step() {
            if let Some(station) = ctx.get_typed::<WifiStation>()? {
                if let Some(iface) = &station.iface {
                    if !relevant_interfaces.iter().any(|(n, _, _)| n == iface) {
                        continue;
                    }
                }
                ctx.remove();
            }
            if let Some(vlan) = ctx.get_typed::<WifiVlan>()? {
                if let Some(iface) = &vlan.iface {
                    if !relevant_interfaces.iter().any(|(n, _, _)| n == iface) {
                        continue;
                    }
                }
                ctx.remove();
            }
        }
        for psswd in remaining_admin_passwords {
            for (niface, _, _) in &relevant_interfaces {
                ctx.push(
                    &WifiStation {
                        key: psswd.password.clone(),
                        vid: None,
                        iface: Some(niface.clone()),
                    },
                    None,
                )?;
            }
        }
        for psswd in &wifi.passwords {
            let Some(profile) = &psswd.profile else {
                // admin passwords are handeled above
                continue;
            };
            for (niface, _, _) in &relevant_interfaces {
                ctx.push(
                    &WifiVlan {
                        name: profile.interface.clone(),
                        network: profile.interface.clone(),
                        vid: profile.vlan_tag,
                        iface: Some(niface.clone()),
                    },
                    None,
                )?;
                ctx.push(
                    &WifiStation {
                        key: psswd.password.clone(),
                        vid: Some(profile.vlan_tag),
                        iface: Some(niface.clone()),
                    },
                    None,
                )?;
            }
        }
        Ok(())
    })
}

pub fn set<C: CtrlContext>(
    ctx: C,
    DeserializeStdin(wifi): DeserializeStdin<Wifi<ProfileIdOpt>>,
) -> Result<(), Error> {
    let lookup = profiles::Lookup::parse(ctx.clone())?;
    let wifi = Wifi {
        ssid: wifi.ssid,
        bands: wifi.bands,
        enabled: wifi.enabled,
        broadcast: wifi.broadcast,
        passwords: wifi
            .passwords
            .into_iter()
            .map(|pass| {
                Ok(Password {
                    profile: match pass.profile {
                        Some(p) => Some(lookup.resolve(&p)?.clone()),
                        None => None,
                    },
                    password: pass.password,
                })
            })
            .collect::<Result<_, Error>>()?,
    };
    let res = update_inner(&ctx, &wifi, &lookup);
    match res {
        Err(Error {
            kind: ErrorKind::CorruptedWifi,
            ..
        }) => {
            // try recreating the config from scratch
            let _ = std::fs::remove_file(ctx.uci_path("wireless"));
            let _ = Command::new("wifi")
                .arg("config")
                .spawn()
                .context("executing `wifi config`")?
                .wait();
            update_inner(&ctx, &wifi, &lookup)?
        }
        Err(err) => return Err(err),
        Ok(()) => (),
    }
    let _ = Command::new("wifi")
        .arg("reload")
        .spawn()
        .context("executing `wifi reload`")?
        .wait();
    Ok(())
}

pub fn edit<C: CtrlContext + Clone>(ctx: C) -> Result<(), Error> {
    let current_wifi = get(ctx.clone())?;
    let current_wifi = Wifi {
        ssid: current_wifi.ssid,
        bands: current_wifi.bands,
        enabled: current_wifi.enabled,
        broadcast: current_wifi.broadcast,
        passwords: current_wifi
            .passwords
            .into_iter()
            .map(|pass| Password {
                profile: pass.profile.map(Into::into),
                password: pass.password,
            })
            .collect(),
    };
    let modified_wifi = crate::utils::edit_in_editor(&current_wifi)?;
    set(ctx, DeserializeStdin(modified_wifi))
}
