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
use uciedit::{dump_all, parse_all, Arena, Configs};

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

fn find_relevant(cfgs: &Configs) -> Result<Vec<(String, WifiInterface, WifiDevice)>, Error> {
    let mut devices = HashMap::new();
    cfgs["wireless"].try_each(|name, device: WifiDevice| {
        devices.insert(
            name.ok_or(ErrorKind::UnnamedWirelessDevice)?.to_string(),
            device,
        );
        Ok::<_, Error>(())
    })?;
    let mut relevant_interfaces: Vec<(String, WifiInterface, WifiDevice)> = Vec::new();
    cfgs["wireless"].try_each(|name, iface: WifiInterface| {
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
    let arena = Arena::new();
    let cfgs = parse_all(
        ctx.uci_root(),
        &arena,
        &["wireless", "startwrt", "network", "firewall"],
    )?;
    get_config(ctx, &cfgs)
}

fn get_config(ctx: impl CtrlContext, cfgs: &Configs) -> Result<Wifi, Error> {
    let lookup = profiles::Lookup::parse(ctx.clone(), cfgs)?;
    let relevant_interfaces = find_relevant(cfgs)?;
    let Some(first_interface) = relevant_interfaces.first() else {
        return Err(ErrorKind::CorruptedWifi.into());
    };
    let mut passwords = BTreeSet::new();
    cfgs["wireless"].try_each(|_, station: WifiStation| {
        if let Some(iface) = &station.iface {
            if !relevant_interfaces.iter().any(|(n, _, _)| n == iface) {
                return Ok(());
            }
        }
        let profile = station.vid.and_then(|vid| lookup.from_vlan(vid)).cloned();
        passwords.insert(Password {
            profile,
            password: station.key,
        });
        Ok::<_, Error>(())
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
}

fn set_config(
    _ctx: &impl CtrlContext,
    cfgs: &mut Configs,
    wifi: &Wifi,
    _lookup: &profiles::Lookup,
) -> Result<(), Error> {
    let mut pending_bands = wifi.bands.clone();
    let mut remaining_admin_passwords = wifi.passwords.iter().filter(|p| p.profile.is_none());
    let first_admin_password = remaining_admin_passwords.next();

    let mut relevant_interfaces = find_relevant(cfgs)?;
    let Some((first_interface_name, first_interface, first_device)) =
        relevant_interfaces.first().cloned()
    else {
        return Err(ErrorKind::CorruptedWifi.into());
    };

    for s in &mut cfgs["wireless"].sections {
        if let Some(mut device) = s.get_typed::<WifiDevice>()? {
            let name = s.name().ok_or(ErrorKind::UnnamedWirelessDevice)?;
            if !relevant_interfaces.iter().any(|(_, i, _)| i.device == name) {
                continue;
            }
            if pending_bands.remove(&device.band) {
                device.disabled = !wifi.enabled;
            } else {
                device.disabled = true;
            }
            s.set(&device)?;
        }
        if let Some(mut iface) = s.get_typed::<WifiInterface>()? {
            let name = s.name().ok_or(ErrorKind::UnnamedWirelessInterface)?;
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
            s.set(&iface)?;
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
        cfgs["wireless"].append(&device, Some(&device_name))?;
        let interface = WifiInterface {
            device: device_name,
            ..first_interface.clone()
        };
        cfgs["wireless"].append(&interface, Some(&interface_name))?;
        relevant_interfaces.push((interface_name, interface, device));
    }

    cfgs["wireless"].sections.retain(|s| {
        if let Ok(station) = s.get::<WifiStation>() {
            if let Some(iface) = &station.iface {
                if !relevant_interfaces.iter().any(|(n, _, _)| n == iface) {
                    return true;
                }
            }
            return false;
        }
        if let Ok(vlan) = s.get::<WifiVlan>() {
            if let Some(iface) = &vlan.iface {
                if !relevant_interfaces.iter().any(|(n, _, _)| n == iface) {
                    return true;
                }
            }
            return false;
        }
        true
    });
    for psswd in remaining_admin_passwords {
        for (niface, _, _) in &relevant_interfaces {
            cfgs["wireless"].append(
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
            cfgs["wireless"].append(
                &WifiVlan {
                    name: profile.interface.clone(),
                    network: profile.interface.clone(),
                    vid: profile.vlan_tag,
                    iface: Some(niface.clone()),
                },
                None,
            )?;
            cfgs["wireless"].append(
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
}

pub fn set<C: CtrlContext>(
    ctx: C,
    DeserializeStdin(wifi): DeserializeStdin<Wifi<ProfileIdOpt>>,
) -> Result<(), Error> {
    let mut retries = 4;
    loop {
        let arena = Arena::new();
        let mut cfgs = parse_all(
            ctx.uci_root(),
            &arena,
            &["wireless", "startwrt", "network", "firewall"],
        )?;
        let lookup = profiles::Lookup::parse(ctx.clone(), &cfgs)?;
        let wifi = Wifi {
            ssid: wifi.ssid.clone(),
            bands: wifi.bands.clone(),
            enabled: wifi.enabled,
            broadcast: wifi.broadcast,
            passwords: wifi
                .passwords
                .iter()
                .map(|pass| {
                    Ok(Password {
                        profile: match &pass.profile {
                            Some(p) => Some(lookup.resolve(p)?.clone()),
                            None => None,
                        },
                        password: pass.password.clone(),
                    })
                })
                .collect::<Result<_, Error>>()?,
        };
        let res = set_config(&ctx, &mut cfgs, &wifi, &lookup);
        match res {
            Err(Error {
                kind: ErrorKind::CorruptedWifi,
                ..
            }) if ctx.effectful() => {
                // try recreating the config from scratch
                let _ = std::fs::remove_file(ctx.uci_root().join("wireless"));
                let _ = Command::new("wifi")
                    .arg("config")
                    .spawn()
                    .context("executing `wifi config`")?
                    .wait();
                continue;
            }
            Err(err) => return Err(err),
            Ok(()) => (),
        }
        match dump_all(ctx.uci_root(), cfgs) {
            Err(uciedit::Error::Conflict { .. }) if retries > 0 => {
                retries -= 1;
                continue;
            }
            Err(err) => return Err(err.into()),
            Ok(()) => {
                if ctx.effectful() {
                    let _ = Command::new("wifi")
                        .arg("reload")
                        .spawn()
                        .context("executing `wifi reload`")?
                        .wait();
                }
                return Ok(());
            }
        }
    }
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
