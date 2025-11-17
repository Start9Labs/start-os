use crate::profiles::{self, ProfileId, ProfileIdOpt};
use crate::utils::{DeserializeStdin, HandlerExtSerde as _};
use crate::{CtrlContext, Error, ErrorKind};
use color_eyre::eyre::Context;
use rpc_toolkit::{from_fn, ParentHandler};
use serde::{Deserialize, Serialize};
use std::collections::{BTreeMap, BTreeSet, HashMap};
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

#[derive(Debug, Serialize, Deserialize, Clone)]
pub struct WifiRadio {
    pub band: String,
    pub enabled: bool,
    pub broadcast: bool,
}

#[derive(Debug, Serialize, Deserialize)]
pub struct Wifi<Id: Ord = ProfileId> {
    pub ssid: String,
    pub radios: BTreeMap<String, WifiRadio>,
    pub passwords: BTreeSet<Password<Id>>,
}

pub fn wifi<C: CtrlContext + Clone>() -> ParentHandler<C> {
    ParentHandler::new()
        .subcommand("get", from_fn(get::<C>).with_display_serializable())
        .subcommand("set", from_fn(set::<C>).with_display_serializable())
        .subcommand("edit", from_fn(edit::<C>).with_display_serializable())
}

type Relevant = (String, WifiInterface, WifiDevice, Option<WifiRadio>);

fn find_relevant_with_radios(
    cfgs: &Configs,
    radios: &BTreeMap<String, WifiRadio>,
) -> Result<Vec<(String, WifiInterface, WifiDevice, WifiRadio)>, Error> {
    let mut devices = HashMap::new();
    cfgs["wireless"].try_each(|name, device: WifiDevice| {
        devices.insert(
            name.ok_or(ErrorKind::UnnamedWirelessDevice)?.to_string(),
            device,
        );
        Ok::<_, Error>(())
    })?;
    let mut relevant_interfaces = Vec::<(String, WifiInterface, WifiDevice, WifiRadio)>::new();
    cfgs["wireless"].try_each(|name, iface: WifiInterface| {
        if iface.mode != WifiMode::AP {
            return Ok(());
        }
        let Some(device) = devices.get(&*iface.device) else {
            return Ok(());
        };
        let name = name.ok_or(ErrorKind::UnnamedWirelessInterface)?.to_string();
        let radio = match radios.get(&name) {
            None => return Ok(()),
            Some(r) => r.clone(),
        };
        relevant_interfaces.push((name, iface, device.clone(), radio));
        Ok::<_, Error>(())
    })?;
    return Ok(relevant_interfaces);
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
    let mut relevant_interfaces = Vec::<(String, WifiInterface, WifiDevice)>::new();
    cfgs["wireless"].try_each(|name, iface: WifiInterface| {
        if iface.mode != WifiMode::AP {
            return Ok(());
        }
        let Some(device) = devices.get(&*iface.device) else {
            return Ok(());
        };
        let name = name.ok_or(ErrorKind::UnnamedWirelessInterface)?.to_string();
        if let Some(first_interface) = relevant_interfaces.first() {
            if first_interface.1.ssid != iface.ssid {
                return Ok(());
            }
        };
        relevant_interfaces.push((name, iface, device.clone()));
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
    let mut radios = BTreeMap::new();
    for (name, iface, device) in &relevant_interfaces {
        radios.insert(
            name.clone(),
            WifiRadio {
                band: device.band.clone(),
                enabled: !device.disabled,
                broadcast: !device.disabled && !iface.hidden,
            },
        );
        if iface.dynamic_vlan != WifiDynamicVlan::REQUIRED {
            if let Some(key) = &iface.key {
                passwords.insert(Password {
                    profile: None,
                    password: key.clone(),
                });
            }
        }
    }
    Ok(Wifi {
        ssid: first_interface.1.ssid.clone(),
        radios,
        passwords,
    })
}

fn set_config(
    _ctx: &impl CtrlContext,
    cfgs: &mut Configs,
    wifi: &Wifi,
    _lookup: &profiles::Lookup,
) -> Result<(), Error> {
    let mut remaining_admin_passwords = wifi.passwords.iter().filter(|p| p.profile.is_none());
    let first_admin_password = remaining_admin_passwords.next();

    let relevant_interfaces = find_relevant_with_radios(cfgs, &wifi.radios)?;
    for s in &mut cfgs["wireless"].sections {
        if let Some(mut device) = s.get_typed::<WifiDevice>()? {
            let name = s.name().ok_or(ErrorKind::UnnamedWirelessDevice)?;
            for (_, rel_iface, _, rel_radio) in &relevant_interfaces {
                if rel_iface.device == name {
                    device.disabled = !rel_radio.enabled;
                    device.band = rel_radio.band.clone();
                    s.set(&device)?;
                    break;
                }
            }
        }
        if let Some(mut iface) = s.get_typed::<WifiInterface>()? {
            let name = s.name().ok_or(ErrorKind::UnnamedWirelessInterface)?;
            for (rel_name, _, _, rel_radio) in &relevant_interfaces {
                if rel_name == &*name {
                    iface.encryption = "psk2".into();
                    iface.ssid = wifi.ssid.clone();
                    if let Some(key) = first_admin_password {
                        iface.key = Some(key.password.clone());
                        iface.dynamic_vlan = WifiDynamicVlan::ALLOWED;
                    } else {
                        iface.key = None;
                        iface.dynamic_vlan = WifiDynamicVlan::REQUIRED;
                    }
                    iface.hidden = !rel_radio.broadcast;
                    s.set(&iface)?;
                    break;
                }
            }
        }
    }

    cfgs["wireless"].sections.retain(|s| {
        if let Ok(station) = s.get::<WifiStation>() {
            if let Some(iface) = &station.iface {
                if !relevant_interfaces.iter().any(|(n, _, _, _)| n == iface) {
                    return true;
                }
            }
            return false;
        }
        if let Ok(vlan) = s.get::<WifiVlan>() {
            if let Some(iface) = &vlan.iface {
                if !relevant_interfaces.iter().any(|(n, _, _, _)| n == iface) {
                    return true;
                }
            }
            return false;
        }
        true
    });

    for psswd in remaining_admin_passwords {
        for (niface, _, _, _) in &relevant_interfaces {
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
        for (niface, _, _, _) in &relevant_interfaces {
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
            radios: wifi.radios.clone(),
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
        radios: current_wifi.radios.clone(),
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
