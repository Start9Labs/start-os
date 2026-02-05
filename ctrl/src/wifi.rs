use crate::profiles::{self, ProfileId, ProfileIdOpt};
use crate::utils::{DeserializeStdin, HandlerExtSerde as _};
use crate::{CtrlContext, Error, ErrorKind};
use color_eyre::eyre::Context;
use rpc_toolkit::{from_fn, ParentHandler};
use serde::{Deserialize, Serialize};
use std::collections::{BTreeMap, BTreeSet, HashMap};
use std::path::PathBuf;
use std::process::Command;
use uciedit::openwrt::{
    WifiChannel, WifiDevice, WifiDynamicVlan, WifiInterface, WifiMode, WifiStation, WifiVlan,
};
use uciedit::{dump_all, parse_all, Arena, Configs};

pub const DEFAULT_LAN_BRIDGE: &str = "br-lan";

#[derive(Debug, Serialize, Deserialize, PartialEq, Eq, PartialOrd, Ord)]
pub struct Password<Id: Ord> {
    pub label: String,
    pub profile: Option<Id>,
    pub password: String,
}

#[derive(Debug, Serialize, Deserialize, Clone)]
pub struct WifiRadio {
    pub band: String,
    pub channel: String,
    pub enabled: bool,
    pub broadcast: bool,
}

#[derive(Debug, Serialize, Deserialize)]
pub struct Wifi<Id: Ord = ProfileId> {
    pub ssid: String,
    pub radios: BTreeMap<String, WifiRadio>,
    pub passwords: BTreeSet<Password<Id>>,
}

#[derive(Debug, Serialize, Deserialize, Clone)]
#[serde(rename_all = "camelCase")]
pub struct BlackoutWindow {
    pub start_time: String,
    pub end_time: String,
    pub days: [bool; 7],
}

#[derive(Debug, Serialize, Deserialize, Clone)]
pub struct BlackoutWindows {
    pub windows: Vec<BlackoutWindow>,
}

pub fn wifi<C: CtrlContext + Clone>() -> ParentHandler<C> {
    ParentHandler::new()
        .subcommand("get", from_fn(get::<C>).with_display_serializable())
        .subcommand("set", from_fn(set::<C>).with_display_serializable())
        .subcommand("edit", from_fn(edit::<C>).with_display_serializable())
        .subcommand(
            "blackout-get",
            from_fn(blackout_get::<C>).with_display_serializable(),
        )
        .subcommand(
            "blackout-set",
            from_fn(blackout_set::<C>).with_display_serializable(),
        )
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
        let label = profile
            .as_ref()
            .map(|p| p.fullname.clone())
            .unwrap_or_default();
        passwords.insert(Password {
            label,
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
                channel: device.channel.to_string(),
                enabled: !device.disabled,
                broadcast: !device.disabled && !iface.hidden,
            },
        );
        if iface.dynamic_vlan != WifiDynamicVlan::REQUIRED {
            if let Some(key) = &iface.key {
                passwords.insert(Password {
                    label: String::new(),
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
                    device.channel = match rel_radio.channel.as_str() {
                        "auto" | "Auto" => WifiChannel::Auto,
                        n => n.parse::<u32>().map(WifiChannel::Int).unwrap_or(WifiChannel::Auto),
                    };
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
                        label: pass.label.clone(),
                        profile: match &pass.profile {
                            Some(p) => Some(lookup.resolve(p)?.clone()),
                            None => None,
                        },
                        password: pass.password.clone(),
                    })
                })
                .collect::<Result<_, Error>>()?,
        };
        let mut seen_passwords = std::collections::HashSet::new();
        let mut seen_labels = std::collections::HashSet::new();
        for pass in &wifi.passwords {
            if !seen_passwords.insert(&pass.password) {
                return Err(ErrorKind::DuplicatePassword.into());
            }
            if !seen_labels.insert(&pass.label) {
                return Err(ErrorKind::DuplicatePasswordLabel.into());
            }
        }
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
                label: pass.label,
                profile: pass.profile.map(Into::into),
                password: pass.password,
            })
            .collect(),
    };
    let modified_wifi = crate::utils::edit_in_editor(&current_wifi)?;
    set(ctx, DeserializeStdin(modified_wifi))
}

const BLACKOUT_TAG: &str = "# start-wrt-wifi-blackout";
const CRONTAB_PATH: &str = "/etc/crontabs/root";

fn crontab_path(ctx: &impl CtrlContext) -> PathBuf {
    if ctx.effectful() {
        PathBuf::from(CRONTAB_PATH)
    } else {
        ctx.uci_root().join("crontab_root")
    }
}

fn parse_hhmm(s: &str) -> Result<(u32, u32), Error> {
    let parts: Vec<&str> = s.split(':').collect();
    if parts.len() != 2 {
        return Err(Error::other(format!("invalid time format: {s:?}")));
    }
    let h: u32 = parts[0]
        .parse()
        .map_err(|_| Error::other(format!("invalid hour: {}", parts[0])))?;
    let m: u32 = parts[1]
        .parse()
        .map_err(|_| Error::other(format!("invalid minute: {}", parts[1])))?;
    Ok((h, m))
}

pub fn blackout_get<C: CtrlContext>(ctx: C) -> Result<Vec<BlackoutWindow>, Error> {
    let path = crontab_path(&ctx);
    let content = std::fs::read_to_string(&path).unwrap_or_default();

    let tagged: Vec<&str> = content
        .lines()
        .filter(|l| l.contains(BLACKOUT_TAG))
        .collect();

    // Collect down/up line pairs
    let down_lines: Vec<&str> = tagged.iter().filter(|l| l.contains("wifi down")).copied().collect();
    let up_lines: Vec<&str> = tagged.iter().filter(|l| l.contains("wifi up")).copied().collect();

    let mut windows = Vec::new();
    for (down, up) in down_lines.iter().zip(up_lines.iter()) {
        let down_parts: Vec<&str> = down.split_whitespace().collect();
        let up_parts: Vec<&str> = up.split_whitespace().collect();

        let start_time = if down_parts.len() >= 2 {
            format!(
                "{:02}:{:02}",
                down_parts[1].parse::<u32>().unwrap_or(0),
                down_parts[0].parse::<u32>().unwrap_or(0)
            )
        } else {
            continue;
        };

        let end_time = if up_parts.len() >= 2 {
            format!(
                "{:02}:{:02}",
                up_parts[1].parse::<u32>().unwrap_or(0),
                up_parts[0].parse::<u32>().unwrap_or(0)
            )
        } else {
            continue;
        };

        let mut days = [false; 7];
        if down_parts.len() >= 5 {
            for d in down_parts[4].split(',') {
                if let Ok(n) = d.parse::<usize>() {
                    if n < 7 {
                        days[n] = true;
                    }
                }
            }
        }

        windows.push(BlackoutWindow {
            start_time,
            end_time,
            days,
        });
    }

    Ok(windows)
}

pub fn blackout_set<C: CtrlContext>(
    ctx: C,
    DeserializeStdin(input): DeserializeStdin<BlackoutWindows>,
) -> Result<(), Error> {
    let path = crontab_path(&ctx);
    let content = std::fs::read_to_string(&path).unwrap_or_default();

    let filtered: Vec<&str> = content
        .lines()
        .filter(|l| !l.contains(BLACKOUT_TAG))
        .collect();

    let mut new_content = filtered.join("\n");
    if !new_content.is_empty() && !new_content.ends_with('\n') {
        new_content.push('\n');
    }

    for window in &input.windows {
        let days_str: String = window
            .days
            .iter()
            .enumerate()
            .filter(|(_, &on)| on)
            .map(|(i, _)| i.to_string())
            .collect::<Vec<_>>()
            .join(",");

        if days_str.is_empty() {
            continue;
        }

        let (start_h, start_m) = parse_hhmm(&window.start_time)?;
        let (end_h, end_m) = parse_hhmm(&window.end_time)?;

        new_content.push_str(&format!(
            "{} {} * * {} wifi down {}\n",
            start_m, start_h, days_str, BLACKOUT_TAG
        ));
        new_content.push_str(&format!(
            "{} {} * * {} wifi up {}\n",
            end_m, end_h, days_str, BLACKOUT_TAG
        ));
    }

    if let Some(parent) = path.parent() {
        std::fs::create_dir_all(parent)?;
    }
    std::fs::write(&path, &new_content)?;

    if ctx.effectful() {
        let _ = Command::new("/etc/init.d/cron")
            .arg("restart")
            .spawn()
            .context("restarting cron")?
            .wait();
    }

    Ok(())
}
