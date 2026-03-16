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
    NetworkBridgeVlan, WifiChannel, WifiDevice, WifiDynamicVlan, WifiInterface, WifiMode,
    WifiStation, WifiVlan,
};
use uciedit::{dump_all, parse_all, Arena, Configs};

pub const DEFAULT_LAN_BRIDGE: &str = "br-lan";

/// Whether wifi needs a full restart or just a PSK hot-reload.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum WifiRestart {
    /// New VLANs or interface config changes — full restart needed
    Full,
    /// Only PSK entries changed — hot reload sufficient
    PskOnly,
}

/// Returns the UCI section names of all wifi-iface sections in AP mode.
pub fn find_ap_interface_names(cfgs: &Configs) -> Result<Vec<String>, Error> {
    let mut names = Vec::new();
    cfgs["wireless"].try_each(|name, iface: WifiInterface| {
        if iface.mode == WifiMode::AP {
            names.push(name.ok_or(ErrorKind::UnnamedWirelessInterface)?.to_string());
        }
        Ok::<_, Error>(())
    })?;
    Ok(names)
}

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
#[serde(rename_all = "camelCase")]
pub struct Wifi<Id: Ord = ProfileId> {
    pub ssid: String,
    pub broadcast_separately: bool,
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
        let label = station
            .label
            .filter(|l| !l.is_empty())
            .unwrap_or_else(|| {
                profile
                    .as_ref()
                    .map(|p| p.fullname.clone())
                    .unwrap_or_default()
            });
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
                if !passwords.iter().any(|p| p.profile.is_none()) {
                    passwords.insert(Password {
                        label: "Default".to_string(),
                        profile: None,
                        password: key.clone(),
                    });
                }
            }
        }
    }
    let all_ssids_match = relevant_interfaces
        .iter()
        .all(|(_, iface, _)| iface.ssid == first_interface.1.ssid);
    let broadcast_separately = !all_ssids_match;
    // Use the 2.4GHz radio's SSID as the base name, falling back to first
    let ssid = relevant_interfaces
        .iter()
        .find(|(_, _, dev)| dev.band == "2g")
        .map(|(_, iface, _)| iface.ssid.clone())
        .unwrap_or_else(|| first_interface.1.ssid.clone());

    Ok(Wifi {
        ssid,
        broadcast_separately,
        radios,
        passwords,
    })
}

/// Determines whether a full wifi restart or PSK-only reload is needed.
fn set_config(
    _ctx: &impl CtrlContext,
    cfgs: &mut Configs,
    wifi: &Wifi,
    _lookup: &profiles::Lookup,
) -> Result<WifiRestart, Error> {
    let first_admin_password = wifi.passwords.iter().find(|p| p.profile.is_none());

    let relevant_interfaces = find_relevant_with_radios(cfgs, &wifi.radios)?;
    let mut iface_config_changed = false;
    for s in &mut cfgs["wireless"].sections {
        if let Some(mut device) = s.get_typed::<WifiDevice>()? {
            let name = s.name().ok_or(ErrorKind::UnnamedWirelessDevice)?;
            for (_, rel_iface, _, rel_radio) in &relevant_interfaces {
                if rel_iface.device == name {
                    let new_disabled = !rel_radio.enabled;
                    let new_channel = match rel_radio.channel.as_str() {
                        "auto" | "Auto" => WifiChannel::Auto,
                        n => n.parse::<u32>().map(WifiChannel::Int).unwrap_or(WifiChannel::Auto),
                    };
                    if device.disabled != new_disabled
                        || device.channel != new_channel
                        || device.band != rel_radio.band
                    {
                        iface_config_changed = true;
                    }
                    device.disabled = new_disabled;
                    device.band = rel_radio.band.clone();
                    device.channel = new_channel;
                    s.set(&device)?;
                    break;
                }
            }
        }
        if let Some(mut iface) = s.get_typed::<WifiInterface>()? {
            let name = s.name().ok_or(ErrorKind::UnnamedWirelessInterface)?;
            for (rel_name, _, rel_device, rel_radio) in &relevant_interfaces {
                if rel_name == &*name {
                    let new_ssid = if wifi.broadcast_separately && rel_device.band == "5g" {
                        format!("{}-5G", wifi.ssid)
                    } else {
                        wifi.ssid.clone()
                    };
                    let new_hidden = !rel_radio.broadcast;
                    let (new_key, new_dynamic_vlan) = if let Some(key) = first_admin_password {
                        (Some(key.password.clone()), WifiDynamicVlan::ALLOWED)
                    } else {
                        (None, WifiDynamicVlan::REQUIRED)
                    };
                    if iface.ssid != new_ssid
                        || iface.hidden != new_hidden
                        || iface.encryption != "psk2"
                        || iface.key != new_key
                        || iface.dynamic_vlan != new_dynamic_vlan
                    {
                        iface_config_changed = true;
                    }
                    iface.encryption = "psk2".into();
                    iface.ssid = new_ssid;
                    iface.key = new_key;
                    iface.dynamic_vlan = new_dynamic_vlan;
                    iface.hidden = new_hidden;
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
        // wifi-vlan sections are managed by profiles — don't remove them here
        true
    });

    for psswd in wifi.passwords.iter().filter(|p| p.profile.is_none()) {
        for (niface, _, _, _) in &relevant_interfaces {
            cfgs["wireless"].append(
                &WifiStation {
                    key: psswd.password.clone(),
                    vid: None,
                    iface: Some(niface.clone()),
                    label: Some(psswd.label.clone()),
                },
                None,
            )?;
        }
    }
    for psswd in &wifi.passwords {
        let Some(profile) = &psswd.profile else {
            // admin passwords are handled above
            continue;
        };
        for (niface, _, _, _) in &relevant_interfaces {
            cfgs["wireless"].append(
                &WifiStation {
                    key: psswd.password.clone(),
                    vid: Some(profile.vlan_tag),
                    iface: Some(niface.clone()),
                    label: Some(psswd.label.clone()),
                },
                None,
            )?;
        }
    }

    // Fallback: create missing wifi-vlans (e.g., profile created before this code existed)
    let mut existing_wifi_vlans: BTreeSet<(u16, String)> = cfgs["wireless"]
        .sections
        .iter()
        .filter_map(|s| {
            let vlan = s.get::<WifiVlan>().ok()?;
            Some((vlan.vid, vlan.iface?.clone()))
        })
        .collect();

    let bridge_vlan_tags: BTreeSet<u16> = cfgs["network"]
        .sections
        .iter()
        .filter_map(|s| s.get::<NetworkBridgeVlan>().ok().map(|v| v.vlan))
        .collect();

    let mut vlans_created = false;
    for psswd in &wifi.passwords {
        let Some(profile) = &psswd.profile else { continue; };
        if !bridge_vlan_tags.contains(&profile.vlan_tag) { continue; }
        for (niface, _, _, _) in &relevant_interfaces {
            let key = (profile.vlan_tag, niface.clone());
            if existing_wifi_vlans.insert(key) {
                cfgs["wireless"].append(
                    &WifiVlan {
                        name: profile.interface.clone(),
                        network: profile.interface.clone(),
                        vid: profile.vlan_tag,
                        iface: Some(niface.clone()),
                    },
                    None,
                )?;
                vlans_created = true;
            }
        }
    }

    if vlans_created || iface_config_changed {
        Ok(WifiRestart::Full)
    } else {
        Ok(WifiRestart::PskOnly)
    }
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
            broadcast_separately: wifi.broadcast_separately,
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
            if !pass.label.is_empty() && !seen_labels.insert(&pass.label) {
                return Err(ErrorKind::DuplicatePasswordLabel.into());
            }
        }
        let restart = match set_config(&ctx, &mut cfgs, &wifi, &lookup) {
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
            Ok(v) => v,
        };
        match dump_all(ctx.uci_root(), cfgs) {
            Err(uciedit::Error::Conflict { .. }) if retries > 0 => {
                retries -= 1;
                continue;
            }
            Err(err) => return Err(err.into()),
            Ok(()) => {
                if ctx.effectful() {
                    match restart {
                        WifiRestart::Full => {
                            // SSID/channel/enabled/hidden changed or new VLANs — full restart
                            let _ = Command::new("wifi")
                                .spawn()
                                .context("executing `wifi`")?
                                .wait();
                        }
                        WifiRestart::PskOnly => {
                            // PSK-only change — fast path, no client disconnection
                            let _ = Command::new("wifi")
                                .arg("reload")
                                .spawn()
                                .context("executing `wifi reload`")?
                                .wait();
                        }
                    }
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
        broadcast_separately: current_wifi.broadcast_separately,
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

#[cfg(test)]
mod tests {
    use super::*;
    use rpc_toolkit::Context;
    use std::sync::Arc;
    use tokio::runtime::Runtime;

    #[derive(Clone)]
    struct TestContext(PathBuf);

    impl Context for TestContext {
        fn runtime(&self) -> Option<Arc<Runtime>> {
            None
        }
    }

    impl CtrlContext for TestContext {
        fn uci_root(&self) -> PathBuf {
            self.0.clone()
        }
        fn effectful(&self) -> bool {
            false
        }
    }

    fn setup_test_configs(dir: &std::path::Path) {
        // startwrt: two profiles — Admin (lan, vlan 99) and Guest (guest, vlan 101)
        std::fs::write(
            dir.join("startwrt"),
            "\
config profile lan
\toption fullname 'Admin'
\toption interface 'lan'
\toption vlan_tag '99'
\toption access_to_new_profiles '1'

config profile guest
\toption fullname 'Guest'
\toption interface 'guest'
\toption vlan_tag '101'
",
        )
        .unwrap();

        // network: bridge device + lan + guest interfaces
        std::fs::write(
            dir.join("network"),
            "\
config device
\toption name 'br-lan'
\toption type 'bridge'
\tlist ports 'eth0'

config interface 'lan'
\toption device 'br-lan.99'
\toption proto 'static'
\toption ipaddr '192.168.1.1'
\toption netmask '255.255.255.0'

config interface 'guest'
\toption device 'br-lan.101'
\toption proto 'static'
\toption ipaddr '192.168.101.1'
\toption netmask '255.255.255.0'

config bridge-vlan
\toption device 'br-lan'
\toption vlan '99'

config bridge-vlan
\toption device 'br-lan'
\toption vlan '101'
",
        )
        .unwrap();

        // firewall: lan + wan zones + forwarding
        std::fs::write(
            dir.join("firewall"),
            "\
config zone
\toption name 'lan'
\tlist network 'lan'
\toption input 'ACCEPT'
\toption output 'ACCEPT'
\toption forward 'ACCEPT'

config zone
\toption name 'wan'
\tlist network 'wan'
\toption input 'REJECT'
\toption output 'ACCEPT'
\toption forward 'REJECT'

config forwarding
\toption src 'lan'
\toption dest 'wan'
",
        )
        .unwrap();
    }

    fn write_wireless_config(dir: &std::path::Path, stations: &str) {
        std::fs::write(
            dir.join("wireless"),
            format!(
                "\
config wifi-device 'radio0'
\toption type 'mac80211'
\toption band '2g'
\toption channel '1'

config wifi-iface 'default_radio0'
\toption device 'radio0'
\toption mode 'ap'
\toption ssid 'TestNet'
\toption encryption 'psk2'
\toption key 'adminpass1'
\toption dynamic_vlan '1'

{stations}"
            ),
        )
        .unwrap();
    }

    fn make_radios() -> BTreeMap<String, WifiRadio> {
        let mut radios = BTreeMap::new();
        radios.insert(
            "default_radio0".into(),
            WifiRadio {
                band: "2g".into(),
                channel: "1".into(),
                enabled: true,
                broadcast: true,
            },
        );
        radios
    }

    fn write_wireless_config_dual_radio(dir: &std::path::Path, ssid_2g: &str, ssid_5g: &str, stations: &str) {
        std::fs::write(
            dir.join("wireless"),
            format!(
                "\
config wifi-device 'radio0'
\toption type 'mac80211'
\toption band '2g'
\toption channel '1'

config wifi-device 'radio1'
\toption type 'mac80211'
\toption band '5g'
\toption channel '36'

config wifi-iface 'default_radio0'
\toption device 'radio0'
\toption mode 'ap'
\toption ssid '{ssid_2g}'
\toption encryption 'psk2'
\toption key 'adminpass1'
\toption dynamic_vlan '1'

config wifi-iface 'default_radio1'
\toption device 'radio1'
\toption mode 'ap'
\toption ssid '{ssid_5g}'
\toption encryption 'psk2'
\toption key 'adminpass1'
\toption dynamic_vlan '1'

{stations}"
            ),
        )
        .unwrap();
    }

    fn make_dual_radios() -> BTreeMap<String, WifiRadio> {
        let mut radios = BTreeMap::new();
        radios.insert(
            "default_radio0".into(),
            WifiRadio {
                band: "2g".into(),
                channel: "1".into(),
                enabled: true,
                broadcast: true,
            },
        );
        radios.insert(
            "default_radio1".into(),
            WifiRadio {
                band: "5g".into(),
                channel: "36".into(),
                enabled: true,
                broadcast: true,
            },
        );
        radios
    }

    #[test]
    fn test_get_reads_label_from_uci() {
        let dir = tempfile::tempdir().unwrap();
        let ctx = TestContext(dir.path().to_path_buf());
        setup_test_configs(dir.path());
        write_wireless_config(
            dir.path(),
            "\
config wifi-station
\toption key 'guestpass1'
\toption vid '101'
\toption iface 'default_radio0'
\toption label 'My Label'
",
        );

        let arena = Arena::new();
        let cfgs = parse_all(
            ctx.uci_root(),
            &arena,
            &["wireless", "startwrt", "network", "firewall"],
        )
        .unwrap();
        let wifi = get_config(ctx, &cfgs).unwrap();

        let labeled = wifi
            .passwords
            .iter()
            .find(|p| p.password == "guestpass1")
            .unwrap();
        assert_eq!(labeled.label, "My Label");
    }

    #[test]
    fn test_get_falls_back_to_profile_name_when_no_label() {
        let dir = tempfile::tempdir().unwrap();
        let ctx = TestContext(dir.path().to_path_buf());
        setup_test_configs(dir.path());
        write_wireless_config(
            dir.path(),
            "\
config wifi-station
\toption key 'guestpass1'
\toption vid '101'
\toption iface 'default_radio0'
",
        );

        let arena = Arena::new();
        let cfgs = parse_all(
            ctx.uci_root(),
            &arena,
            &["wireless", "startwrt", "network", "firewall"],
        )
        .unwrap();
        let wifi = get_config(ctx, &cfgs).unwrap();

        let fallback = wifi
            .passwords
            .iter()
            .find(|p| p.password == "guestpass1")
            .unwrap();
        assert_eq!(fallback.label, "Guest");
    }

    #[test]
    fn test_set_persists_label() {
        let dir = tempfile::tempdir().unwrap();
        let ctx = TestContext(dir.path().to_path_buf());
        setup_test_configs(dir.path());
        write_wireless_config(dir.path(), "");

        let arena = Arena::new();
        let mut cfgs = parse_all(
            ctx.uci_root(),
            &arena,
            &["wireless", "startwrt", "network", "firewall"],
        )
        .unwrap();
        let lookup = profiles::Lookup::parse(ctx.clone(), &cfgs).unwrap();

        let guest_id = lookup.from_fullname("Guest").unwrap().clone();
        let mut passwords = BTreeSet::new();
        passwords.insert(Password {
            label: "Main Admin".into(),
            profile: None,
            password: "adminpass1".into(),
        });
        passwords.insert(Password {
            label: "Kids WiFi".into(),
            profile: Some(guest_id.clone()),
            password: "kidspass1".into(),
        });

        let wifi = Wifi {
            ssid: "TestNet".into(),
            broadcast_separately: false,
            radios: make_radios(),
            passwords,
        };
        set_config(&ctx, &mut cfgs, &wifi, &lookup).unwrap();
        dump_all(ctx.uci_root(), cfgs).unwrap();

        // Re-parse and check label was written
        let arena2 = Arena::new();
        let cfgs2 = parse_all(
            ctx.uci_root(),
            &arena2,
            &["wireless", "startwrt", "network", "firewall"],
        )
        .unwrap();

        let mut found_kids = false;
        cfgs2["wireless"].try_each(|_, station: WifiStation| {
            if station.key == "kidspass1" {
                assert_eq!(station.label.as_deref(), Some("Kids WiFi"));
                found_kids = true;
            }
            Ok::<_, Error>(())
        })
        .unwrap();

        assert!(found_kids, "did not find station with key 'kidspass1'");
    }

    #[test]
    fn test_set_round_trip_preserves_label() {
        let dir = tempfile::tempdir().unwrap();
        let ctx = TestContext(dir.path().to_path_buf());
        setup_test_configs(dir.path());
        write_wireless_config(
            dir.path(),
            "\
config wifi-station
\toption key 'guestpass1'
\toption vid '101'
\toption iface 'default_radio0'
\toption label 'Custom Name'
",
        );

        // GET
        let arena = Arena::new();
        let cfgs = parse_all(
            ctx.uci_root(),
            &arena,
            &["wireless", "startwrt", "network", "firewall"],
        )
        .unwrap();
        let wifi = get_config(ctx.clone(), &cfgs).unwrap();

        let labeled = wifi
            .passwords
            .iter()
            .find(|p| p.password == "guestpass1")
            .unwrap();
        assert_eq!(labeled.label, "Custom Name");

        // SET (round-trip the same data)
        drop(cfgs);
        drop(arena);

        let arena2 = Arena::new();
        let mut cfgs2 = parse_all(
            ctx.uci_root(),
            &arena2,
            &["wireless", "startwrt", "network", "firewall"],
        )
        .unwrap();
        let lookup2 = profiles::Lookup::parse(ctx.clone(), &cfgs2).unwrap();
        set_config(&ctx, &mut cfgs2, &wifi, &lookup2).unwrap();
        dump_all(ctx.uci_root(), cfgs2).unwrap();

        // GET again
        let arena3 = Arena::new();
        let cfgs3 = parse_all(
            ctx.uci_root(),
            &arena3,
            &["wireless", "startwrt", "network", "firewall"],
        )
        .unwrap();
        let wifi2 = get_config(ctx, &cfgs3).unwrap();

        let preserved = wifi2
            .passwords
            .iter()
            .find(|p| p.password == "guestpass1")
            .unwrap();
        assert_eq!(preserved.label, "Custom Name");
    }

    #[test]
    fn test_set_allows_duplicate_empty_labels() {
        // Two admin passwords both with empty label should not trigger DuplicatePasswordLabel
        let mut seen_passwords = std::collections::HashSet::new();
        let mut seen_labels = std::collections::HashSet::new();
        let passwords = vec![
            Password::<ProfileId> {
                label: "".into(),
                profile: None,
                password: "pass1".into(),
            },
            Password {
                label: "".into(),
                profile: None,
                password: "pass2".into(),
            },
        ];
        for pass in &passwords {
            assert!(seen_passwords.insert(&pass.password));
            // This is the actual validation logic from set()
            if !pass.label.is_empty() && !seen_labels.insert(&pass.label) {
                panic!("should not reject duplicate empty labels");
            }
        }
    }

    #[test]
    fn test_set_rejects_duplicate_nonempty_labels() {
        let mut seen_labels = std::collections::HashSet::new();
        let passwords = vec![
            Password::<ProfileId> {
                label: "Same Name".into(),
                profile: None,
                password: "pass1".into(),
            },
            Password {
                label: "Same Name".into(),
                profile: None,
                password: "pass2".into(),
            },
        ];
        let mut found_dup = false;
        for pass in &passwords {
            if !pass.label.is_empty() && !seen_labels.insert(&pass.label) {
                found_dup = true;
            }
        }
        assert!(found_dup, "should reject duplicate non-empty labels");
    }

    #[test]
    fn test_dual_radio_broadcast_separately_detected() {
        let dir = tempfile::tempdir().unwrap();
        let ctx = TestContext(dir.path().to_path_buf());
        setup_test_configs(dir.path());
        write_wireless_config_dual_radio(dir.path(), "TestNet", "TestNet-5G", "");

        let arena = Arena::new();
        let cfgs = parse_all(
            ctx.uci_root(),
            &arena,
            &["wireless", "startwrt", "network", "firewall"],
        )
        .unwrap();
        let wifi = get_config(ctx, &cfgs).unwrap();

        assert!(wifi.broadcast_separately);
        assert_eq!(wifi.ssid, "TestNet"); // 2.4GHz SSID used as base
    }

    #[test]
    fn test_dual_radio_same_ssid_not_broadcast_separately() {
        let dir = tempfile::tempdir().unwrap();
        let ctx = TestContext(dir.path().to_path_buf());
        setup_test_configs(dir.path());
        write_wireless_config_dual_radio(dir.path(), "TestNet", "TestNet", "");

        let arena = Arena::new();
        let cfgs = parse_all(
            ctx.uci_root(),
            &arena,
            &["wireless", "startwrt", "network", "firewall"],
        )
        .unwrap();
        let wifi = get_config(ctx, &cfgs).unwrap();

        assert!(!wifi.broadcast_separately);
        assert_eq!(wifi.ssid, "TestNet");
    }

    #[test]
    fn test_dual_radio_broadcast_separately_round_trip() {
        let dir = tempfile::tempdir().unwrap();
        let ctx = TestContext(dir.path().to_path_buf());
        setup_test_configs(dir.path());
        // Start with same SSID on both radios
        write_wireless_config_dual_radio(dir.path(), "TestNet", "TestNet", "");

        // SET with broadcast_separately = true
        let arena = Arena::new();
        let mut cfgs = parse_all(
            ctx.uci_root(),
            &arena,
            &["wireless", "startwrt", "network", "firewall"],
        )
        .unwrap();
        let lookup = profiles::Lookup::parse(ctx.clone(), &cfgs).unwrap();

        let mut passwords = BTreeSet::new();
        passwords.insert(Password {
            label: "Main Admin".into(),
            profile: None,
            password: "adminpass1".into(),
        });

        let wifi = Wifi {
            ssid: "TestNet".into(),
            broadcast_separately: true,
            radios: make_dual_radios(),
            passwords,
        };
        set_config(&ctx, &mut cfgs, &wifi, &lookup).unwrap();
        dump_all(ctx.uci_root(), cfgs).unwrap();

        // GET — verify 5GHz got "-5G" suffix and broadcast_separately is detected
        let arena2 = Arena::new();
        let cfgs2 = parse_all(
            ctx.uci_root(),
            &arena2,
            &["wireless", "startwrt", "network", "firewall"],
        )
        .unwrap();
        let wifi2 = get_config(ctx.clone(), &cfgs2).unwrap();

        assert!(wifi2.broadcast_separately);
        assert_eq!(wifi2.ssid, "TestNet");

        // SET with broadcast_separately = false — both should get same SSID
        drop(cfgs2);
        drop(arena2);

        let arena3 = Arena::new();
        let mut cfgs3 = parse_all(
            ctx.uci_root(),
            &arena3,
            &["wireless", "startwrt", "network", "firewall"],
        )
        .unwrap();
        let lookup3 = profiles::Lookup::parse(ctx.clone(), &cfgs3).unwrap();

        let mut passwords3 = BTreeSet::new();
        passwords3.insert(Password {
            label: "Main Admin".into(),
            profile: None,
            password: "adminpass1".into(),
        });

        let wifi3 = Wifi {
            ssid: "TestNet".into(),
            broadcast_separately: false,
            radios: make_dual_radios(),
            passwords: passwords3,
        };
        set_config(&ctx, &mut cfgs3, &wifi3, &lookup3).unwrap();
        dump_all(ctx.uci_root(), cfgs3).unwrap();

        // GET — verify both radios have same SSID
        let arena4 = Arena::new();
        let cfgs4 = parse_all(
            ctx.uci_root(),
            &arena4,
            &["wireless", "startwrt", "network", "firewall"],
        )
        .unwrap();
        let wifi4 = get_config(ctx, &cfgs4).unwrap();

        assert!(!wifi4.broadcast_separately);
        assert_eq!(wifi4.ssid, "TestNet");
    }

    #[test]
    fn test_set_config_returns_full_when_ssid_changes() {
        let dir = tempfile::tempdir().unwrap();
        let ctx = TestContext(dir.path().to_path_buf());
        setup_test_configs(dir.path());
        write_wireless_config_dual_radio(dir.path(), "TestNet", "TestNet", "");

        let arena = Arena::new();
        let mut cfgs = parse_all(
            ctx.uci_root(),
            &arena,
            &["wireless", "startwrt", "network", "firewall"],
        )
        .unwrap();
        let lookup = profiles::Lookup::parse(ctx.clone(), &cfgs).unwrap();

        let mut passwords = BTreeSet::new();
        passwords.insert(Password {
            label: "Admin".into(),
            profile: None,
            password: "adminpass1".into(),
        });

        // Toggle broadcast_separately — changes 5GHz SSID from "TestNet" to "TestNet-5G"
        let wifi = Wifi {
            ssid: "TestNet".into(),
            broadcast_separately: true,
            radios: make_dual_radios(),
            passwords,
        };
        let restart = set_config(&ctx, &mut cfgs, &wifi, &lookup).unwrap();
        assert_eq!(restart, WifiRestart::Full, "SSID change should require full restart");
    }

    #[test]
    fn test_set_config_returns_psk_only_when_passwords_change() {
        let dir = tempfile::tempdir().unwrap();
        let ctx = TestContext(dir.path().to_path_buf());
        setup_test_configs(dir.path());
        write_wireless_config(dir.path(), "\
config wifi-station
\toption key 'guestpass1'
\toption vid '101'
\toption iface 'default_radio0'
\toption label 'Guest'
");

        // First set to establish baseline
        let arena = Arena::new();
        let mut cfgs = parse_all(
            ctx.uci_root(),
            &arena,
            &["wireless", "startwrt", "network", "firewall"],
        )
        .unwrap();
        let lookup = profiles::Lookup::parse(ctx.clone(), &cfgs).unwrap();

        let guest_id = lookup.from_fullname("Guest").unwrap().clone();
        let mut passwords = BTreeSet::new();
        passwords.insert(Password {
            label: "Admin".into(),
            profile: None,
            password: "adminpass1".into(),
        });
        passwords.insert(Password {
            label: "Guest".into(),
            profile: Some(guest_id.clone()),
            password: "guestpass1".into(),
        });

        let wifi = Wifi {
            ssid: "TestNet".into(),
            broadcast_separately: false,
            radios: make_radios(),
            passwords,
        };
        set_config(&ctx, &mut cfgs, &wifi, &lookup).unwrap();
        dump_all(ctx.uci_root(), cfgs).unwrap();

        // Now change only passwords — should be PskOnly
        let arena2 = Arena::new();
        let mut cfgs2 = parse_all(
            ctx.uci_root(),
            &arena2,
            &["wireless", "startwrt", "network", "firewall"],
        )
        .unwrap();
        let lookup2 = profiles::Lookup::parse(ctx.clone(), &cfgs2).unwrap();
        let guest_id2 = lookup2.from_fullname("Guest").unwrap().clone();

        let mut passwords2 = BTreeSet::new();
        passwords2.insert(Password {
            label: "Admin".into(),
            profile: None,
            password: "adminpass1".into(),
        });
        passwords2.insert(Password {
            label: "Guest".into(),
            profile: Some(guest_id2),
            password: "newguestpass".into(),
        });

        let wifi2 = Wifi {
            ssid: "TestNet".into(),
            broadcast_separately: false,
            radios: make_radios(),
            passwords: passwords2,
        };
        let restart = set_config(&ctx, &mut cfgs2, &wifi2, &lookup2).unwrap();
        assert_eq!(restart, WifiRestart::PskOnly, "password-only change should use PSK reload");
    }
}
