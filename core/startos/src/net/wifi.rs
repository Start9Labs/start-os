use std::collections::{BTreeMap, BTreeSet, HashMap};
use std::path::Path;
use std::sync::Arc;
use std::time::Duration;

use clap::builder::TypedValueParser;
use clap::Parser;
use isocountry::CountryCode;
use lazy_static::lazy_static;
use regex::Regex;
use rpc_toolkit::{from_fn_async, Context, Empty, HandlerExt, ParentHandler};
use serde::{Deserialize, Serialize};
use tokio::process::Command;
use tokio::sync::RwLock;
use tracing::instrument;
use ts_rs::TS;

use crate::context::{CliContext, RpcContext};
use crate::db::model::public::WifiInfo;
use crate::db::model::Database;
use crate::prelude::*;
use crate::util::serde::{display_serializable, HandlerExtSerde, WithIoFormat};
use crate::util::Invoke;
use crate::{Error, ErrorKind};

type WifiManager = Arc<RwLock<Option<WpaCli>>>;

// pub fn wifi_manager(ctx: &RpcContext) -> Result<&WifiManager, Error> {
//     if let Some(wifi_manager) = ctx.wifi_manager.as_ref() {
//         Ok(wifi_manager)
//     } else {
//         Err(Error::new(
//             color_eyre::eyre::eyre!("No WiFi interface available"),
//             ErrorKind::Wifi,
//         ))
//     }
// }

pub fn wifi<C: Context>() -> ParentHandler<C> {
    ParentHandler::new()
        .subcommand(
            "set-enabled",
            from_fn_async(set_enabled)
                .no_display()
                .with_about("Enable or disable wifi")
                .with_call_remote::<CliContext>(),
        )
        .subcommand(
            "add",
            from_fn_async(add)
                .no_display()
                .with_about("Add wifi ssid and password")
                .with_call_remote::<CliContext>(),
        )
        .subcommand(
            "connect",
            from_fn_async(connect)
                .no_display()
                .with_about("Connect to wifi network")
                .with_call_remote::<CliContext>(),
        )
        .subcommand(
            "remove",
            from_fn_async(remove)
                .no_display()
                .with_about("Remove a wifi network")
                .with_call_remote::<CliContext>(),
        )
        .subcommand(
            "get",
            from_fn_async(get)
                .with_display_serializable()
                .with_custom_display_fn(|handle, result| {
                    Ok(display_wifi_info(handle.params, result))
                })
                .with_about("List wifi info")
                .with_call_remote::<CliContext>(),
        )
        .subcommand(
            "country",
            country::<C>().with_about("Command to set country"),
        )
        .subcommand(
            "available",
            available::<C>().with_about("Command to list available wifi networks"),
        )
}

#[derive(Deserialize, Serialize, Parser, TS)]
#[serde(rename_all = "camelCase")]
#[command(rename_all = "kebab-case")]
pub struct SetWifiEnabledParams {
    pub enabled: bool,
}

pub async fn set_enabled(
    ctx: RpcContext,
    SetWifiEnabledParams { enabled }: SetWifiEnabledParams,
) -> Result<(), Error> {
    if enabled {
        Command::new("rfkill")
            .arg("unblock")
            .arg("all")
            .invoke(ErrorKind::Wifi)
            .await?;
    } else {
        Command::new("rfkill")
            .arg("block")
            .arg("all")
            .invoke(ErrorKind::Wifi)
            .await?;
    }
    let iface = if let Some(man) = ctx.wifi_manager.read().await.as_ref().filter(|_| enabled) {
        Some(man.interface.clone())
    } else {
        None
    };
    ctx.db
        .mutate(|d| {
            d.as_public_mut()
                .as_server_info_mut()
                .as_network_mut()
                .as_wifi_mut()
                .as_interface_mut()
                .ser(&iface)
        })
        .await
        .result?;
    Ok(())
}

pub fn available<C: Context>() -> ParentHandler<C> {
    ParentHandler::new().subcommand(
        "get",
        from_fn_async(get_available)
            .with_display_serializable()
            .with_custom_display_fn(|handle, result| Ok(display_wifi_list(handle.params, result)))
            .with_about("List available wifi networks")
            .with_call_remote::<CliContext>(),
    )
}

pub fn country<C: Context>() -> ParentHandler<C> {
    ParentHandler::new().subcommand(
        "set",
        from_fn_async(set_country)
            .no_display()
            .with_about("Set Country")
            .with_call_remote::<CliContext>(),
    )
}

#[derive(Deserialize, Serialize, Parser, TS)]
#[serde(rename_all = "camelCase")]
#[command(rename_all = "kebab-case")]
pub struct AddParams {
    ssid: String,
    password: String,
}
#[instrument(skip_all)]
pub async fn add(ctx: RpcContext, AddParams { ssid, password }: AddParams) -> Result<(), Error> {
    let wifi_manager = ctx.wifi_manager.clone();
    if !ssid.is_ascii() {
        return Err(Error::new(
            color_eyre::eyre::eyre!("SSID may not have special characters"),
            ErrorKind::Wifi,
        ));
    }
    if !password.is_ascii() {
        return Err(Error::new(
            color_eyre::eyre::eyre!("WiFi Password may not have special characters"),
            ErrorKind::Wifi,
        ));
    }
    async fn add_procedure(
        db: TypedPatchDb<Database>,
        wifi_manager: WifiManager,
        ssid: &Ssid,
        password: &Psk,
    ) -> Result<(), Error> {
        tracing::info!("Adding new WiFi network: '{}'", ssid.0);
        let mut wpa_supplicant = wifi_manager.write_owned().await;
        let wpa_supplicant = wpa_supplicant.as_mut().ok_or_else(|| {
            Error::new(
                color_eyre::eyre::eyre!("No WiFi interface available"),
                ErrorKind::Wifi,
            )
        })?;
        wpa_supplicant.add_network(db, ssid, password).await?;
        Ok(())
    }
    if let Err(err) = add_procedure(
        ctx.db.clone(),
        wifi_manager.clone(),
        &Ssid(ssid.clone()),
        &Psk(password.clone()),
    )
    .await
    {
        tracing::error!("Failed to add new WiFi network '{}': {}", ssid, err);
        tracing::debug!("{:?}", err);
        return Err(Error::new(
            color_eyre::eyre::eyre!("Failed adding {}", ssid),
            ErrorKind::Wifi,
        ));
    }
    ctx.db
        .mutate(|db| {
            db.as_public_mut()
                .as_server_info_mut()
                .as_network_mut()
                .as_wifi_mut()
                .as_ssids_mut()
                .mutate(|s| {
                    s.insert(ssid);
                    Ok(())
                })
        })
        .await
        .result?;
    Ok(())
}
#[derive(Deserialize, Serialize, Parser, TS)]
#[serde(rename_all = "camelCase")]
#[command(rename_all = "kebab-case")]
pub struct SsidParams {
    ssid: String,
}

#[instrument(skip_all)]
pub async fn connect(ctx: RpcContext, SsidParams { ssid }: SsidParams) -> Result<(), Error> {
    let wifi_manager = ctx.wifi_manager.clone();
    if !ssid.is_ascii() {
        return Err(Error::new(
            color_eyre::eyre::eyre!("SSID may not have special characters"),
            ErrorKind::Wifi,
        ));
    }
    async fn connect_procedure(
        db: TypedPatchDb<Database>,
        wifi_manager: WifiManager,
        ssid: &Ssid,
    ) -> Result<(), Error> {
        let mut wpa_supplicant = wifi_manager.write_owned().await;
        let wpa_supplicant = wpa_supplicant.as_mut().ok_or_else(|| {
            Error::new(
                color_eyre::eyre::eyre!("No WiFi interface available"),
                ErrorKind::Wifi,
            )
        })?;
        let current = wpa_supplicant.get_current_network().await?;
        let connected = wpa_supplicant.select_network(db.clone(), ssid).await?;
        if connected {
            tracing::info!("Successfully connected to WiFi: '{}'", ssid.0);
        } else {
            tracing::info!("Failed to connect to WiFi: '{}'", ssid.0);
            match current {
                None => {
                    tracing::info!("No WiFi to revert to!");
                }
                Some(current) => {
                    wpa_supplicant.select_network(db, &current).await?;
                }
            }
        }
        Ok(())
    }

    if let Err(err) =
        connect_procedure(ctx.db.clone(), wifi_manager.clone(), &Ssid(ssid.clone())).await
    {
        tracing::error!("Failed to connect to WiFi network '{}': {}", &ssid, err);
        return Err(Error::new(
            color_eyre::eyre::eyre!("Can't connect to {}", ssid),
            ErrorKind::Wifi,
        ));
    }

    ctx.db
        .mutate(|db| {
            let wifi = db
                .as_public_mut()
                .as_server_info_mut()
                .as_network_mut()
                .as_wifi_mut();
            wifi.as_ssids_mut().mutate(|s| {
                s.insert(ssid.clone());
                Ok(())
            })?;
            wifi.as_selected_mut().ser(&Some(ssid))
        })
        .await
        .result?;
    Ok(())
}

#[instrument(skip_all)]
pub async fn remove(ctx: RpcContext, SsidParams { ssid }: SsidParams) -> Result<(), Error> {
    let wifi_manager = ctx.wifi_manager.clone();
    if !ssid.is_ascii() {
        return Err(Error::new(
            color_eyre::eyre::eyre!("SSID may not have special characters"),
            ErrorKind::Wifi,
        ));
    }

    let mut wpa_supplicant = wifi_manager.write_owned().await;
    let wpa_supplicant = wpa_supplicant.as_mut().ok_or_else(|| {
        Error::new(
            color_eyre::eyre::eyre!("No WiFi interface available"),
            ErrorKind::Wifi,
        )
    })?;
    let current = wpa_supplicant.get_current_network().await?;
    let ssid = Ssid(ssid);
    let is_current_being_removed = matches!(current, Some(current) if current == ssid);
    let is_current_removed_and_no_hardwire =
        is_current_being_removed && !interface_connected(&ctx.ethernet_interface).await?;
    if is_current_removed_and_no_hardwire {
        return Err(Error::new(color_eyre::eyre::eyre!("Forbidden: Deleting this network would make your server unreachable. Either connect to ethernet or connect to a different WiFi network to remedy this."), ErrorKind::Wifi));
    }

    wpa_supplicant.remove_network(ctx.db.clone(), &ssid).await?;

    ctx.db
        .mutate(|db| {
            let wifi = db
                .as_public_mut()
                .as_server_info_mut()
                .as_network_mut()
                .as_wifi_mut();
            wifi.as_ssids_mut().mutate(|s| {
                s.remove(&ssid.0);
                Ok(())
            })?;
            wifi.as_selected_mut()
                .map_mutate(|s| Ok(s.filter(|s| s == &ssid.0)))
        })
        .await
        .result?;
    Ok(())
}
#[derive(serde::Serialize, serde::Deserialize)]
#[serde(rename_all = "camelCase")]
pub struct WifiListInfo {
    ssids: HashMap<Ssid, SignalStrength>,
    connected: Option<Ssid>,
    country: Option<CountryCode>,
    ethernet: bool,
    available_wifi: Vec<WifiListOut>,
}
#[derive(serde::Serialize, serde::Deserialize, Clone)]
#[serde(rename_all = "camelCase")]
pub struct WifiListInfoLow {
    strength: SignalStrength,
    security: Vec<String>,
}
#[derive(serde::Serialize, serde::Deserialize)]
#[serde(rename_all = "camelCase")]
pub struct WifiListOut {
    ssid: Ssid,
    strength: SignalStrength,
    security: Vec<String>,
}
pub type WifiList = HashMap<Ssid, WifiListInfoLow>;
fn display_wifi_info(params: WithIoFormat<Empty>, info: WifiListInfo) {
    use prettytable::*;

    if let Some(format) = params.format {
        return display_serializable(format, info);
    }

    let mut table_global = Table::new();
    table_global.add_row(row![bc =>
        "CONNECTED",
        "SIGNAL STRENGTH",
        "COUNTRY",
        "ETHERNET",
    ]);
    table_global.add_row(row![
        &info
            .connected
            .as_ref()
            .map_or("N/A".to_owned(), |c| c.0.clone()),
        &info
            .connected
            .as_ref()
            .and_then(|x| info.ssids.get(x))
            .map_or("N/A".to_owned(), |ss| format!("{}", ss.0)),
        info.country.as_ref().map(|c| c.alpha2()).unwrap_or("00"),
        &format!("{}", info.ethernet)
    ]);
    table_global.print_tty(false).unwrap();

    let mut table_ssids = Table::new();
    table_ssids.add_row(row![bc => "SSID", "STRENGTH"]);
    for (ssid, signal_strength) in &info.ssids {
        let mut row = row![&ssid.0, format!("{}", signal_strength.0)];
        row.iter_mut()
            .map(|c| {
                c.style(Attr::ForegroundColor(match &signal_strength.0 {
                    x if x >= &90 => color::GREEN,
                    x if x == &50 => color::MAGENTA,
                    x if x == &0 => color::RED,
                    _ => color::YELLOW,
                }))
            })
            .for_each(drop);
        table_ssids.add_row(row);
    }
    table_ssids.print_tty(false).unwrap();

    let mut table_global = Table::new();
    table_global.add_row(row![bc =>
        "SSID",
        "STRENGTH",
        "SECURITY",
    ]);
    for table_info in info.available_wifi {
        table_global.add_row(row![
            &table_info.ssid.0,
            &format!("{}", table_info.strength.0),
            &table_info.security.join(" ")
        ]);
    }

    table_global.print_tty(false).unwrap();
}

fn display_wifi_list(params: WithIoFormat<Empty>, info: Vec<WifiListOut>) {
    use prettytable::*;

    if let Some(format) = params.format {
        return display_serializable(format, info);
    }

    let mut table_global = Table::new();
    table_global.add_row(row![bc =>
        "SSID",
        "STRENGTH",
        "SECURITY",
    ]);
    for table_info in info {
        table_global.add_row(row![
            &table_info.ssid.0,
            &format!("{}", table_info.strength.0),
            &table_info.security.join(" ")
        ]);
    }

    table_global.print_tty(false).unwrap();
}

// #[command(display(display_wifi_info))]
#[instrument(skip_all)]
pub async fn get(ctx: RpcContext, _: Empty) -> Result<WifiListInfo, Error> {
    let wifi_manager = ctx.wifi_manager.clone();
    let wpa_supplicant = wifi_manager.read_owned().await;
    let wpa_supplicant = wpa_supplicant.as_ref().ok_or_else(|| {
        Error::new(
            color_eyre::eyre::eyre!("No WiFi interface available"),
            ErrorKind::Wifi,
        )
    })?;
    let (list_networks, current_res, country_res, ethernet_res, signal_strengths) = tokio::join!(
        wpa_supplicant.list_networks_low(),
        wpa_supplicant.get_current_network(),
        wpa_supplicant.get_country_low(),
        interface_connected(&ctx.ethernet_interface),
        wpa_supplicant.list_wifi_low()
    );
    let signal_strengths = signal_strengths?;
    let list_networks: BTreeSet<_> = list_networks?.into_iter().map(|(_, x)| x.ssid).collect();
    let available_wifi = {
        let mut wifi_list: Vec<WifiListOut> = signal_strengths
            .clone()
            .into_iter()
            .filter(|(ssid, _)| !list_networks.contains(ssid))
            .map(|(ssid, info)| WifiListOut {
                ssid,
                strength: info.strength,
                security: info.security,
            })
            .collect();
        wifi_list.sort_by_key(|x| x.strength);
        wifi_list.reverse();
        wifi_list
    };
    let ssids: HashMap<Ssid, SignalStrength> = list_networks
        .into_iter()
        .map(|ssid| {
            let signal_strength = signal_strengths
                .get(&ssid)
                .map(|x| x.strength)
                .unwrap_or_default();
            (ssid, signal_strength)
        })
        .collect();
    let current = current_res?;
    Ok(WifiListInfo {
        ssids,
        connected: current,
        country: country_res?,
        ethernet: ethernet_res?,
        available_wifi,
    })
}

#[instrument(skip_all)]
pub async fn get_available(ctx: RpcContext, _: Empty) -> Result<Vec<WifiListOut>, Error> {
    let wifi_manager = ctx.wifi_manager.clone();
    let wpa_supplicant = wifi_manager.read_owned().await;
    let wpa_supplicant = wpa_supplicant.as_ref().ok_or_else(|| {
        Error::new(
            color_eyre::eyre::eyre!("No WiFi interface available"),
            ErrorKind::Wifi,
        )
    })?;
    let (wifi_list, network_list) = tokio::join!(
        wpa_supplicant.list_wifi_low(),
        wpa_supplicant.list_networks_low()
    );
    let network_list = network_list?
        .into_iter()
        .map(|(_, info)| info.ssid)
        .collect::<BTreeSet<_>>();
    let mut wifi_list: Vec<WifiListOut> = wifi_list?
        .into_iter()
        .filter(|(ssid, _)| !network_list.contains(ssid))
        .map(|(ssid, info)| WifiListOut {
            ssid,
            strength: info.strength,
            security: info.security,
        })
        .collect();
    wifi_list.sort_by_key(|x| x.strength);
    wifi_list.reverse();
    Ok(wifi_list)
}

#[derive(Deserialize, Serialize, Parser, TS)]
#[serde(rename_all = "camelCase")]
#[command(rename_all = "kebab-case")]
pub struct SetCountryParams {
    #[arg(value_parser = CountryCodeParser)]
    #[ts(type = "string")]
    country: CountryCode,
}
pub async fn set_country(
    ctx: RpcContext,
    SetCountryParams { country }: SetCountryParams,
) -> Result<(), Error> {
    let wifi_manager = ctx.wifi_manager.clone();
    if !interface_connected(&ctx.ethernet_interface).await? {
        return Err(Error::new(
            color_eyre::eyre::eyre!("Won't change country without hardwire connection"),
            crate::ErrorKind::Wifi,
        ));
    }
    let mut wpa_supplicant = wifi_manager.write_owned().await;
    let wpa_supplicant = wpa_supplicant.as_mut().ok_or_else(|| {
        Error::new(
            color_eyre::eyre::eyre!("No WiFi interface available"),
            ErrorKind::Wifi,
        )
    })?;
    wpa_supplicant.set_country_low(country.alpha2()).await?;
    for (network_id, _wifi_info) in wpa_supplicant.list_networks_low().await? {
        wpa_supplicant.remove_network_low(network_id).await?;
    }
    wpa_supplicant.remove_all_connections().await?;

    wpa_supplicant.save_config(ctx.db.clone()).await?;

    Ok(())
}

#[derive(Debug)]
pub struct WpaCli {
    interface: String,
}
#[derive(Clone, Debug, PartialEq, Eq, PartialOrd, Ord)]
pub struct NetworkId(String);

/// Ssid are the names of the wifis, usually human readable.
#[derive(
    Clone, Debug, PartialEq, Eq, PartialOrd, Ord, Hash, serde::Serialize, serde::Deserialize,
)]
pub struct Ssid(String);

/// So a signal strength is a number between 0-100, I want the null option to be 0 since there is no signal
#[derive(
    Clone,
    Copy,
    Debug,
    Default,
    PartialEq,
    Eq,
    PartialOrd,
    Ord,
    Hash,
    serde::Serialize,
    serde::Deserialize,
)]
pub struct SignalStrength(u8);

impl SignalStrength {
    fn new(size: Option<u8>) -> Self {
        let size = match size {
            None => return Self(0),
            Some(x) => x,
        };
        if size >= 100 {
            return Self(100);
        }
        Self(size)
    }
}

#[derive(Debug, Clone)]
pub struct WifiInfoLow {
    ssid: Ssid,
    device: Option<String>,
}

#[derive(Clone, Debug)]
pub struct Psk(String);
impl WpaCli {
    pub fn init(interface: String) -> Self {
        WpaCli { interface }
    }

    #[instrument(skip_all)]
    pub async fn set_add_network_low(&mut self, ssid: &Ssid, psk: &Psk) -> Result<(), Error> {
        let _ = Command::new("nmcli")
            .arg("-a")
            .arg("-w")
            .arg("30")
            .arg("d")
            .arg("wifi")
            .arg("con")
            .arg(&ssid.0)
            .arg("password")
            .arg(&psk.0)
            .invoke(ErrorKind::Wifi)
            .await?;
        Ok(())
    }
    #[instrument(skip_all)]
    pub async fn add_network_low(&mut self, ssid: &Ssid, psk: &Psk) -> Result<(), Error> {
        if self.find_networks(ssid).await?.is_empty() {
            Command::new("nmcli")
                .arg("con")
                .arg("add")
                .arg("con-name")
                .arg(&ssid.0)
                .arg("type")
                .arg("wifi")
                .arg("ssid")
                .arg(&ssid.0)
                .invoke(ErrorKind::Wifi)
                .await?;
        }
        Command::new("nmcli")
            .arg("con")
            .arg("modify")
            .arg(&ssid.0)
            .arg("wifi-sec.key-mgmt")
            .arg("wpa-psk")
            .invoke(ErrorKind::Wifi)
            .await?;
        Command::new("nmcli")
            .arg("con")
            .arg("modify")
            .arg(&ssid.0)
            .arg("ifname")
            .arg(&self.interface)
            .invoke(ErrorKind::Wifi)
            .await
            .map(|_| ())
            .unwrap_or_else(|e| {
                tracing::warn!("Failed to set interface {} for {}", self.interface, ssid.0);
                tracing::debug!("{:?}", e);
            });
        Command::new("nmcli")
            .arg("con")
            .arg("modify")
            .arg(&ssid.0)
            .arg("wifi-sec.psk")
            .arg(&psk.0)
            .invoke(ErrorKind::Wifi)
            .await?;
        Ok(())
    }
    pub async fn set_country_low(&mut self, country_code: &str) -> Result<(), Error> {
        let _ = Command::new("iw")
            .arg("reg")
            .arg("set")
            .arg(country_code)
            .invoke(ErrorKind::Wifi)
            .await?;

        Ok(())
    }
    pub async fn get_country_low(&self) -> Result<Option<CountryCode>, Error> {
        let r = Command::new("iw")
            .arg("reg")
            .arg("get")
            .invoke(ErrorKind::Wifi)
            .await?;
        let r = String::from_utf8(r)?;
        lazy_static! {
            static ref RE: Regex = Regex::new("country (\\w+):").unwrap();
        }
        let first_country = r.lines().find(|s| s.contains("country")).ok_or_else(|| {
            Error::new(
                color_eyre::eyre::eyre!("Could not find a country config lines"),
                ErrorKind::Wifi,
            )
        })?;
        let country = &RE.captures(first_country).ok_or_else(|| {
            Error::new(
                color_eyre::eyre::eyre!("Could not find a country config with regex"),
                ErrorKind::Wifi,
            )
        })?[1];
        if country == "00" {
            Ok(None)
        } else {
            Ok(Some(CountryCode::for_alpha2(country).map_err(|_| {
                Error::new(
                    color_eyre::eyre::eyre!("Invalid Country Code: {}", country),
                    ErrorKind::Wifi,
                )
            })?))
        }
    }
    pub async fn remove_network_low(&mut self, id: NetworkId) -> Result<(), Error> {
        let _ = Command::new("nmcli")
            .arg("c")
            .arg("del")
            .arg(&id.0)
            .invoke(ErrorKind::Wifi)
            .await?;
        Ok(())
    }
    #[instrument(skip_all)]
    pub async fn list_networks_low(&self) -> Result<BTreeMap<NetworkId, WifiInfoLow>, Error> {
        let r = Command::new("nmcli")
            .arg("-t")
            .arg("c")
            .arg("show")
            .invoke(ErrorKind::Wifi)
            .await?;
        let r = String::from_utf8(r)?;
        tracing::info!("JCWM: all the networks: {:?}", r);
        Ok(r.lines()
            .filter_map(|l| {
                let mut cs = l.split(':');
                let name = Ssid(cs.next()?.to_owned());
                let uuid = NetworkId(cs.next()?.to_owned());
                let connection_type = cs.next()?;
                let device = cs.next();
                if !connection_type.contains("wireless") {
                    return None;
                }
                let info = WifiInfoLow {
                    ssid: name,
                    device: device.map(|x| x.to_owned()),
                };
                Some((uuid, info))
            })
            .collect::<BTreeMap<NetworkId, WifiInfoLow>>())
    }

    #[instrument(skip_all)]
    pub async fn list_wifi_low(&self) -> Result<WifiList, Error> {
        let r = Command::new("nmcli")
            .arg("-g")
            .arg("SSID,SIGNAL,security")
            .arg("d")
            .arg("wifi")
            .arg("list")
            .invoke(ErrorKind::Wifi)
            .await?;
        Ok(String::from_utf8(r)?
            .lines()
            .filter_map(|l| {
                let mut values = l.split(':');
                let ssid = Ssid(values.next()?.to_owned());
                let signal = SignalStrength::new(std::str::FromStr::from_str(values.next()?).ok());
                let security: Vec<String> =
                    values.next()?.split(' ').map(|x| x.to_owned()).collect();
                Some((
                    ssid,
                    WifiListInfoLow {
                        strength: signal,
                        security,
                    },
                ))
            })
            .collect::<WifiList>())
    }
    pub async fn select_network_low(&mut self, id: &NetworkId) -> Result<(), Error> {
        let _ = Command::new("nmcli")
            .arg("c")
            .arg("up")
            .arg(&id.0)
            .invoke(ErrorKind::Wifi)
            .await?;
        Ok(())
    }
    pub async fn remove_all_connections(&mut self) -> Result<(), Error> {
        let location_connections = Path::new("/etc/NetworkManager/system-connections");
        let mut connections = tokio::fs::read_dir(&location_connections).await?;
        while let Some(connection) = connections.next_entry().await? {
            let path = connection.path();
            if path.is_file() {
                let _ = tokio::fs::remove_file(&path).await?;
            }
        }

        Ok(())
    }
    pub async fn save_config(&mut self, db: TypedPatchDb<Database>) -> Result<(), Error> {
        let new_country = self.get_country_low().await?;
        db.mutate(|d| {
            d.as_public_mut()
                .as_server_info_mut()
                .as_network_mut()
                .as_wifi_mut()
                .as_last_region_mut()
                .ser(&new_country)
        })
        .await
        .result
    }
    async fn check_active_network(&self, ssid: &Ssid) -> Result<Option<NetworkId>, Error> {
        Ok(self
            .list_networks_low()
            .await?
            .iter()
            .find_map(|(network_id, wifi_info)| {
                wifi_info.device.as_ref()?;
                if wifi_info.ssid == *ssid {
                    Some(network_id.clone())
                } else {
                    None
                }
            }))
    }
    pub async fn find_networks(&self, ssid: &Ssid) -> Result<Vec<NetworkId>, Error> {
        Ok(self
            .list_networks_low()
            .await?
            .iter()
            .filter_map(|(network_id, wifi_info)| {
                if wifi_info.ssid == *ssid {
                    Some(network_id.clone())
                } else {
                    None
                }
            })
            .collect())
    }
    #[instrument(skip_all)]
    pub async fn select_network(
        &mut self,
        db: TypedPatchDb<Database>,
        ssid: &Ssid,
    ) -> Result<bool, Error> {
        let m_id = self.check_active_network(ssid).await?;
        match m_id {
            None => Err(Error::new(
                color_eyre::eyre::eyre!("SSID Not Found"),
                ErrorKind::Wifi,
            )),
            Some(x) => {
                self.select_network_low(&x).await?;
                self.save_config(db).await?;
                let connect = async {
                    let mut current;
                    loop {
                        current = self.get_current_network().await;
                        if let Ok(Some(ssid)) = &current {
                            tracing::debug!("Connected to: {}", ssid.0);
                            break;
                        }
                        tokio::time::sleep(Duration::from_millis(500)).await;
                        tracing::debug!("Retrying...");
                    }
                    current
                };
                let res = match tokio::time::timeout(Duration::from_secs(20), connect).await {
                    Err(_) => None,
                    Ok(net) => net?,
                };
                tracing::debug!("{:?}", res);
                Ok(match res {
                    None => false,
                    Some(net) => &net == ssid,
                })
            }
        }
    }
    #[instrument(skip_all)]
    pub async fn get_current_network(&self) -> Result<Option<Ssid>, Error> {
        let r = Command::new("iwgetid")
            .arg(&self.interface)
            .arg("--raw")
            .invoke(ErrorKind::Wifi)
            .await?;
        let output = String::from_utf8(r)?;
        let network = output.trim();
        tracing::debug!("Current Network: \"{}\"", network);
        if network.is_empty() {
            Ok(None)
        } else {
            Ok(Some(Ssid(network.to_owned())))
        }
    }
    #[instrument(skip_all)]
    pub async fn remove_network(
        &mut self,
        db: TypedPatchDb<Database>,
        ssid: &Ssid,
    ) -> Result<bool, Error> {
        let found_networks = self.find_networks(ssid).await?;
        if found_networks.is_empty() {
            return Ok(true);
        }
        for network_id in found_networks {
            self.remove_network_low(network_id).await?;
        }
        self.save_config(db).await?;
        Ok(true)
    }
    #[instrument(skip_all)]
    pub async fn set_add_network(
        &mut self,
        db: TypedPatchDb<Database>,
        ssid: &Ssid,
        psk: &Psk,
    ) -> Result<(), Error> {
        self.set_add_network_low(ssid, psk).await?;
        self.save_config(db).await?;
        Ok(())
    }
    #[instrument(skip_all)]
    pub async fn add_network(
        &mut self,
        db: TypedPatchDb<Database>,
        ssid: &Ssid,
        psk: &Psk,
    ) -> Result<(), Error> {
        self.add_network_low(ssid, psk).await?;
        self.save_config(db).await?;
        Ok(())
    }
}

#[instrument(skip_all)]
pub async fn interface_connected(interface: &str) -> Result<bool, Error> {
    let out = Command::new("ifconfig")
        .arg(interface)
        .invoke(ErrorKind::Wifi)
        .await?;
    let v = std::str::from_utf8(&out)?
        .lines()
        .find(|s| s.contains("inet"));
    Ok(v.is_some())
}

#[derive(Clone)]
struct CountryCodeParser;
impl TypedValueParser for CountryCodeParser {
    type Value = CountryCode;
    fn parse_ref(
        &self,
        _: &clap::Command,
        _: Option<&clap::Arg>,
        value: &std::ffi::OsStr,
    ) -> Result<Self::Value, clap::Error> {
        let code = value.to_string_lossy();
        CountryCode::for_alpha2(&code).map_err(|_| {
            clap::Error::raw(
                clap::error::ErrorKind::ValueValidation,
                color_eyre::eyre::eyre!("Invalid Country Code: {}", code),
            )
        })
    }
}

#[instrument(skip_all)]
pub async fn synchronize_network_manager<P: AsRef<Path>>(
    main_datadir: P,
    wifi: &WifiInfo,
) -> Result<(), Error> {
    let persistent = main_datadir.as_ref().join("system-connections");

    if tokio::fs::metadata(&persistent).await.is_err() {
        tokio::fs::create_dir_all(&persistent).await?;
    }
    crate::disk::mount::util::bind(&persistent, "/etc/NetworkManager/system-connections", false)
        .await?;

    if !wifi.enabled {
        Command::new("rfkill")
            .arg("block")
            .arg("all")
            .invoke(ErrorKind::Wifi)
            .await?;
    }

    Command::new("systemctl")
        .arg("restart")
        .arg("NetworkManager")
        .invoke(ErrorKind::Wifi)
        .await?;

    let Some(wifi_iface) = wifi.interface.as_ref().filter(|_| wifi.enabled) else {
        return Ok(());
    };

    Command::new("ifconfig")
        .arg(wifi_iface)
        .arg("up")
        .invoke(ErrorKind::Wifi)
        .await?;
    if let Some(last_country_code) = wifi.last_region {
        tracing::info!("Setting the region");
        let _ = Command::new("iw")
            .arg("reg")
            .arg("set")
            .arg(last_country_code.alpha2())
            .invoke(ErrorKind::Wifi)
            .await?;
    } else {
        tracing::info!("Setting the region fallback");
        let _ = Command::new("iw")
            .arg("reg")
            .arg("set")
            .arg("US")
            .invoke(ErrorKind::Wifi)
            .await?;
    }
    Ok(())
}
