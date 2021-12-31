use std::collections::{BTreeMap, HashMap};
use std::path::{Path, PathBuf};
use std::sync::Arc;
use std::time::Duration;

use clap::ArgMatches;
use isocountry::CountryCode;
use lazy_static::lazy_static;
use regex::Regex;
use rpc_toolkit::command;
use tokio::process::Command;
use tokio::sync::RwLock;
use tracing::instrument;

use crate::context::RpcContext;
use crate::util::serde::{display_serializable, IoFormat};
use crate::util::{display_none, Invoke};
use crate::{Error, ErrorKind};

#[command(subcommands(add, connect, delete, get, set_country, get_available))]
pub async fn wifi() -> Result<(), Error> {
    Ok(())
}
#[command(display(display_none))]
#[instrument(skip(ctx))]
pub async fn add(
    #[context] ctx: RpcContext,
    #[arg] ssid: String,
    #[arg] password: String,
    #[arg] priority: isize,
    #[arg] connect: bool,
) -> Result<(), Error> {
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
        wifi_manager: Arc<RwLock<WpaCli>>,
        ssid: &Ssid,
        password: &Psk,
        priority: isize,
        connect: bool,
    ) -> Result<(), Error> {
        tracing::info!("Adding new WiFi network: '{}'", ssid.0);
        let mut wpa_supplicant = wifi_manager.write().await;
        wpa_supplicant.add_network(ssid, password, priority).await?;
        drop(wpa_supplicant);
        if connect {
            let wpa_supplicant = wifi_manager.read().await;
            let current = wpa_supplicant.get_current_network().await?;
            drop(wpa_supplicant);
            let mut wpa_supplicant = wifi_manager.write().await;
            let connected = wpa_supplicant.select_network(ssid).await?;
            if !connected {
                tracing::error!("Faild to add new WiFi network: '{}'", ssid.0);
                wpa_supplicant.remove_network(ssid).await?;
                match current {
                    None => {}
                    Some(current) => {
                        wpa_supplicant.select_network(&current).await?;
                    }
                }
            }
        }
        Ok(())
    }
    tokio::spawn(async move {
        match add_procedure(
            ctx.wifi_manager.clone(),
            &Ssid(ssid.clone()),
            &Psk(password.clone()),
            priority,
            connect,
        )
        .await
        {
            Err(e) => {
                tracing::error!("Failed to add new WiFi network '{}': {}", ssid, e);
                tracing::debug!("{:?}", e);
            }
            Ok(_) => {}
        }
    });
    Ok(())
}

#[command(display(display_none))]
#[instrument(skip(ctx))]
pub async fn connect(#[context] ctx: RpcContext, #[arg] ssid: String) -> Result<(), Error> {
    if !ssid.is_ascii() {
        return Err(Error::new(
            color_eyre::eyre::eyre!("SSID may not have special characters"),
            ErrorKind::Wifi,
        ));
    }
    async fn connect_procedure(
        wifi_manager: Arc<RwLock<WpaCli>>,
        ssid: &Ssid,
    ) -> Result<(), Error> {
        let wpa_supplicant = wifi_manager.read().await;
        let current = wpa_supplicant.get_current_network().await?;
        drop(wpa_supplicant);
        let mut wpa_supplicant = wifi_manager.write().await;
        let connected = wpa_supplicant.select_network(&ssid).await?;
        if connected {
            tracing::info!("Successfully connected to WiFi: '{}'", ssid.0);
        } else {
            tracing::error!("Failed to connect to WiFi: '{}'", ssid.0);
            match current {
                None => {
                    tracing::warn!("No WiFi to revert to!");
                }
                Some(current) => {
                    wpa_supplicant.select_network(&current).await?;
                }
            }
        }
        Ok(())
    }
    tokio::spawn(async move {
        match connect_procedure(ctx.wifi_manager.clone(), &Ssid(ssid.clone())).await {
            Err(e) => {
                tracing::error!("Failed to connect to WiFi network '{}': {}", &ssid, e);
            }
            Ok(_) => {}
        }
    });
    Ok(())
}

#[command(display(display_none))]
#[instrument(skip(ctx))]
pub async fn delete(#[context] ctx: RpcContext, #[arg] ssid: String) -> Result<(), Error> {
    if !ssid.is_ascii() {
        return Err(Error::new(
            color_eyre::eyre::eyre!("SSID may not have special characters"),
            ErrorKind::Wifi,
        ));
    }
    let wpa_supplicant = ctx.wifi_manager.read().await;
    let current = wpa_supplicant.get_current_network().await?;
    drop(wpa_supplicant);
    let mut wpa_supplicant = ctx.wifi_manager.write().await;
    let ssid = Ssid(ssid);
    match current {
        None => {
            wpa_supplicant.remove_network(&ssid).await?;
        }
        Some(current) => {
            if current == ssid {
                if interface_connected("eth0").await? {
                    wpa_supplicant.remove_network(&ssid).await?;
                } else {
                    return Err(Error::new(color_eyre::eyre::eyre!("Forbidden: Deleting this Network would make your Embassy Unreachable. Either connect to ethernet or connect to a different WiFi network to remedy this."), ErrorKind::Wifi));
                }
            }
        }
    }
    Ok(())
}
#[derive(serde::Serialize, serde::Deserialize)]
#[serde(rename_all = "kebab-case")]
pub struct WiFiInfo {
    ssids: Vec<String>,
    connected: Option<String>,
    country: CountryCode,
    ethernet: bool,
    signal_strength: Option<usize>,
}
#[derive(serde::Serialize, serde::Deserialize)]
#[serde(rename_all = "kebab-case")]
pub struct WifiListInfo {
    signal: u8,
    security: Vec<String>,
}
pub type WifiList = HashMap<Ssid, WifiListInfo>;
fn display_wifi_info(info: WiFiInfo, matches: &ArgMatches<'_>) {
    use prettytable::*;

    if matches.is_present("format") {
        return display_serializable(info, matches);
    }

    let mut table_global = Table::new();
    table_global.add_row(row![bc =>
        "CONNECTED",
        "SIGNAL_STRENGTH",
        "COUNTRY",
        "ETHERNET",
    ]);
    table_global.add_row(row![
        &info
            .connected
            .as_ref()
            .map_or("[N/A]".to_owned(), |c| format!("{}", c)),
        &info
            .signal_strength
            .as_ref()
            .map_or("[N/A]".to_owned(), |ss| format!("{}", ss)),
        &format!("{}", info.country.alpha2()),
        &format!("{}", info.ethernet)
    ]);
    table_global.print_tty(false);

    let mut table_ssids = Table::new();
    table_ssids.add_row(row![bc => "SSID",]);
    for ssid in &info.ssids {
        let mut row = row![ssid];
        if Some(ssid) == info.connected.as_ref() {
            row.iter_mut()
                .map(|c| {
                    c.style(Attr::ForegroundColor(match &info.signal_strength {
                        Some(100) => color::GREEN,
                        Some(0) => color::RED,
                        _ => color::YELLOW,
                    }))
                })
                .collect::<()>()
        }
        table_ssids.add_row(row);
    }
    table_ssids.print_tty(false);
}

fn display_wifi_list(info: WifiList, matches: &ArgMatches<'_>) {
    use prettytable::*;

    if matches.is_present("format") {
        return display_serializable(info, matches);
    }

    let mut table_global = Table::new();
    table_global.add_row(row![bc =>
        "SSID",
        "SIGNAL",
        "SECURITY",
    ]);
    for (ssid, table_info) in info {
        table_global.add_row(row![
            &ssid.0,
            &format!("{}", table_info.signal),
            &format!("{}", table_info.security.join(" "))
        ]);
    }

    table_global.print_tty(false);
}

#[command(display(display_wifi_info))]
#[instrument(skip(ctx))]
pub async fn get(
    #[context] ctx: RpcContext,
    #[allow(unused_variables)]
    #[arg(long = "format")]
    format: Option<IoFormat>,
) -> Result<WiFiInfo, Error> {
    let wpa_supplicant = ctx.wifi_manager.read().await;
    let ssids_task = async {
        Result::<Vec<String>, Error>::Ok(
            wpa_supplicant
                .list_networks_low()
                .await?
                .into_keys()
                .map(|x| x.0)
                .collect::<Vec<String>>(),
        )
    };
    let current_task = wpa_supplicant.get_current_network();
    let country_task = wpa_supplicant.get_country_low();
    let ethernet_task = interface_connected("eth0"); // TODO: pull from config
    let rssi_task = wpa_supplicant.signal_poll_low();
    let (ssids_res, current_res, country_res, ethernet_res, rssi_res) = tokio::join!(
        ssids_task,
        current_task,
        country_task,
        ethernet_task,
        rssi_task
    );
    let current = current_res?;
    let signal_strength = match rssi_res? {
        None => None,
        Some(x) if x <= -100 => Some(0 as usize),
        Some(x) if x >= -50 => Some(100 as usize),
        Some(x) => Some(2 * (x + 100) as usize),
    };
    Ok(WiFiInfo {
        ssids: ssids_res?,
        connected: current.map(|x| x.0),
        country: country_res?,
        ethernet: ethernet_res?,
        signal_strength,
    })
}

#[command(display(display_wifi_list))]
#[instrument(skip(ctx))]
pub async fn get_available(
    #[context] ctx: RpcContext,
    #[allow(unused_variables)]
    #[arg(long = "format")]
    format: Option<IoFormat>,
) -> Result<WifiList, Error> {
    let wpa_supplicant = ctx.wifi_manager.read().await;
    let (wifi_list, network_list) = tokio::join!(
        wpa_supplicant.list_wifi_low(),
        wpa_supplicant.list_networks_low()
    );
    let network_list = network_list?;

    Ok(wifi_list?
        .into_iter()
        .filter(|(ssid, _)| !network_list.contains_key(ssid))
        .collect())
}

#[command(display(display_none))]
#[instrument(skip(ctx))]
pub async fn set_country(
    #[context] ctx: RpcContext,
    #[arg(parse(country_code_parse))] country: CountryCode,
) -> Result<(), Error> {
    let mut wpa_supplicant = ctx.wifi_manager.write().await;
    wpa_supplicant.set_country_low(country.alpha2()).await
}

#[derive(Debug)]
pub struct WpaCli {
    datadir: PathBuf,
    interface: String,
}
#[derive(Clone, Debug, PartialEq, Eq, PartialOrd, Ord)]
pub struct NetworkId(String);

#[derive(
    Clone, Debug, PartialEq, Eq, PartialOrd, Ord, Hash, serde::Serialize, serde::Deserialize,
)]
pub struct Ssid(String);
#[derive(Clone, Debug)]
pub struct Psk(String);
impl WpaCli {
    pub fn init(interface: String, datadir: PathBuf) -> Self {
        WpaCli { interface, datadir }
    }

    pub async fn set_network_low(&mut self, ssid: &Ssid, psk: &Psk) -> Result<(), Error> {
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
    pub async fn set_country_low(&mut self, country_code: &str) -> Result<(), Error> {
        let _ = Command::new("iw")
            .arg("reg")
            .arg("set")
            .arg(country_code)
            .invoke(ErrorKind::Wifi)
            .await?;
        Ok(())
    }
    pub async fn get_country_low(&self) -> Result<CountryCode, Error> {
        let r = Command::new("iw")
            .arg("reg")
            .arg("get")
            .invoke(ErrorKind::Wifi)
            .await?;
        let r = String::from_utf8(r)?;
        lazy_static! {
            static ref RE: Regex = Regex::new("country (\\w+):").unwrap();
        }
        let first_country = r
            .lines()
            .filter(|s| s.contains("country"))
            .next()
            .ok_or_else(|| {
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
        Ok(CountryCode::for_alpha2(country).or(Err(Error::new(
            color_eyre::eyre::eyre!("Invalid Country Code: {}", country),
            ErrorKind::Wifi,
        )))?)
    }
    // pub async fn enable_network_low(&mut self, id: &NetworkId) -> Result<(), Error> {
    //     let _ = Command::new("wpa_cli")
    //         .arg("-i")
    //         .arg(&self.interface)
    //         .arg("enable_network")
    //         .arg(&id.0)
    //         .invoke(ErrorKind::Wifi)
    //         .await?;
    //     Ok(())
    // }
    // pub async fn save_config_low(&mut self) -> Result<(), Error> {
    //     let _ = Command::new("wpa_cli")
    //         .arg("-i")
    //         .arg(&self.interface)
    //         .arg("save_config")
    //         .invoke(ErrorKind::Wifi)
    //         .await?;
    //     Ok(())
    // }
    pub async fn remove_network_low(&mut self, id: NetworkId) -> Result<(), Error> {
        let _ = Command::new("nmcli")
            .arg("c")
            .arg("del")
            .arg(&id.0)
            .invoke(ErrorKind::Wifi)
            .await?;
        Ok(())
    }
    // pub async fn reconfigure_low(&mut self) -> Result<(), Error> {
    //     let _ = Command::new("wpa_cli")
    //         .arg("-i")
    //         .arg(&self.interface)
    //         .arg("reconfigure")
    //         .invoke(ErrorKind::Wifi)
    //         .await?;
    //     Ok(())
    // }
    #[instrument]
    pub async fn list_networks_low(&self) -> Result<BTreeMap<Ssid, NetworkId>, Error> {
        let r = Command::new("nmcli")
            .arg("-t")
            .arg("c")
            .arg("show")
            .invoke(ErrorKind::Wifi)
            .await?;
        Ok(String::from_utf8(r)?
            .lines()
            .filter_map(|l| {
                let mut cs = dbg!(l).split(":");
                let name = Ssid(cs.next()?.to_owned());
                let uuid = NetworkId(cs.next()?.to_owned());
                let _connection_type = cs.next()?;
                let device = cs.next()?;
                if device != &self.interface {
                    println!("Bad B");
                    return None;
                }
                Some((name, uuid))
            })
            .collect::<BTreeMap<Ssid, NetworkId>>())
    }

    #[instrument]
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
                let mut values = dbg!(l).split(":");
                let ssid = Ssid(values.next()?.to_owned());
                let signal: u8 = std::str::FromStr::from_str(values.next()?).ok()?;
                let security: Vec<String> =
                    values.next()?.split(" ").map(|x| x.to_owned()).collect();
                Some((ssid, WifiListInfo { signal, security }))
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
    // pub async fn new_password_low(&mut self, id: &Ssid, pass: &Psk) -> Result<(), Error> {
    //     let _ = Command::new("wpa_cli")
    //         .arg("-i")
    //         .arg(&self.interface)
    //         .arg("new_password")
    //         .arg(&id.0)
    //         .arg(&pass.0)
    //         .invoke(ErrorKind::Wifi)
    //         .await?;
    //     Ok(())
    // }
    #[instrument]
    pub async fn signal_poll_low(&self) -> Result<Option<isize>, Error> {
        let r = Command::new("wpa_cli")
            .arg("-i")
            .arg(&self.interface)
            .arg("signal_poll")
            .invoke(ErrorKind::Wifi)
            .await?;
        let e = || {
            Error::new(
                color_eyre::eyre::eyre!("Invalid output from wpa_cli signal_poll"),
                ErrorKind::Wifi,
            )
        };
        let output = String::from_utf8(r)?;
        Ok(if output.trim() == "FAIL" {
            None
        } else {
            let l = output.lines().next().ok_or_else(e)?;
            let rssi = l.split("=").nth(1).ok_or_else(e)?.parse()?;
            Some(rssi)
        })
    }

    // // High Level
    // pub async fn save_config(&mut self) -> Result<(), Error> {
    //     self.save_config_low().await?;
    //     tokio::fs::copy(
    //         "/etc/wpa_supplicant.conf",
    //         self.datadir.join("wpa_supplicant.conf"),
    //     )
    //     .await?;
    //     Ok(())
    // }
    pub async fn check_network(&self, ssid: &Ssid) -> Result<Option<NetworkId>, Error> {
        dbg!(Ok(self.list_networks_low().await?.remove(ssid)))
    }
    #[instrument]
    pub async fn select_network(&mut self, ssid: &Ssid) -> Result<bool, Error> {
        let m_id = self.check_network(ssid).await?;
        match m_id {
            None => Err(Error::new(
                color_eyre::eyre::eyre!("SSID Not Found"),
                ErrorKind::Wifi,
            )),
            Some(x) => {
                self.select_network_low(&x).await?;
                // self.save_config().await?;
                let connect = async {
                    let mut current;
                    loop {
                        current = self.get_current_network().await;
                        match &current {
                            Ok(Some(ssid)) => {
                                tracing::debug!("Connected to: {}", ssid.0);
                                break;
                            }
                            _ => {}
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
    #[instrument]
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
    #[instrument]
    pub async fn remove_network(&mut self, ssid: &Ssid) -> Result<bool, Error> {
        match self.check_network(ssid).await? {
            None => Ok(false),
            Some(x) => {
                self.remove_network_low(x).await?;
                // self.save_config().await?;
                // self.reconfigure_low().await?;
                Ok(true)
            }
        }
    }
    #[instrument]
    pub async fn add_network(
        &mut self,
        ssid: &Ssid,
        psk: &Psk,
        priority: isize,
    ) -> Result<(), Error> {
        self.set_network_low(&ssid, &psk).await?;
        // self.save_config().await?;
        Ok(())
    }
}

#[instrument]
pub async fn interface_connected(interface: &str) -> Result<bool, Error> {
    let out = Command::new("ifconfig")
        .arg(interface)
        .invoke(ErrorKind::Wifi)
        .await?;
    let v = std::str::from_utf8(&out)?
        .lines()
        .filter(|s| s.contains("inet"))
        .next();
    Ok(!v.is_none())
}

pub fn country_code_parse(code: &str, _matches: &ArgMatches<'_>) -> Result<CountryCode, Error> {
    CountryCode::for_alpha2(code).or(Err(Error::new(
        color_eyre::eyre::eyre!("Invalid Country Code: {}", code),
        ErrorKind::Wifi,
    )))
}

#[instrument(skip(main_datadir))]
pub async fn synchronize_wpa_supplicant_conf<P: AsRef<Path>>(main_datadir: P) -> Result<(), Error> {
    let persistent = main_datadir.as_ref().join("wpa_supplicant.conf");
    tracing::debug!("persistent: {:?}", persistent);
    if tokio::fs::metadata(&persistent).await.is_err() {
        tokio::fs::write(&persistent, include_str!("wpa_supplicant.conf.base")).await?;
    }
    let volatile = Path::new("/etc/wpa_supplicant.conf");
    tracing::debug!("link: {:?}", volatile);
    if tokio::fs::metadata(&volatile).await.is_ok() {
        tokio::fs::remove_file(&volatile).await?
    }
    tokio::fs::copy(&persistent, volatile).await?;
    Command::new("systemctl")
        .arg("restart")
        .arg("wpa_supplicant")
        .invoke(ErrorKind::Wifi)
        .await?;
    Command::new("ifconfig")
        .arg("wlan0")
        .arg("up")
        .invoke(ErrorKind::Wifi)
        .await?;
    Command::new("dhclient").invoke(ErrorKind::Wifi).await?;
    Ok(())
}
