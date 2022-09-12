use std::collections::{BTreeMap, BTreeSet, HashMap};
use std::path::Path;
use std::sync::Arc;
use std::time::Duration;

use clap::ArgMatches;
use isocountry::CountryCode;
use lazy_static::lazy_static;
use patch_db::DbHandle;
use regex::Regex;
use rpc_toolkit::command;
use tokio::process::Command;
use tokio::sync::RwLock;
use tracing::instrument;

use crate::context::RpcContext;
use crate::util::serde::{display_serializable, IoFormat};
use crate::util::{display_none, Invoke};
use crate::{Error, ErrorKind};

#[command(subcommands(add, connect, delete, get, country, available))]
pub async fn wifi() -> Result<(), Error> {
    Ok(())
}

#[command(subcommands(get_available))]
pub async fn available() -> Result<(), Error> {
    Ok(())
}

#[command(subcommands(set_country))]
pub async fn country() -> Result<(), Error> {
    Ok(())
}

#[command(display(display_none))]
#[instrument(skip(ctx, password))]
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
        db: impl DbHandle,
        wifi_manager: Arc<RwLock<WpaCli>>,
        ssid: &Ssid,
        password: &Psk,
        priority: isize,
    ) -> Result<(), Error> {
        tracing::info!("Adding new WiFi network: '{}'", ssid.0);
        let mut wpa_supplicant = wifi_manager.write().await;
        wpa_supplicant
            .add_network(db, ssid, password, priority)
            .await?;
        drop(wpa_supplicant);
        Ok(())
    }
    if let Err(err) = add_procedure(
        &mut ctx.db.handle(),
        ctx.wifi_manager.clone(),
        &Ssid(ssid.clone()),
        &Psk(password.clone()),
        priority,
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
        mut db: impl DbHandle,
        wifi_manager: Arc<RwLock<WpaCli>>,
        ssid: &Ssid,
    ) -> Result<(), Error> {
        let wpa_supplicant = wifi_manager.read().await;
        let current = wpa_supplicant.get_current_network().await?;
        drop(wpa_supplicant);
        let mut wpa_supplicant = wifi_manager.write().await;
        let connected = wpa_supplicant.select_network(&mut db, ssid).await?;
        if connected {
            tracing::info!("Successfully connected to WiFi: '{}'", ssid.0);
        } else {
            tracing::info!("Failed to connect to WiFi: '{}'", ssid.0);
            match current {
                None => {
                    tracing::info!("No WiFi to revert to!");
                }
                Some(current) => {
                    wpa_supplicant.select_network(&mut db, &current).await?;
                }
            }
        }
        Ok(())
    }

    if let Err(err) = connect_procedure(
        &mut ctx.db.handle(),
        ctx.wifi_manager.clone(),
        &Ssid(ssid.clone()),
    )
    .await
    {
        tracing::error!("Failed to connect to WiFi network '{}': {}", &ssid, err);
        return Err(Error::new(
            color_eyre::eyre::eyre!("Can't connect to {}", ssid),
            ErrorKind::Wifi,
        ));
    }
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
    let is_current_being_removed = matches!(current, Some(current) if current == ssid);
    let is_current_removed_and_no_hardwire =
        is_current_being_removed && !interface_connected("eth0").await?;
    if is_current_removed_and_no_hardwire {
        return Err(Error::new(color_eyre::eyre::eyre!("Forbidden: Deleting this Network would make your Embassy Unreachable. Either connect to ethernet or connect to a different WiFi network to remedy this."), ErrorKind::Wifi));
    }

    wpa_supplicant
        .remove_network(&mut ctx.db.handle(), &ssid)
        .await?;
    Ok(())
}
#[derive(serde::Serialize, serde::Deserialize)]
#[serde(rename_all = "kebab-case")]
pub struct WiFiInfo {
    ssids: HashMap<Ssid, SignalStrength>,
    connected: Option<Ssid>,
    country: CountryCode,
    ethernet: bool,
    available_wifi: Vec<WifiListOut>,
}
#[derive(serde::Serialize, serde::Deserialize, Clone)]
#[serde(rename_all = "kebab-case")]
pub struct WifiListInfo {
    strength: SignalStrength,
    security: Vec<String>,
}
#[derive(serde::Serialize, serde::Deserialize)]
#[serde(rename_all = "kebab-case")]
pub struct WifiListOut {
    ssid: Ssid,
    strength: SignalStrength,
    security: Vec<String>,
}
pub type WifiList = HashMap<Ssid, WifiListInfo>;
fn display_wifi_info(info: WiFiInfo, matches: &ArgMatches) {
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
            .map_or("[N/A]".to_owned(), |c| c.0.clone()),
        &info
            .connected
            .as_ref()
            .and_then(|x| info.ssids.get(x))
            .map_or("[N/A]".to_owned(), |ss| format!("{}", ss.0)),
        &info.country.alpha2(),
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

fn display_wifi_list(info: Vec<WifiListOut>, matches: &ArgMatches) {
    use prettytable::*;

    if matches.is_present("format") {
        return display_serializable(info, matches);
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

#[command(display(display_wifi_info))]
#[instrument(skip(ctx))]
pub async fn get(
    #[context] ctx: RpcContext,
    #[allow(unused_variables)]
    #[arg(long = "format")]
    format: Option<IoFormat>,
) -> Result<WiFiInfo, Error> {
    let wpa_supplicant = ctx.wifi_manager.read().await;
    let (list_networks, current_res, country_res, ethernet_res, signal_strengths) = tokio::join!(
        wpa_supplicant.list_networks_low(),
        wpa_supplicant.get_current_network(),
        wpa_supplicant.get_country_low(),
        interface_connected("eth0"), // TODO: pull from config
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
    Ok(WiFiInfo {
        ssids,
        connected: current,
        country: country_res?,
        ethernet: ethernet_res?,
        available_wifi,
    })
}

#[command(rename = "get", display(display_wifi_list))]
#[instrument(skip(ctx))]
pub async fn get_available(
    #[context] ctx: RpcContext,
    #[allow(unused_variables)]
    #[arg(long = "format")]
    format: Option<IoFormat>,
) -> Result<Vec<WifiListOut>, Error> {
    let wpa_supplicant = ctx.wifi_manager.read().await;
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

#[command(rename = "set", display(display_none))]
pub async fn set_country(
    #[context] ctx: RpcContext,
    #[arg(parse(country_code_parse))] country: CountryCode,
) -> Result<(), Error> {
    if !interface_connected("eth0").await? {
        return Err(Error::new(
            color_eyre::eyre::eyre!("Won't change country without hardwire connection"),
            crate::ErrorKind::Wifi,
        ));
    }
    let mut wpa_supplicant = ctx.wifi_manager.write().await;
    wpa_supplicant.set_country_low(country.alpha2()).await?;
    for (network_id, _wifi_info) in wpa_supplicant.list_networks_low().await? {
        wpa_supplicant.remove_network_low(network_id).await?;
    }
    wpa_supplicant.remove_all_connections().await?;

    wpa_supplicant.save_config(&mut ctx.db.handle()).await?;

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
pub struct WifiInfo {
    ssid: Ssid,
    device: Option<String>,
}

#[derive(Clone, Debug)]
pub struct Psk(String);
impl WpaCli {
    pub fn init(interface: String) -> Self {
        WpaCli { interface }
    }

    #[instrument(skip(self, psk))]
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
    #[instrument(skip(self, psk))]
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
        Ok(CountryCode::for_alpha2(country).map_err(|_| {
            Error::new(
                color_eyre::eyre::eyre!("Invalid Country Code: {}", country),
                ErrorKind::Wifi,
            )
        })?)
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
    #[instrument]
    pub async fn list_networks_low(&self) -> Result<BTreeMap<NetworkId, WifiInfo>, Error> {
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
                let info = WifiInfo {
                    ssid: name,
                    device: device.map(|x| x.to_owned()),
                };
                Some((uuid, info))
            })
            .collect::<BTreeMap<NetworkId, WifiInfo>>())
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
                let mut values = l.split(':');
                let ssid = Ssid(values.next()?.to_owned());
                let signal = SignalStrength::new(std::str::FromStr::from_str(values.next()?).ok());
                let security: Vec<String> =
                    values.next()?.split(' ').map(|x| x.to_owned()).collect();
                Some((
                    ssid,
                    WifiListInfo {
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
    pub async fn save_config(&mut self, mut db: impl DbHandle) -> Result<(), Error> {
        crate::db::DatabaseModel::new()
            .server_info()
            .last_wifi_region()
            .put(&mut db, &Some(self.get_country_low().await?))
            .await?;
        Ok(())
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
    #[instrument(skip(db))]
    pub async fn select_network(&mut self, db: impl DbHandle, ssid: &Ssid) -> Result<bool, Error> {
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
    #[instrument(skip(db))]
    pub async fn remove_network(&mut self, db: impl DbHandle, ssid: &Ssid) -> Result<bool, Error> {
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
    #[instrument(skip(psk, db))]
    pub async fn set_add_network(
        &mut self,
        db: impl DbHandle,
        ssid: &Ssid,
        psk: &Psk,
        priority: isize,
    ) -> Result<(), Error> {
        self.set_add_network_low(ssid, psk).await?;
        self.save_config(db).await?;
        Ok(())
    }
    #[instrument(skip(psk, db))]
    pub async fn add_network(
        &mut self,
        db: impl DbHandle,
        ssid: &Ssid,
        psk: &Psk,
        priority: isize,
    ) -> Result<(), Error> {
        self.add_network_low(ssid, psk).await?;
        self.save_config(db).await?;
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
        .find(|s| s.contains("inet"));
    Ok(v.is_some())
}

pub fn country_code_parse(code: &str, _matches: &ArgMatches) -> Result<CountryCode, Error> {
    CountryCode::for_alpha2(code).map_err(|_| {
        Error::new(
            color_eyre::eyre::eyre!("Invalid Country Code: {}", code),
            ErrorKind::Wifi,
        )
    })
}

#[instrument(skip(main_datadir))]
pub async fn synchronize_wpa_supplicant_conf<P: AsRef<Path>>(
    main_datadir: P,
    last_country_code: &Option<CountryCode>,
) -> Result<(), Error> {
    let persistent = main_datadir.as_ref().join("system-connections");
    tracing::debug!("persistent: {:?}", persistent);
    // let supplicant = Path::new("/etc/wpa_supplicant.conf");

    if tokio::fs::metadata(&persistent).await.is_err() {
        tokio::fs::create_dir_all(&persistent).await?;
    }
    crate::disk::mount::util::bind(&persistent, "/etc/NetworkManager/system-connections", false)
        .await?;
    // if tokio::fs::metadata(&supplicant).await.is_err() {
    //     tokio::fs::write(&supplicant, include_str!("wpa_supplicant.conf.base")).await?;
    // }

    Command::new("systemctl")
        .arg("restart")
        .arg("NetworkManager")
        .invoke(ErrorKind::Wifi)
        .await?;
    Command::new("ifconfig")
        .arg("wlan0")
        .arg("up")
        .invoke(ErrorKind::Wifi)
        .await?;
    if let Some(last_country_code) = last_country_code {
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
