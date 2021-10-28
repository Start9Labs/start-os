use std::collections::BTreeMap;
use std::path::{Path, PathBuf};
use std::sync::Arc;
use std::time::Duration;

use clap::ArgMatches;
use isocountry::CountryCode;
use rpc_toolkit::command;
use tokio::process::Command;
use tokio::sync::RwLock;
use tracing::instrument;

use crate::context::RpcContext;
use crate::util::serde::{display_serializable, IoFormat};
use crate::util::{display_none, Invoke};
use crate::{Error, ErrorKind};

#[command(subcommands(add, connect, delete, get, set_country))]
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
        ssid: &str,
        password: &str,
        priority: isize,
        connect: bool,
    ) -> Result<(), Error> {
        tracing::info!("Adding new WiFi network: '{}'", ssid);
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
                tracing::info!("Failed to add new WiFi network: '{}'", ssid);
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
            &ssid,
            &password,
            priority,
            connect,
        )
        .await
        {
            Err(e) => {
                tracing::info!("Failed to add new WiFi network '{}': {}", ssid, e);
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
        ssid: &String,
    ) -> Result<(), Error> {
        let wpa_supplicant = wifi_manager.read().await;
        let current = wpa_supplicant.get_current_network().await?;
        drop(wpa_supplicant);
        let mut wpa_supplicant = wifi_manager.write().await;
        let connected = wpa_supplicant.select_network(&ssid).await?;
        if connected {
            tracing::info!("Successfully connected to WiFi: '{}'", ssid);
        } else {
            tracing::info!("Failed to connect to WiFi: '{}'", ssid);
            match current {
                None => {
                    tracing::info!("No WiFi to revert to!");
                }
                Some(current) => {
                    wpa_supplicant.select_network(&current).await?;
                }
            }
        }
        Ok(())
    }
    tokio::spawn(async move {
        match connect_procedure(ctx.wifi_manager.clone(), &ssid).await {
            Err(e) => {
                tracing::info!("Failed to connect to WiFi network '{}': {}", &ssid, e);
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
        connected: current,
        country: country_res?,
        ethernet: ethernet_res?,
        signal_strength,
    })
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
#[derive(Clone)]
pub struct NetworkId(String);
pub enum NetworkAttr {
    Ssid(String),
    Psk(String),
    Priority(isize),
    ScanSsid(bool),
}
impl NetworkAttr {
    fn name(&self) -> &'static str {
        use NetworkAttr::*;
        match self {
            Ssid(_) => "ssid",
            Psk(_) => "psk",
            Priority(_) => "priority",
            ScanSsid(_) => "scan_ssid",
        }
    }
    fn value(&self) -> String {
        use NetworkAttr::*;
        match self {
            Ssid(s) => format!("\"{}\"", s),
            Psk(s) => format!("\"{}\"", s),
            Priority(n) => format!("{}", n),
            ScanSsid(b) => {
                if *b {
                    String::from("1")
                } else {
                    String::from("0")
                }
            }
        }
    }
}
impl WpaCli {
    pub fn init(interface: String, datadir: PathBuf) -> Self {
        WpaCli { interface, datadir }
    }

    // Low Level
    pub async fn add_network_low(&mut self) -> Result<NetworkId, Error> {
        let r = Command::new("wpa_cli")
            .arg("-i")
            .arg(&self.interface)
            .arg("add_network")
            .invoke(ErrorKind::Wifi)
            .await?;
        let s = std::str::from_utf8(&r)?;
        Ok(NetworkId(s.trim().to_owned()))
    }
    pub async fn set_network_low(
        &mut self,
        id: &NetworkId,
        attr: &NetworkAttr,
    ) -> Result<(), Error> {
        let _ = Command::new("wpa_cli")
            .arg("-i")
            .arg(&self.interface)
            .arg("set_network")
            .arg(&id.0)
            .arg(attr.name())
            .arg(attr.value())
            .invoke(ErrorKind::Wifi)
            .await?;
        Ok(())
    }
    pub async fn set_country_low(&mut self, country_code: &str) -> Result<(), Error> {
        let _ = Command::new("wpa_cli")
            .arg("-i")
            .arg(&self.interface)
            .arg("set")
            .arg("country")
            .arg(country_code)
            .invoke(ErrorKind::Wifi)
            .await?;
        Ok(())
    }
    pub async fn get_country_low(&self) -> Result<CountryCode, Error> {
        let r = Command::new("wpa_cli")
            .arg("-i")
            .arg(&self.interface)
            .arg("get")
            .arg("country")
            .invoke(ErrorKind::Wifi)
            .await?;
        Ok(CountryCode::for_alpha2(&String::from_utf8(r)?).unwrap())
    }
    pub async fn enable_network_low(&mut self, id: &NetworkId) -> Result<(), Error> {
        let _ = Command::new("wpa_cli")
            .arg("-i")
            .arg(&self.interface)
            .arg("enable_network")
            .arg(&id.0)
            .invoke(ErrorKind::Wifi)
            .await?;
        Ok(())
    }
    pub async fn save_config_low(&mut self) -> Result<(), Error> {
        let _ = Command::new("wpa_cli")
            .arg("-i")
            .arg(&self.interface)
            .arg("save_config")
            .invoke(ErrorKind::Wifi)
            .await?;
        Ok(())
    }
    pub async fn remove_network_low(&mut self, id: NetworkId) -> Result<(), Error> {
        let _ = Command::new("wpa_cli")
            .arg("-i")
            .arg(&self.interface)
            .arg("remove_network")
            .arg(&id.0)
            .invoke(ErrorKind::Wifi)
            .await?;
        Ok(())
    }
    pub async fn reconfigure_low(&mut self) -> Result<(), Error> {
        let _ = Command::new("wpa_cli")
            .arg("-i")
            .arg(&self.interface)
            .arg("reconfigure")
            .invoke(ErrorKind::Wifi)
            .await?;
        Ok(())
    }
    #[instrument]
    pub async fn list_networks_low(&self) -> Result<BTreeMap<String, NetworkId>, Error> {
        let r = Command::new("wpa_cli")
            .arg("-i")
            .arg(&self.interface)
            .arg("list_networks")
            .invoke(ErrorKind::Wifi)
            .await?;
        Ok(String::from_utf8(r)?
            .lines()
            .skip(1)
            .filter_map(|l| {
                let mut cs = l.split("\t");
                let nid = NetworkId(cs.next()?.to_owned());
                let ssid = cs.next()?.to_owned();
                Some((ssid, nid))
            })
            .collect::<BTreeMap<String, NetworkId>>())
    }
    pub async fn select_network_low(&mut self, id: &NetworkId) -> Result<(), Error> {
        let _ = Command::new("wpa_cli")
            .arg("-i")
            .arg(&self.interface)
            .arg("select_network")
            .arg(&id.0)
            .invoke(ErrorKind::Wifi)
            .await?;
        Ok(())
    }
    pub async fn new_password_low(&mut self, id: &NetworkId, pass: &str) -> Result<(), Error> {
        let _ = Command::new("wpa_cli")
            .arg("-i")
            .arg(&self.interface)
            .arg("new_password")
            .arg(&id.0)
            .arg(pass)
            .invoke(ErrorKind::Wifi)
            .await?;
        Ok(())
    }
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

    // High Level
    pub async fn save_config(&mut self) -> Result<(), Error> {
        self.save_config_low().await?;
        tokio::fs::copy(
            "/etc/wpa_supplicant.conf",
            self.datadir.join("wpa_supplicant.conf"),
        )
        .await?;
        Ok(())
    }
    pub async fn check_network(&self, ssid: &str) -> Result<Option<NetworkId>, Error> {
        Ok(self.list_networks_low().await?.remove(ssid))
    }
    #[instrument]
    pub async fn select_network(&mut self, ssid: &str) -> Result<bool, Error> {
        let m_id = self.check_network(ssid).await?;
        match m_id {
            None => Err(Error::new(
                color_eyre::eyre::eyre!("SSID Not Found"),
                ErrorKind::Wifi,
            )),
            Some(x) => {
                self.select_network_low(&x).await?;
                self.save_config().await?;
                let connect = async {
                    let mut current;
                    loop {
                        current = self.get_current_network().await;
                        match &current {
                            Ok(Some(ssid)) => {
                                tracing::debug!("Connected to: {}", ssid);
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
                    Some(net) => net == ssid,
                })
            }
        }
    }
    #[instrument]
    pub async fn get_current_network(&self) -> Result<Option<String>, Error> {
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
            Ok(Some(network.to_owned()))
        }
    }
    #[instrument]
    pub async fn remove_network(&mut self, ssid: &str) -> Result<bool, Error> {
        match self.check_network(ssid).await? {
            None => Ok(false),
            Some(x) => {
                self.remove_network_low(x).await?;
                self.save_config().await?;
                self.reconfigure_low().await?;
                Ok(true)
            }
        }
    }
    #[instrument]
    pub async fn add_network(
        &mut self,
        ssid: &str,
        psk: &str,
        priority: isize,
    ) -> Result<(), Error> {
        use NetworkAttr::*;
        let nid = match self.check_network(ssid).await? {
            None => {
                let nid = self.add_network_low().await?;
                self.set_network_low(&nid, &Ssid(ssid.to_owned())).await?;
                self.set_network_low(&nid, &Psk(psk.to_owned())).await?;
                self.set_network_low(&nid, &Priority(priority)).await?;
                self.set_network_low(&nid, &ScanSsid(true)).await?;
                Result::<NetworkId, Error>::Ok(nid)
            }
            Some(nid) => {
                self.new_password_low(&nid, psk).await?;
                Ok(nid)
            }
        }?;
        self.enable_network_low(&nid).await?;
        self.save_config().await?;
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
