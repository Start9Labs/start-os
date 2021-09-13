use std::collections::HashMap;
use std::path::Path;
use std::time::Duration;

use clap::ArgMatches;
use isocountry::CountryCode;
use rpc_toolkit::command;
use tokio::process::Command;

use crate::util::{display_none, display_serializable, Invoke, IoFormat};
use crate::{Error, ErrorKind};

#[command(subcommands(add, connect, delete, get, set_country))]
pub async fn wifi() -> Result<(), Error> {
    Ok(())
}

#[command(display(display_none))]
pub async fn add(
    #[arg] ssid: String,
    #[arg] password: String,
    #[arg] priority: isize,
    #[arg] connect: bool,
) -> Result<(), Error> {
    let wpa_supplicant = WpaCli { interface: "wlan0" }; // TODO: pull from config
    if !ssid.is_ascii() {
        return Err(Error::new(
            anyhow::anyhow!("SSID may not have special characters"),
            ErrorKind::Wifi,
        ));
    }
    if !password.is_ascii() {
        return Err(Error::new(
            anyhow::anyhow!("WiFi Password may not have special characters"),
            ErrorKind::Wifi,
        ));
    }
    async fn add_procedure<'a>(
        wpa_supplicant: WpaCli<'a>,
        ssid: &str,
        password: &str,
        priority: isize,
        connect: bool,
    ) -> Result<(), Error> {
        log::info!("Adding new WiFi network: '{}'", ssid);
        wpa_supplicant.add_network(ssid, password, priority).await?;
        if connect {
            let current = wpa_supplicant.get_current_network().await?;
            let connected = wpa_supplicant.select_network(ssid).await?;
            if !connected {
                log::error!("Faild to add new WiFi network: '{}'", ssid);
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
        match add_procedure(wpa_supplicant, &ssid, &password, priority, connect).await {
            Err(e) => {
                log::error!("Failed to add new WiFi network '{}': {}", ssid, e);
            }
            Ok(_) => {}
        }
    });
    Ok(())
}

#[command(display(display_none))]
pub async fn connect(#[arg] ssid: String) -> Result<(), Error> {
    if !ssid.is_ascii() {
        return Err(Error::new(
            anyhow::anyhow!("SSID may not have special characters"),
            ErrorKind::Wifi,
        ));
    }
    async fn connect_procedure<'a>(wpa_supplicant: WpaCli<'a>, ssid: &String) -> Result<(), Error> {
        let current = wpa_supplicant.get_current_network().await?;
        let connected = wpa_supplicant.select_network(&ssid).await?;
        if connected {
            log::info!("Successfully connected to WiFi: '{}'", ssid);
        } else {
            log::error!("Failed to connect to WiFi: '{}'", ssid);
            match current {
                None => {
                    log::warn!("No WiFi to revert to!");
                }
                Some(current) => {
                    wpa_supplicant.select_network(&current).await?;
                }
            }
        }
        Ok(())
    }
    let wpa_supplicant = WpaCli { interface: "wlan0" };
    tokio::spawn(async move {
        match connect_procedure(wpa_supplicant, &ssid).await {
            Err(e) => {
                log::error!("Failed to connect to WiFi network '{}': {}", &ssid, e);
            }
            Ok(_) => {}
        }
    });
    Ok(())
}

#[command(display(display_none))]
pub async fn delete(#[arg] ssid: String) -> Result<(), Error> {
    if !ssid.is_ascii() {
        return Err(Error::new(
            anyhow::anyhow!("SSID may not have special characters"),
            ErrorKind::Wifi,
        ));
    }
    let wpa_supplicant = WpaCli { interface: "wlan0" };
    let current = wpa_supplicant.get_current_network().await?;
    match current {
        None => {
            wpa_supplicant.remove_network(&ssid).await?;
        }
        Some(current) => {
            if current == ssid {
                if interface_connected("eth0").await? {
                    wpa_supplicant.remove_network(&ssid).await?;
                } else {
                    return Err(Error::new(anyhow::anyhow!("Forbidden: Deleting this Network would make your Embassy Unreachable. Either connect to ethernet or connect to a different WiFi network to remedy this."), ErrorKind::Wifi));
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
pub async fn get(
    #[allow(unused_variables)]
    #[arg(long = "format")]
    format: Option<IoFormat>,
) -> Result<WiFiInfo, Error> {
    let wpa_supplicant = WpaCli { interface: "wlan0" };
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
pub async fn set_country(
    #[arg(parse(country_code_parse))] country: CountryCode,
) -> Result<(), Error> {
    let wpa_supplicant = WpaCli { interface: "wlan0" };
    wpa_supplicant.set_country_low(country.alpha2()).await
}

pub struct WpaCli<'a> {
    interface: &'a str,
}
#[derive(Clone)]
pub struct NetworkId(String);
pub enum NetworkAttr {
    Ssid(String),
    Psk(String),
    Priority(isize),
    ScanSsid(bool),
}
impl std::fmt::Display for NetworkAttr {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        use NetworkAttr::*;
        match self {
            Ssid(s) => write!(f, "\"{}\"", s),
            Psk(s) => write!(f, "\"{}\"", s),
            Priority(n) => write!(f, "{}", n),
            ScanSsid(b) => {
                if *b {
                    write!(f, "1")
                } else {
                    write!(f, "0")
                }
            }
        }
    }
}
impl<'a> WpaCli<'a> {
    // Low Level
    pub async fn add_network_low(&self) -> Result<NetworkId, Error> {
        let r = Command::new("wpa_cli")
            .arg("-i")
            .arg(self.interface)
            .arg("add_network")
            .invoke(ErrorKind::Wifi)
            .await?;
        let s = std::str::from_utf8(&r)?;
        Ok(NetworkId(s.trim().to_owned()))
    }
    pub async fn set_network_low(&self, id: &NetworkId, attr: &NetworkAttr) -> Result<(), Error> {
        let _ = Command::new("wpa_cli")
            .arg("-i")
            .arg(self.interface)
            .arg("set_network")
            .arg(&id.0)
            .arg(format!("{}", attr))
            .invoke(ErrorKind::Wifi)
            .await?;
        Ok(())
    }
    pub async fn set_country_low(&self, country_code: &str) -> Result<(), Error> {
        let _ = Command::new("wpa_cli")
            .arg("-i")
            .arg(self.interface)
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
            .arg(self.interface)
            .arg("get")
            .arg("country")
            .invoke(ErrorKind::Wifi)
            .await?;
        Ok(CountryCode::for_alpha2(&String::from_utf8(r)?).unwrap())
    }
    pub async fn enable_network_low(&self, id: &NetworkId) -> Result<(), Error> {
        let _ = Command::new("wpa_cli")
            .arg("-i")
            .arg(self.interface)
            .arg("enable_network")
            .arg(&id.0)
            .invoke(ErrorKind::Wifi)
            .await?;
        Ok(())
    }
    pub async fn save_config_low(&self) -> Result<(), Error> {
        let _ = Command::new("wpa_cli")
            .arg("-i")
            .arg(self.interface)
            .arg("save_config")
            .invoke(ErrorKind::Wifi)
            .await?;
        Ok(())
    }
    pub async fn remove_network_low(&self, id: NetworkId) -> Result<(), Error> {
        let _ = Command::new("wpa_cli")
            .arg("-i")
            .arg(self.interface)
            .arg("remove_network")
            .arg(&id.0)
            .invoke(ErrorKind::Wifi)
            .await?;
        Ok(())
    }
    pub async fn reconfigure_low(&self) -> Result<(), Error> {
        let _ = Command::new("wpa_cli")
            .arg("-i")
            .arg(self.interface)
            .arg("reconfigure")
            .invoke(ErrorKind::Wifi)
            .await?;
        Ok(())
    }
    pub async fn list_networks_low(&self) -> Result<HashMap<String, NetworkId>, Error> {
        let r = Command::new("wpa_cli")
            .arg("-i")
            .arg(self.interface)
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
            .collect::<HashMap<String, NetworkId>>())
    }
    pub async fn select_network_low(&self, id: &NetworkId) -> Result<(), Error> {
        let _ = Command::new("wpa_cli")
            .arg("-i")
            .arg(self.interface)
            .arg("select_network")
            .arg(&id.0)
            .invoke(ErrorKind::Wifi)
            .await?;
        Ok(())
    }
    pub async fn new_password_low(&self, id: &NetworkId, pass: &str) -> Result<(), Error> {
        let _ = Command::new("wpa_cli")
            .arg("-i")
            .arg(self.interface)
            .arg("new_password")
            .arg(&id.0)
            .arg(pass)
            .invoke(ErrorKind::Wifi)
            .await?;
        Ok(())
    }
    pub async fn signal_poll_low(&self) -> Result<Option<isize>, Error> {
        let r = Command::new("wpa_cli")
            .arg("-i")
            .arg(self.interface)
            .arg("signal_poll")
            .invoke(ErrorKind::Wifi)
            .await?;
        let e = || {
            Error::new(
                anyhow::anyhow!("Invalid output from wpa_cli signal_poll"),
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
    pub async fn check_network(&self, ssid: &str) -> Result<Option<NetworkId>, Error> {
        Ok(self.list_networks_low().await?.remove(ssid))
    }
    pub async fn select_network(&self, ssid: &str) -> Result<bool, Error> {
        let m_id = self.check_network(ssid).await?;
        match m_id {
            None => Err(Error::new(
                anyhow::anyhow!("SSID Not Found"),
                ErrorKind::Wifi,
            )),
            Some(x) => {
                self.select_network_low(&x).await?;
                self.save_config_low().await?;
                let connect = async {
                    let mut current;
                    loop {
                        current = self.get_current_network().await;
                        match &current {
                            Ok(Some(_)) => {
                                break;
                            }
                            _ => {}
                        }
                    }
                    current
                };
                let res = match tokio::time::timeout(Duration::from_secs(20), connect).await {
                    Err(_) => None,
                    Ok(net) => net?,
                };
                Ok(match res {
                    None => false,
                    Some(net) => net == ssid,
                })
            }
        }
    }
    pub async fn get_current_network(&self) -> Result<Option<String>, Error> {
        let r = Command::new("iwgetid")
            .arg(self.interface)
            .arg("--raw")
            .invoke(ErrorKind::Wifi)
            .await?;
        let output = String::from_utf8(r)?;
        if output.trim().is_empty() {
            Ok(None)
        } else {
            Ok(Some(output))
        }
    }
    pub async fn remove_network(&self, ssid: &str) -> Result<bool, Error> {
        match self.check_network(ssid).await? {
            None => Ok(false),
            Some(x) => {
                self.remove_network_low(x).await?;
                self.save_config_low().await?;
                self.reconfigure_low().await?;
                Ok(true)
            }
        }
    }
    pub async fn add_network(&self, ssid: &str, psk: &str, priority: isize) -> Result<(), Error> {
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
        self.save_config_low().await?;
        Ok(())
    }
}

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
        anyhow::anyhow!("Invalid Country Code: {}", code),
        ErrorKind::Wifi,
    )))
}

pub async fn synchronize_wpa_supplicant_conf<P: AsRef<Path>>(main_datadir: P) -> Result<(), Error> {
    let target = main_datadir.as_ref().join("wpa_supplicant.conf");
    if tokio::fs::metadata(&target).await.is_err() {
        tokio::fs::write(&target, include_str!("wpa_supplicant.conf.base")).await?;
    }
    let link = Path::new("/etc/wpa_supplicant.conf");
    if let Ok(meta) = tokio::fs::symlink_metadata(&link).await {
        if meta.file_type().is_symlink() {
            tokio::fs::remove_file(&link).await?
        }
    }
    tokio::fs::symlink(&target, link).await?;
    Ok(())
}

#[tokio::test]
pub async fn test_interface_connected() {
    println!("{}", interface_connected("wlp5s0").await.unwrap());
    println!("{}", interface_connected("enp4s0f1").await.unwrap());
}

#[tokio::test]
pub async fn test_signal_strength() {
    let wpa = WpaCli {
        interface: "wlp5s0",
    };
    println!("{}", wpa.signal_poll_low().await.unwrap().unwrap())
}
