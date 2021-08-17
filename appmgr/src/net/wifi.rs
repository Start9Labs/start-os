use std::collections::HashMap;
use std::str::from_utf8;
use std::time::Duration;

use futures::Future;
use rpc_toolkit::command;

use crate::context::EitherContext;
use crate::util::display_none;
use crate::{cmd, Error, ErrorKind};

#[command(subcommands(add, connect, delete, get))]
pub async fn wifi(#[context] ctx: EitherContext) -> Result<EitherContext, Error> {
    Ok(ctx)
}

#[derive(Debug, Clone, serde::Serialize, serde::Deserialize)]
pub struct AddWifiReq {
    ssid: String,
    password: String,
    country: String,
    priority: isize,
    connect: bool,
}
#[command(rpc_only)]
pub async fn add(#[context] ctx: EitherContext, #[arg] req: AddWifiReq) -> Result<(), Error> {
    let wpa_supplicant = WpaCli { interface: "wlan0" };
    if !req.ssid.is_ascii() {
        return Err(Error::new(
            anyhow::anyhow!("SSID not in the ASCII charset"),
            ErrorKind::WifiError,
        ));
    }
    if !req.password.is_ascii() {
        return Err(Error::new(
            anyhow::anyhow!("Wifi Password not in the ASCII charset"),
            ErrorKind::WifiError,
        ));
    }
    async fn add_procedure<'a>(wpa_supplicant: WpaCli<'a>, req: &AddWifiReq) -> Result<(), Error> {
        log::info!("Adding new WiFi network: '{}'", req.ssid);
        wpa_supplicant
            .add_network(&req.ssid, &req.password, req.priority)
            .await?;
        if req.connect {
            let current = wpa_supplicant.get_current_network().await?;
            let connected = wpa_supplicant.select_network(&req.ssid).await?;
            if !connected {
                log::error!("Faild to add new WiFi network: '{}'", req.ssid);
                wpa_supplicant.remove_network(&req.ssid).await?;
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
        match add_procedure(wpa_supplicant, &req).await {
            Err(e) => {
                log::error!("Failed to add new Wifi network '{}': {}", req.ssid, e);
            }
            Ok(_) => {}
        }
    });
    Ok(())
}

#[command(display(display_none))]
pub async fn connect(#[context] ctx: EitherContext, #[arg] ssid: String) -> Result<(), Error> {
    if !ssid.is_ascii() {
        return Err(Error::new(
            anyhow::anyhow!("SSID not in the ASCII charset"),
            ErrorKind::WifiError,
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
                    log::error!("No WiFi to revert to!");
                }
                Some(current) => {
                    wpa_supplicant.select_network(&ssid).await?;
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
pub async fn delete(#[context] ctx: EitherContext, #[arg] ssid: String) -> Result<(), Error> {
    if !ssid.is_ascii() {
        return Err(Error::new(
            anyhow::anyhow!("SSID not in ASCII charset"),
            ErrorKind::WifiError,
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
                    return Err(Error::new(anyhow::anyhow!("Forbidden: Deleting this Network would make your Embassy Unreachable. Either connect to ethernet or connect to a different WiFi network to remedy this."), ErrorKind::WifiError));
                }
            }
        }
    }
    Ok(())
}
#[derive(serde::Serialize, serde::Deserialize)]
pub struct WiFiInfo {
    ssids: Vec<String>,
    selected: Option<String>,
    connected: Option<String>,
    country: String,
    ethernet: bool,
}
#[command(display(display_none))]
pub async fn get(#[context] ctx: EitherContext) -> Result<WiFiInfo, Error> {
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
    let ethernet_task = interface_connected("eth0");
    let (ssids_res, current_res, country_res, ethernet_res) =
        tokio::join!(ssids_task, current_task, country_task, ethernet_task);
    let current = current_res?;
    Ok(WiFiInfo {
        ssids: ssids_res?,
        selected: current.clone(),
        connected: current,
        country: country_res?,
        ethernet: ethernet_res?,
    })
}

pub struct WpaCli<'a> {
    interface: &'a str,
}
#[derive(Clone)]
struct NetworkId(String);
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
        let r = cmd!(wpa_cli -i ((self.interface)) add_network)
            .output()
            .await?;
        r.status
            .exit_ok()
            .map_err(|e| e.with_kind(ErrorKind::WifiError))?;
        let s = from_utf8(r.stdout)?;
        Ok(NetworkId(s.trim().to_owned()))
    }
    pub async fn set_network_low(&self, id: &NetworkId, attr: &NetworkAttr) -> Result<(), Error> {
        let r = cmd!(wpa_cli -i ((self.interface)) set_network ((id)) ((attr)))
            .output()
            .await?;
        r.status
            .exit_ok()
            .map_err(|e| e.with_kind(ErrorKind::WifiError))?;
        Ok(())
    }
    pub async fn set_country_low(&self, country_code: &str) -> Result<(), Error> {
        let r = cmd!(wpa_cli -i ((self.interface)) set country ((country_code)))
            .output()
            .await?;
        r.status
            .exit_ok()
            .map_err(|e| e.with_kind(ErrorKind::WifiError))?;
        Ok(())
    }
    pub async fn get_country_low(&self) -> Result<String, Error> {
        let r = cmd!(wpa_cli -i ((self.interface)) get country)
            .output()
            .await?;
        r.status
            .exit_ok()
            .map_err(|e| e.with_kind(ErrorKind::WifiError))?;
        Ok(r.stdout)
    }
    pub async fn enable_network_low(&self, id: &NetworkId) -> Result<(), Error> {
        let r = cmd!(wpa_cli -i ((self.interface)) enable_network ((id)))
            .output()
            .await?;
        r.status
            .exit_ok()
            .map_err(|e| e.with_kind(ErrorKind::WifiError))?;
        Ok(())
    }
    pub async fn save_config_low(&self) -> Result<(), Error> {
        let r = cmd!(wpa_cli -i ((self.interface)) save_config)
            .output()
            .await?;
        r.status
            .exit_ok()
            .map_err(|e| e.with_kind(ErrorKind::WifiError))?;
        Ok(())
    }
    pub async fn remove_network_low(&self, id: NetworkId) -> Result<(), Error> {
        let r = cmd!(wpa_cli -i ((self.interface)) remove_network ((id)))
            .output()
            .await?;
        r.status
            .exit_ok()
            .map_err(|e| e.with_kind(ErrorKind::WifiError))?;
        Ok(())
    }
    pub async fn reconfigure_low(&self) -> Result<(), Error> {
        let r = cmd!(wpa_cli -i ((self.interface)) reconfigure)
            .output()
            .await?;
        r.status
            .exit_ok()
            .map_err(|e| e.with_kind(ErrorKind::WifiError))?;
        Ok(())
    }
    pub async fn list_networks_low(&self) -> Result<HashMap<String, NetworkId>, Error> {
        let r = cmd!(wpa_cli -i ((self.interface)) list_networks)
            .output()
            .await?;
        r.status
            .exit_ok()
            .map_err(|e| e.with_kind(ErrorKind::WifiError))?;
        Ok(std::str::from_utf8(r.stdout)?
            .lines()
            .skip(1)
            .filter_map(|l| {
                let cs = l.split("\t");
                Some((cs.nth(1)?.to_owned(), NetworkId(cs.nth(0)?.to_owned())))
            })
            .collect::<HashMap<String, NetworkId>>())
    }
    pub async fn select_network_low(&self, id: &NetworkId) -> Result<(), Error> {
        let r = cmd!(wpa_cli -i ((self.interface)) select_network ((id)))
            .output()
            .await?;
        r.status
            .exit_ok()
            .map_err(|e| e.with_kind(ErrorKind::WifiError))?;
        Ok(())
    }
    pub async fn new_password_low(&self, id: &NetworkId, pass: &str) -> Result<(), Error> {
        let r = cmd!(wpa_cli -i ((self.interface)) new_password ((id)) ((pass)))
            .output()
            .await?;
        r.status
            .exit_ok()
            .map_err(|e| e.with_kind(ErrorKind::WifiError))?;
        Ok(())
    }

    // High Level
    pub async fn check_network(&self, ssid: &str) -> Result<Option<NetworkId>, Error> {
        Ok(self
            .list_networks_low()
            .await?
            .get(ssid)
            .map(|a| (*a).clone()))
    }
    pub async fn select_network(&self, ssid: &str) -> Result<bool, Error> {
        let m_id = self.check_network(ssid).await?;
        match m_id {
            None => Err(Error::new(
                anyhow::anyhow!("SSID Not Found"),
                ErrorKind::WifiError,
            )),
            Some(x) => {
                self.select_network_low(&x).await?;
                self.save_config_low().await?;
                let connect = async {
                    let mut current = Ok(None);
                    loop {
                        current = self.get_current_network().await;
                        match &current {
                            Ok(Some(x)) => {
                                break;
                            }
                            _ => {}
                        }
                    }
                    current
                };
                let timeout = tokio::time::sleep(Duration::from_secs(20));
                let res = tokio::select! {
                    net = connect => { net? }
                    _ = timeout => { None }
                };
                Ok(match res {
                    None => false,
                    Some(net) => net == ssid,
                })
            }
        }
    }
    pub async fn get_current_network(&self) -> Result<Option<String>, Error> {
        let r = cmd!(iwgetid((self.interface)) - -raw).output().await?;
        r.status
            .exit_ok()
            .map_err(|e| e.with_kind(ErrorKind::WifiError))?;
        if r.stdout == "" {
            Ok(None)
        } else {
            Ok(Some(r.stdout))
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
    let out = cmd!(ifconfig((interface))).output().await?;
    let v = std::str::from_utf8(&out.stdout)?
        .lines()
        .filter(|s| s.contains("inet"))
        .next();
    Ok(!v.is_none())
}

#[tokio::test]
pub async fn test_interface_connected() {
    println!("{}", interface_connected("wlp5s0").await.unwrap());
    println!("{}", interface_connected("enp4s0f1").await.unwrap());
}
