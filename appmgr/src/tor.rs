use std::collections::{BTreeSet, HashMap};
use std::net::Ipv4Addr;
use std::os::unix::process::ExitStatusExt;
use std::path::Path;
use std::time::{Duration, Instant};

use failure::ResultExt as _;
use tokio::io::AsyncReadExt;
use tokio::io::AsyncWriteExt;

use crate::util::{PersistencePath, YamlUpdateHandle};
use crate::{Error, ResultExt as _};

#[derive(Debug, Clone, Copy, serde::Deserialize, serde::Serialize)]
pub struct PortMapping {
    pub internal: u16,
    pub tor: u16,
}

pub const ETC_TOR_RC: &'static str = "/etc/tor/torrc";
pub const HIDDEN_SERVICE_DIR_ROOT: &'static str = "/var/lib/tor";
pub const ETC_HOSTNAME: &'static str = "/etc/hostname";
pub const ETC_NGINX_SERVICES_CONF: &'static str = "/etc/nginx/sites-available/start9-services.conf";

#[derive(Debug, Clone, Copy, serde::Deserialize, serde::Serialize)]
#[serde(rename_all = "lowercase")]
pub enum HiddenServiceVersion {
    V1,
    V2,
    V3,
}
impl From<HiddenServiceVersion> for usize {
    fn from(v: HiddenServiceVersion) -> Self {
        match v {
            HiddenServiceVersion::V1 => 1,
            HiddenServiceVersion::V2 => 2,
            HiddenServiceVersion::V3 => 3,
        }
    }
}
impl std::convert::TryFrom<usize> for HiddenServiceVersion {
    type Error = failure::Error;
    fn try_from(v: usize) -> Result<Self, Self::Error> {
        Ok(match v {
            1 => HiddenServiceVersion::V1,
            2 => HiddenServiceVersion::V2,
            3 => HiddenServiceVersion::V3,
            n => bail!("Invalid HiddenServiceVersion {}", n),
        })
    }
}
impl Default for HiddenServiceVersion {
    fn default() -> Self {
        HiddenServiceVersion::V3
    }
}
impl std::fmt::Display for HiddenServiceVersion {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "HiddenServiceVersion {}", usize::from(*self))
    }
}

#[derive(Debug, Clone, serde::Deserialize, serde::Serialize)]
pub struct Service {
    pub ip: Ipv4Addr,
    pub ports: Vec<PortMapping>,
    #[serde(default)]
    pub hidden_service_version: HiddenServiceVersion,
}

#[derive(Debug, Clone, serde::Deserialize, serde::Serialize)]
pub struct NewService {
    pub ports: Vec<PortMapping>,
    #[serde(default)]
    pub hidden_service_version: HiddenServiceVersion,
}

#[derive(Debug, Clone, serde::Deserialize, serde::Serialize)]
pub struct ServicesMap {
    pub map: HashMap<String, Service>,
    pub ips: BTreeSet<Ipv4Addr>,
}
impl Default for ServicesMap {
    fn default() -> Self {
        ServicesMap {
            map: Default::default(),
            ips: Default::default(),
        }
    }
}
impl ServicesMap {
    pub fn add(&mut self, name: String, service: NewService) -> Ipv4Addr {
        let ip = self
            .map
            .get(&name)
            .map(|a| a.ip.clone())
            .unwrap_or_else(|| {
                Ipv4Addr::from(
                    u32::from(
                        self.ips
                            .range(..)
                            .next_back()
                            .cloned()
                            .unwrap_or_else(|| crate::HOST_IP.into()),
                    ) + 1,
                )
            });
        self.ips.insert(ip);
        self.map.insert(
            name,
            Service {
                ip,
                ports: service.ports,
                hidden_service_version: service.hidden_service_version,
            },
        );
        ip
    }
    pub fn remove(&mut self, name: &str) {
        let s = self.map.remove(name);
        if let Some(s) = s {
            self.ips.remove(&s.ip);
        }
    }
}

pub async fn services_map(path: &PersistencePath) -> Result<ServicesMap, Error> {
    let f = path.maybe_read(false).await.transpose()?;
    if let Some(mut f) = f {
        crate::util::from_yaml_async_reader(&mut *f).await
    } else {
        Ok(Default::default())
    }
}

pub async fn services_map_mut(
    path: PersistencePath,
) -> Result<YamlUpdateHandle<ServicesMap>, Error> {
    YamlUpdateHandle::new_or_default(path).await
}

pub async fn write_services(hidden_services: &ServicesMap) -> Result<(), Error> {
    tokio::fs::copy(crate::TOR_RC, ETC_TOR_RC)
        .await
        .with_context(|e| format!("{} -> {}: {}", crate::TOR_RC, ETC_TOR_RC, e))
        .with_code(crate::error::FILESYSTEM_ERROR)?;
    let mut f = tokio::fs::OpenOptions::new()
        .append(true)
        .open(ETC_TOR_RC)
        .await?;
    f.write_all(b"\n").await?;
    for (name, service) in &hidden_services.map {
        if service.ports.is_empty() {
            continue;
        }
        f.write_all(b"\n").await?;
        f.write_all(format!("# HIDDEN SERVICE FOR {}\n", name).as_bytes())
            .await?;
        f.write_all(
            format!(
                "HiddenServiceDir {}/app-{}/\n",
                HIDDEN_SERVICE_DIR_ROOT, name
            )
            .as_bytes(),
        )
        .await?;
        f.write_all(format!("{}\n", service.hidden_service_version).as_bytes())
            .await?;
        for port in &service.ports {
            f.write_all(
                format!(
                    "HiddenServicePort {} {}:{}\n",
                    port.tor, service.ip, port.internal
                )
                .as_bytes(),
            )
            .await?;
        }
        f.write_all(b"\n").await?;
    }
    write_lan_services(hidden_services).await?; // I know this doesn't belong here, should be abstracted along with `write_services`.
                                                // This whole module should be refactored as its no longer just managing tor.
    Ok(())
}

pub async fn write_lan_services(hidden_services: &ServicesMap) -> Result<(), Error> {
    let hostname = tokio::fs::read_to_string(ETC_HOSTNAME).await?;
    let mut f = tokio::fs::File::create(ETC_NGINX_SERVICES_CONF).await?;
    for (name, service) in &hidden_services.map {
        if service
            .ports
            .iter()
            .filter(|p| p.internal == 80)
            .next()
            .is_none()
        {
            continue;
        }
        f.write_all(
            format!(
                include_str!("nginx.conf.template"),
                hostname = hostname.trim(),
                app_id = name,
                app_ip = service.ip,
            )
            .as_bytes(),
        )
        .await?;
    }

    Ok(())
}

pub async fn read_tor_address(name: &str, timeout: Option<Duration>) -> Result<String, Error> {
    log::info!("Retrieving Tor hidden service address for {}.", name);
    let addr_path = Path::new(HIDDEN_SERVICE_DIR_ROOT)
        .join(format!("app-{}", name))
        .join("hostname");
    if let Some(timeout) = timeout {
        let start = Instant::now();
        while {
            if addr_path.exists() {
                false
            } else {
                if start.elapsed() >= timeout {
                    log::warn!("Timed out waiting for tor to start.");
                    false
                } else {
                    true
                }
            }
        } {
            tokio::time::sleep(Duration::from_millis(100)).await;
        }
    }
    let tor_addr = match tokio::fs::read_to_string(&addr_path).await {
        Err(e) if e.kind() == std::io::ErrorKind::NotFound => Err(e)
            .with_context(|e| format!("{}: {}", addr_path.display(), e))
            .with_code(crate::error::NOT_FOUND),
        a => a
            .with_context(|e| format!("{}: {}", addr_path.display(), e))
            .with_code(crate::error::FILESYSTEM_ERROR),
    }?;
    Ok(tor_addr.trim().to_owned())
}

pub async fn read_tor_key(
    name: &str,
    version: HiddenServiceVersion,
    timeout: Option<Duration>,
) -> Result<String, Error> {
    log::info!("Retrieving Tor hidden service key for {}.", name);
    let addr_path = Path::new(HIDDEN_SERVICE_DIR_ROOT)
        .join(format!("app-{}", name))
        .join(match version {
            HiddenServiceVersion::V3 => "hs_ed25519_secret_key",
            _ => "private_key",
        });
    if let Some(timeout) = timeout {
        let start = Instant::now();
        while {
            if addr_path.exists() {
                false
            } else {
                if start.elapsed() >= timeout {
                    log::warn!("Timed out waiting for tor to start.");
                    false
                } else {
                    true
                }
            }
        } {
            tokio::time::sleep(Duration::from_millis(100)).await;
        }
    }
    let tor_key = match version {
        HiddenServiceVersion::V3 => {
            let mut f = tokio::fs::File::open(&addr_path)
                .await
                .with_context(|e| format!("{}: {}", e, addr_path.display()))
                .with_code(crate::error::FILESYSTEM_ERROR)?;
            let mut buf = [0; 96];
            f.read_exact(&mut buf).await?;
            base32::encode(base32::Alphabet::RFC4648 { padding: false }, &buf[32..]).to_lowercase()
        }
        _ => tokio::fs::read_to_string(&addr_path)
            .await
            .with_context(|e| format!("{}: {}", e, addr_path.display()))
            .with_code(crate::error::FILESYSTEM_ERROR)?
            .trim_end_matches("\u{0}")
            .to_string(),
    };
    Ok(tor_key.trim().to_owned())
}

pub async fn set_svc(
    name: &str,
    service: NewService,
) -> Result<(Ipv4Addr, Option<String>, Option<String>), Error> {
    log::info!(
        "Adding Tor hidden service {} to {}.",
        name,
        crate::SERVICES_YAML
    );
    let is_listening = !service.ports.is_empty();
    let path = PersistencePath::from_ref(crate::SERVICES_YAML);
    let mut hidden_services = services_map_mut(path).await?;
    let ver = service.hidden_service_version;
    let ip = hidden_services.add(name.to_owned(), service);
    log::info!("Adding Tor hidden service {} to {}.", name, ETC_TOR_RC);
    write_services(&hidden_services).await?;
    let addr_path = Path::new(HIDDEN_SERVICE_DIR_ROOT)
        .join(format!("app-{}", name))
        .join("hostname");
    tokio::fs::remove_file(addr_path).await.or_else(|e| {
        if e.kind() == std::io::ErrorKind::NotFound {
            Ok(())
        } else {
            Err(e)
        }
    })?;
    nix::unistd::sync();
    hidden_services.commit().await?;
    log::info!("Reloading Tor.");
    let svc_exit = std::process::Command::new("service")
        .args(&["tor", "reload"])
        .status()?;
    crate::ensure_code!(
        svc_exit.success(),
        crate::error::GENERAL_ERROR,
        "Failed to Reload Tor: {}",
        svc_exit
            .code()
            .or_else(|| { svc_exit.signal().map(|a| 128 + a) })
            .unwrap_or(0)
    );
    log::info!("Reloading Nginx.");
    let svc_exit = std::process::Command::new("service")
        .args(&["nginx", "reload"])
        .status()?;
    crate::ensure_code!(
        svc_exit.success(),
        crate::error::GENERAL_ERROR,
        "Failed to Reload Nginx: {}",
        svc_exit
            .code()
            .or_else(|| { svc_exit.signal().map(|a| 128 + a) })
            .unwrap_or(0)
    );
    Ok((
        ip,
        if is_listening {
            Some(read_tor_address(name, Some(Duration::from_secs(30))).await?)
        } else {
            None
        },
        if is_listening {
            Some(read_tor_key(name, ver, Some(Duration::from_secs(30))).await?)
        } else {
            None
        },
    ))
}

pub async fn rm_svc(name: &str) -> Result<(), Error> {
    log::info!(
        "Removing Tor hidden service {} from {}.",
        name,
        crate::SERVICES_YAML
    );
    let path = PersistencePath::from_ref(crate::SERVICES_YAML);
    let mut hidden_services = services_map_mut(path).await?;
    hidden_services.remove(name);
    let hidden_service_path = Path::new(HIDDEN_SERVICE_DIR_ROOT).join(format!("app-{}", name));
    log::info!("Removing {}", hidden_service_path.display());
    if hidden_service_path.exists() {
        tokio::fs::remove_dir_all(hidden_service_path).await?;
    }
    log::info!("Removing Tor hidden service {} from {}.", name, ETC_TOR_RC);
    write_services(&hidden_services).await?;
    hidden_services.commit().await?;
    log::info!("Reloading Tor.");
    let svc_exit = std::process::Command::new("service")
        .args(&["tor", "reload"])
        .status()?;
    crate::ensure_code!(
        svc_exit.success(),
        crate::error::GENERAL_ERROR,
        "Failed to Reload Tor: {}",
        svc_exit.code().unwrap_or(0)
    );
    log::info!("Reloading Nginx.");
    let svc_exit = std::process::Command::new("service")
        .args(&["nginx", "reload"])
        .status()?;
    crate::ensure_code!(
        svc_exit.success(),
        crate::error::GENERAL_ERROR,
        "Failed to Reload Nginx: {}",
        svc_exit
            .code()
            .or_else(|| { svc_exit.signal().map(|a| 128 + a) })
            .unwrap_or(0)
    );
    Ok(())
}

pub async fn change_key(
    name: &str,
    key: Option<&ed25519_dalek::ExpandedSecretKey>,
) -> Result<(), Error> {
    let hidden_service_path = Path::new(HIDDEN_SERVICE_DIR_ROOT).join(format!("app-{}", name));
    log::info!("Removing {}", hidden_service_path.display());
    if hidden_service_path.exists() {
        tokio::fs::remove_dir_all(&hidden_service_path)
            .await
            .with_context(|e| format!("{}: {}", hidden_service_path.display(), e))
            .with_code(crate::error::FILESYSTEM_ERROR)?;
    }
    if let Some(key) = key {
        tokio::fs::create_dir_all(&hidden_service_path).await?;
        let key_path = hidden_service_path.join("hs_ed25519_secret_key");
        let mut key_data = b"== ed25519v1-secret: type0 ==".to_vec();
        key_data.extend_from_slice(&key.to_bytes());
        tokio::fs::write(&key_path, key_data)
            .await
            .with_context(|e| format!("{}: {}", key_path.display(), e))
            .with_code(crate::error::FILESYSTEM_ERROR)?;
    }
    log::info!("Reloading Tor.");
    let svc_exit = std::process::Command::new("service")
        .args(&["tor", "reload"])
        .status()?;
    crate::ensure_code!(
        svc_exit.success(),
        crate::error::GENERAL_ERROR,
        "Failed to Reload Tor: {}",
        svc_exit.code().unwrap_or(0)
    );
    let mut info = crate::apps::list_info_mut().await?;
    if let Some(mut i) = info.get_mut(name) {
        if i.tor_address.is_some() {
            i.tor_address = Some(read_tor_address(name, Some(Duration::from_secs(30))).await?);
        }
    }
    Ok(())
}

pub async fn reload() -> Result<(), Error> {
    let path = PersistencePath::from_ref(crate::SERVICES_YAML);
    let hidden_services = services_map(&path).await?;
    log::info!("Syncing Tor hidden services to {}.", ETC_TOR_RC);
    write_services(&hidden_services).await?;
    log::info!("Reloading Tor.");
    let svc_exit = std::process::Command::new("service")
        .args(&["tor", "reload"])
        .status()?;
    crate::ensure_code!(
        svc_exit.success(),
        crate::error::GENERAL_ERROR,
        "Failed to Reload Tor: {}",
        svc_exit.code().unwrap_or(0)
    );
    Ok(())
}

pub async fn restart() -> Result<(), Error> {
    let path = PersistencePath::from_ref(crate::SERVICES_YAML);
    let hidden_services = services_map(&path).await?;
    log::info!("Syncing Tor hidden services to {}.", ETC_TOR_RC);
    write_services(&hidden_services).await?;
    log::info!("Restarting Tor.");
    let svc_exit = std::process::Command::new("service")
        .args(&["tor", "restart"])
        .status()?;
    crate::ensure_code!(
        svc_exit.success(),
        crate::error::GENERAL_ERROR,
        "Failed to Restart Tor: {}",
        svc_exit.code().unwrap_or(0)
    );
    Ok(())
}
