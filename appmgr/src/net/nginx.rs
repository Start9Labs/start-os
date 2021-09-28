use std::collections::BTreeMap;
use std::net::Ipv4Addr;
use std::path::PathBuf;

use futures::FutureExt;
use indexmap::IndexSet;
use sqlx::SqlitePool;
use tokio::sync::Mutex;

use super::interface::{InterfaceId, LanPortConfig};
use super::ssl::SslManager;
use crate::s9pk::manifest::PackageId;
use crate::util::{Invoke, Port};
use crate::{Error, ErrorKind, ResultExt};

pub struct NginxController(Mutex<NginxControllerInner>);
impl NginxController {
    pub async fn init(nginx_root: PathBuf, db: SqlitePool) -> Result<Self, Error> {
        Ok(NginxController(Mutex::new(
            NginxControllerInner::init(nginx_root, db).await?,
        )))
    }
    pub async fn add<I: IntoIterator<Item = (InterfaceId, InterfaceMetadata)>>(
        &self,
        package: PackageId,
        ipv4: Ipv4Addr,
        interfaces: I,
    ) -> Result<(), Error> {
        self.0.lock().await.add(package, ipv4, interfaces).await
    }
    pub async fn remove(&self, package: &PackageId) -> Result<(), Error> {
        self.0.lock().await.remove(package).await
    }
}

pub struct NginxControllerInner {
    nginx_root: PathBuf,
    interfaces: BTreeMap<PackageId, PackageNetInfo>,
    ssl_manager: SslManager,
}
impl NginxControllerInner {
    async fn init(nginx_root: PathBuf, db: SqlitePool) -> Result<Self, Error> {
        Ok(NginxControllerInner {
            nginx_root,
            interfaces: BTreeMap::new(),
            ssl_manager: SslManager::init(db).await?,
        })
    }
    async fn add<I: IntoIterator<Item = (InterfaceId, InterfaceMetadata)>>(
        &mut self,
        package: PackageId,
        ipv4: Ipv4Addr,
        interfaces: I,
    ) -> Result<(), Error> {
        let interface_map = interfaces
            .into_iter()
            .filter(|(_, meta)| {
                // don't add nginx stuff for anything we can't connect to over some flavor of http
                (meta.protocols.contains("http") || meta.protocols.contains("https"))
                // also don't add nginx unless it has at least one exposed port
                        && meta.lan_config.len() > 0
            })
            .collect::<BTreeMap<InterfaceId, InterfaceMetadata>>();

        for (id, meta) in interface_map.iter() {
            for (port, lan_port_config) in meta.lan_config.iter() {
                // get ssl certificate chain
                let (listen_args, ssl_certificate_line, ssl_certificate_key_line) =
                    if lan_port_config.ssl {
                        let ssl_path_key = self
                            .nginx_root
                            .join(format!("ssl/{}_{}.key.pem", package, id));
                        let ssl_path_cert = self
                            .nginx_root
                            .join(format!("ssl/{}_{}.cert.pem", package, id));
                        let (key, chain) = self.ssl_manager.certificate_for(&meta.dns_base).await?;
                        // write nginx ssl certs
                        futures::try_join!(
                            tokio::fs::write(&ssl_path_key, key.private_key_to_pem_pkcs8()?).map(
                                |res| res.with_ctx(|_| (
                                    ErrorKind::Filesystem,
                                    ssl_path_key.display().to_string()
                                ))
                            ),
                            tokio::fs::write(
                                &ssl_path_cert,
                                chain
                                    .into_iter()
                                    .flat_map(|c| c.to_pem().unwrap())
                                    .collect::<Vec<u8>>()
                            )
                            .map(|res| res.with_ctx(|_| (
                                ErrorKind::Filesystem,
                                ssl_path_cert.display().to_string()
                            ))),
                        )?;

                        (
                            format!("{} ssl", lan_port_config.mapping),
                            format!("ssl_certificate {};", ssl_path_cert.to_str().unwrap()),
                            format!("ssl_certificate_key {};", ssl_path_key.to_str().unwrap()),
                        )
                    } else {
                        (
                            format!("{}", lan_port_config.mapping),
                            String::from(""),
                            String::from(""),
                        )
                    };
                // write nginx configs
                let nginx_conf_path = self
                    .nginx_root
                    .join(format!("sites-available/{}_{}.conf", package, id));
                tokio::fs::write(
                    &nginx_conf_path,
                    format!(
                        include_str!("nginx.conf.template"),
                        listen_args = listen_args,
                        hostname = meta.dns_base,
                        ssl_certificate_line = ssl_certificate_line,
                        ssl_certificate_key_line = ssl_certificate_key_line,
                        app_ip = ipv4,
                        internal_port = port.0,
                    ),
                )
                .await
                .with_ctx(|_| (ErrorKind::Filesystem, nginx_conf_path.display().to_string()))?;
                let sites_enabled_link_path = self
                    .nginx_root
                    .join(format!("sites-enabled/{}_{}.conf", package, id));
                if tokio::fs::metadata(&sites_enabled_link_path).await.is_ok() {
                    tokio::fs::remove_file(&sites_enabled_link_path).await?;
                }
                tokio::fs::symlink(&nginx_conf_path, &sites_enabled_link_path)
                    .await
                    .with_ctx(|_| (ErrorKind::Filesystem, nginx_conf_path.display().to_string()))?;
            }
        }
        match self.interfaces.get_mut(&package) {
            None => {
                let info = PackageNetInfo {
                    ip: ipv4,
                    interfaces: interface_map,
                };
                self.interfaces.insert(package, info);
            }
            Some(p) => {
                p.interfaces.extend(interface_map);
            }
        };

        self.hup().await?;
        Ok(())
    }
    async fn remove(&mut self, package: &PackageId) -> Result<(), Error> {
        let removed = self.interfaces.remove(package);
        if let Some(net_info) = removed {
            for (id, _meta) in net_info.interfaces {
                // remove ssl certificates and nginx configs
                let key_path = self
                    .nginx_root
                    .join(format!("ssl/{}_{}.key.pem", package, id));
                let cert_path = self
                    .nginx_root
                    .join(format!("ssl/{}_{}.cert.pem", package, id));
                let enabled_path = self
                    .nginx_root
                    .join(format!("sites-enabled/{}_{}.conf", package, id));
                let available_path = self
                    .nginx_root
                    .join(format!("sites-available/{}_{}.conf", package, id));
                let _ = futures::try_join!(
                    tokio::fs::remove_file(&key_path).map(|res| res
                        .with_ctx(|_| (ErrorKind::Filesystem, key_path.display().to_string()))),
                    tokio::fs::remove_file(&cert_path).map(|res| res
                        .with_ctx(|_| (ErrorKind::Filesystem, key_path.display().to_string()))),
                    tokio::fs::remove_file(&enabled_path).map(|res| res
                        .with_ctx(|_| (ErrorKind::Filesystem, enabled_path.display().to_string()))),
                    tokio::fs::remove_file(&available_path).map(|res| res.with_ctx(|_| (
                        ErrorKind::Filesystem,
                        available_path.display().to_string()
                    ))),
                )?;
            }
        }
        self.hup().await?;
        Ok(())
    }
    async fn hup(&self) -> Result<(), Error> {
        let _ = tokio::process::Command::new("systemctl")
            .arg("reload")
            .arg("nginx")
            .invoke(ErrorKind::Nginx)
            .await?;
        Ok(())
    }
}
struct PackageNetInfo {
    ip: Ipv4Addr,
    interfaces: BTreeMap<InterfaceId, InterfaceMetadata>,
}
pub struct InterfaceMetadata {
    pub dns_base: String,
    pub lan_config: BTreeMap<Port, LanPortConfig>,
    pub protocols: IndexSet<String>,
}
