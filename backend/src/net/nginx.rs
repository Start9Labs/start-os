use std::collections::BTreeMap;
use std::net::Ipv4Addr;
use std::path::{Path, PathBuf};

use futures::FutureExt;
use indexmap::IndexSet;
use tokio::sync::Mutex;
use tracing::instrument;

use super::interface::{InterfaceId, LanPortConfig};
use super::ssl::SslManager;
use crate::hostname::Hostname;
use crate::s9pk::manifest::PackageId;
use crate::util::serde::Port;
use crate::util::Invoke;
use crate::{Error, ErrorKind, ResultExt};

pub struct NginxController {
    pub nginx_root: PathBuf,
    inner: Mutex<NginxControllerInner>,
}
impl NginxController {
    pub async fn init(
        nginx_root: PathBuf,
        ssl_manager: &SslManager,
        host_name: &Hostname,
    ) -> Result<Self, Error> {
        Ok(NginxController {
            inner: Mutex::new(
                NginxControllerInner::init(&nginx_root, ssl_manager, host_name).await?,
            ),
            nginx_root,
        })
    }
    pub async fn add<I: IntoIterator<Item = (InterfaceId, InterfaceMetadata)>>(
        &self,
        ssl_manager: &SslManager,
        package: PackageId,
        ipv4: Ipv4Addr,
        interfaces: I,
    ) -> Result<(), Error> {
        self.inner
            .lock()
            .await
            .add(&self.nginx_root, ssl_manager, package, ipv4, interfaces)
            .await
    }
    pub async fn remove(&self, package: &PackageId) -> Result<(), Error> {
        self.inner
            .lock()
            .await
            .remove(&self.nginx_root, package)
            .await
    }
}

pub struct NginxControllerInner {
    interfaces: BTreeMap<PackageId, PackageNetInfo>,
}
impl NginxControllerInner {
    #[instrument]
    async fn init(
        nginx_root: &Path,
        ssl_manager: &SslManager,
        host_name: &Hostname,
    ) -> Result<Self, Error> {
        let inner = NginxControllerInner {
            interfaces: BTreeMap::new(),
        };
        // write main ssl key/cert to fs location
        let (key, cert) = ssl_manager
            .certificate_for(host_name.as_ref(), &"embassy".parse().unwrap())
            .await?;
        let ssl_path_key = nginx_root.join(format!("ssl/embassy_main.key.pem"));
        let ssl_path_cert = nginx_root.join(format!("ssl/embassy_main.cert.pem"));
        tokio::try_join!(
            crate::net::ssl::export_key(&key, &ssl_path_key),
            crate::net::ssl::export_cert(&cert, &ssl_path_cert),
        )?;
        Ok(inner)
    }
    #[instrument(skip(self, interfaces))]
    async fn add<I: IntoIterator<Item = (InterfaceId, InterfaceMetadata)>>(
        &mut self,
        nginx_root: &Path,
        ssl_manager: &SslManager,
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
                let (
                    listen_args,
                    ssl_certificate_line,
                    ssl_certificate_key_line,
                    proxy_redirect_directive,
                ) = if lan_port_config.ssl {
                    // these have already been written by the net controller
                    let package_path = nginx_root.join(format!("ssl/{}", package));
                    if tokio::fs::metadata(&package_path).await.is_err() {
                        tokio::fs::create_dir_all(&package_path)
                            .await
                            .with_ctx(|_| {
                                (ErrorKind::Filesystem, package_path.display().to_string())
                            })?;
                    }
                    let ssl_path_key = package_path.join(format!("{}.key.pem", id));
                    let ssl_path_cert = package_path.join(format!("{}.cert.pem", id));
                    let (key, chain) = ssl_manager
                        .certificate_for(&meta.dns_base, &package)
                        .await?;
                    tokio::try_join!(
                        crate::net::ssl::export_key(&key, &ssl_path_key),
                        crate::net::ssl::export_cert(&chain, &ssl_path_cert)
                    )?;
                    (
                        format!("{} ssl", port.0),
                        format!("ssl_certificate {};", ssl_path_cert.to_str().unwrap()),
                        format!("ssl_certificate_key {};", ssl_path_key.to_str().unwrap()),
                        format!("proxy_redirect http://$host/ https://$host/;"),
                    )
                } else {
                    (
                        format!("{}", port.0),
                        String::from(""),
                        String::from(""),
                        String::from(""),
                    )
                };
                // write nginx configs
                let nginx_conf_path = nginx_root.join(format!(
                    "sites-available/{}_{}_{}.conf",
                    package, id, port.0
                ));
                tokio::fs::write(
                    &nginx_conf_path,
                    format!(
                        include_str!("nginx.conf.template"),
                        listen_args = listen_args,
                        listen_args_ipv6 = listen_args,
                        hostname = meta.dns_base,
                        ssl_certificate_line = ssl_certificate_line,
                        ssl_certificate_key_line = ssl_certificate_key_line,
                        app_ip = ipv4,
                        internal_port = lan_port_config.internal,
                        proxy_redirect_directive = proxy_redirect_directive,
                    ),
                )
                .await
                .with_ctx(|_| (ErrorKind::Filesystem, nginx_conf_path.display().to_string()))?;
                let sites_enabled_link_path =
                    nginx_root.join(format!("sites-enabled/{}_{}_{}.conf", package, id, port.0));
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

    #[instrument(skip(self))]
    async fn remove(&mut self, nginx_root: &Path, package: &PackageId) -> Result<(), Error> {
        let removed = self.interfaces.remove(package);
        if let Some(net_info) = removed {
            for (id, meta) in net_info.interfaces {
                for (port, _lan_port_config) in meta.lan_config.iter() {
                    // remove ssl certificates and nginx configs
                    let package_path = nginx_root.join(format!("ssl/{}", package));
                    let enabled_path = nginx_root
                        .join(format!("sites-enabled/{}_{}_{}.conf", package, id, port.0));
                    let available_path = nginx_root.join(format!(
                        "sites-available/{}_{}_{}.conf",
                        package, id, port.0
                    ));
                    let _ = tokio::try_join!(
                        async {
                            if tokio::fs::metadata(&package_path).await.is_ok() {
                                tokio::fs::remove_dir_all(&package_path)
                                    .map(|res| {
                                        res.with_ctx(|_| {
                                            (
                                                ErrorKind::Filesystem,
                                                package_path.display().to_string(),
                                            )
                                        })
                                    })
                                    .await?;
                                Ok(())
                            } else {
                                Ok(())
                            }
                        },
                        tokio::fs::remove_file(&enabled_path).map(|res| res.with_ctx(|_| (
                            ErrorKind::Filesystem,
                            enabled_path.display().to_string()
                        ))),
                        tokio::fs::remove_file(&available_path).map(|res| res.with_ctx(|_| (
                            ErrorKind::Filesystem,
                            available_path.display().to_string()
                        ))),
                    )?;
                }
            }
        }
        self.hup().await?;
        Ok(())
    }

    #[instrument(skip(self))]
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
    interfaces: BTreeMap<InterfaceId, InterfaceMetadata>,
}
pub struct InterfaceMetadata {
    pub dns_base: String,
    pub lan_config: BTreeMap<Port, LanPortConfig>,
    pub protocols: IndexSet<String>,
}
