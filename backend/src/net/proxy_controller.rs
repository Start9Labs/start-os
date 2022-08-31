use std::collections::BTreeMap;
use std::net::{Ipv4Addr, SocketAddr};
use std::path::{Path, PathBuf};
use std::str::FromStr;

use models::{InterfaceId, PackageId};
use tokio::sync::Mutex;
use tracing::instrument;

use crate::net::ssl::SslManager;
use crate::net::{InterfaceMetadata, PackageNetInfo};
use crate::{Error, ResultExt};

pub struct ProxyController {
    inner: Mutex<ProxyControllerInner>,
}

impl ProxyController {
    pub async fn init(ssl_manager: &SslManager) -> Result<Self, Error> {
        Ok(ProxyController {
            inner: Mutex::new(ProxyControllerInner::init(ssl_manager).await?),
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
            .add(ssl_manager, package, ipv4, interfaces)
            .await
    }

    pub async fn remove(&self, package: &PackageId) -> Result<(), Error> {
        self.inner
            .lock()
            .await
            .remove(&self.proxy_root, package)
            .await
    }
}

struct ProxyControllerInner {
    interfaces: BTreeMap<PackageId, PackageNetInfo>,
}

impl ProxyControllerInner {
    #[instrument]
    async fn init(ssl_manager: &SslManager) -> Result<Self, Error> {
        let inner = ProxyControllerInner {
            interfaces: BTreeMap::new(),
        };
        // write main ssl key/cert to fs location
        // let (key, cert) = ssl_manager
        //     .certificate_for(&get_hostname().await?, &"embassy".parse().unwrap())
        //     .await?;
        // let ssl_path_key = nginx_root.join(format!("ssl/embassy_main.key.pem"));
        // let ssl_path_cert = nginx_root.join(format!("ssl/embassy_main.cert.pem"));
        // tokio::try_join!(
        //     crate::net::ssl::export_key(&key, &ssl_path_key),
        //     crate::net::ssl::export_cert(&cert, &ssl_path_cert),
        // )?;
        Ok(inner)
    }

    #[instrument(skip(self, interfaces))]
    async fn add<I: IntoIterator<Item = (InterfaceId, InterfaceMetadata)>>(
        &mut self,
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
                let addr = SocketAddr::from_str(&format!("{}:{}", meta.dns_base, port.0))
                    .with_kind(crate::ErrorKind::InvalidIP)?;


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
     Ok(())
    }
}
