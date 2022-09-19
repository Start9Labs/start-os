use color_eyre::eyre::eyre;
use std::collections::BTreeMap;
use std::net::{Ipv4Addr, SocketAddr};

use models::{InterfaceId, PackageId};
use tokio::sync::Mutex;
use tracing::{info, instrument};

use crate::net::embassy_http_server::EmbassyHTTPServer;
use crate::net::ssl::SslManager;
use crate::net::{InterfaceMetadata, PackageNetInfo};

use crate::{Error, ResultExt};

pub struct ProxyController {
    inner: Mutex<ProxyControllerInner>,
}

impl ProxyController {
    pub async fn init(embassyd_addr: SocketAddr, ssl_manager: &SslManager) -> Result<Self, Error> {
        Ok(ProxyController {
            inner: Mutex::new(ProxyControllerInner::init(embassyd_addr, ssl_manager).await?),
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
        self.inner.lock().await.remove(package).await
    }
}

struct ProxyControllerInner {
    embassyd_addr: SocketAddr,
    service_servers: BTreeMap<u16, EmbassyHTTPServer>,
    iface_lookups: BTreeMap<(PackageId, InterfaceId), String>,
    interfaces: BTreeMap<PackageId, PackageNetInfo>,
}

impl ProxyControllerInner {
    #[instrument]
    async fn init(embassyd_addr: SocketAddr, ssl_manager: &SslManager) -> Result<Self, Error> {
        let inner = ProxyControllerInner {
            embassyd_addr,
            interfaces: BTreeMap::new(),
            iface_lookups: BTreeMap::new(),
            service_servers: BTreeMap::new(),
        };

        // let emnbassyd_port_80_svc = EmbassyHTTPServer::new(embassyd_addr).await?;
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
                        && !meta.lan_config.is_empty()
            })
            .collect::<BTreeMap<InterfaceId, InterfaceMetadata>>();

        for (id, meta) in interface_map.iter() {
            for (external_svc_port, lan_port_config) in meta.lan_config.iter() {
                self.iface_lookups
                    .insert((package.clone(), id.clone()), meta.fqdn.clone());

                let docker_addr = SocketAddr::from((ipv4, lan_port_config.internal));
                info!("docker ip: {}", docker_addr);

                if let Some(server) = self.service_servers.get_mut(&external_svc_port.0) {
                    server.add_docker_mapping(meta.fqdn.to_owned(), docker_addr).await;
                } else {
                    let mut new_service_server =
                        EmbassyHTTPServer::new(self.embassyd_addr.ip(), external_svc_port.0)
                            .await?;
                    new_service_server
                        .add_docker_mapping(meta.fqdn.to_owned(), docker_addr)
                        .await;

                    self.service_servers
                        .insert(external_svc_port.0, new_service_server);
                }
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

    #[instrument(skip(self))]
    async fn remove(&mut self, package: &PackageId) -> Result<(), Error> {
        let mut server_removal = false;
        let mut server_removal_port: u16 = 0;
        let mut removed_interface_id = InterfaceId::default();

        let package_interface_info = self.interfaces.get(package);
        if let Some(net_info) = package_interface_info {
            for (id, meta) in &net_info.interfaces {
                for (service_ext_port, _lan_port_config) in meta.lan_config.iter() {
                    if let Some(server) = self.service_servers.get_mut(&service_ext_port.0) {
                        if let Some(dns_base) =
                            self.iface_lookups.get(&(package.clone(), id.clone()))
                        {
                            server.remove_docker_mapping(dns_base.to_string()).await;

                            if server.docker_mapping.read().await.is_empty() {
                                server_removal = true;
                                server_removal_port = service_ext_port.0;
                                removed_interface_id = id.to_owned();
                                break;
                            }
                        }
                    }
                }
            }
        }

        if server_removal {
            if let Some(removed_server) = self.service_servers.remove(&server_removal_port) {
                removed_server.shutdown.send(()).map_err(|_| {
                    Error::new(
                        eyre!("Hyper server did not quit properly"),
                        crate::ErrorKind::JoinError,
                    )
                })?;
                removed_server
                    .handle
                    .await
                    .with_kind(crate::ErrorKind::JoinError)?;
                self.interfaces.remove(&package.clone());
                self.iface_lookups
                    .remove(&(package.clone(), removed_interface_id));
            }
        }
        Ok(())
    }
}
