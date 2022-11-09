use std::collections::BTreeMap;
use std::net::{Ipv4Addr, SocketAddr};
use std::str::FromStr;
use std::sync::Arc;

use color_eyre::eyre::eyre;
use futures::FutureExt;
use http::{Method, Request, Response};
use hyper::upgrade::Upgraded;
use hyper::{Body, Error as HyperError};
use models::{InterfaceId, PackageId};
use openssl::pkey::{PKey, Private};
use openssl::x509::X509;
use tokio::net::TcpStream;
use tokio::sync::Mutex;
use tracing::{error, info, instrument};

use crate::net::net_utils::{host_addr_fqdn, ResourceFqdn};
use crate::net::ssl::SslManager;
use crate::net::vhost_controller::VHOSTController;
use crate::net::{HttpClient, HttpHandler, InterfaceMetadata, PackageNetInfo};
use crate::{Error, ResultExt};

pub struct ProxyController {
    inner: Mutex<ProxyControllerInner>,
}

impl ProxyController {
    pub async fn init(
        embassyd_socket_addr: SocketAddr,
        embassy_fqdn: ResourceFqdn,
        ssl_manager: SslManager,
    ) -> Result<Self, Error> {
        Ok(ProxyController {
            inner: Mutex::new(
                ProxyControllerInner::init(embassyd_socket_addr, embassy_fqdn, ssl_manager).await?,
            ),
        })
    }

    pub async fn add_docker_service<I: IntoIterator<Item = (InterfaceId, InterfaceMetadata)>>(
        &self,
        package: PackageId,
        ipv4: Ipv4Addr,
        interfaces: I,
    ) -> Result<(), Error> {
        self.inner
            .lock()
            .await
            .add_docker_service(package, ipv4, interfaces)
            .await
    }

    pub async fn remove_docker_service(&self, package: &PackageId) -> Result<(), Error> {
        self.inner.lock().await.remove_docker_service(package).await
    }

    pub async fn add_certificate_to_resolver(
        &self,
        fqdn: ResourceFqdn,
        cert_data: (PKey<Private>, Vec<X509>),
    ) -> Result<(), Error> {
        self.inner
            .lock()
            .await
            .add_certificate_to_resolver(fqdn, cert_data)
            .await
    }

    pub async fn add_handle(
        &self,
        ext_port: u16,
        fqdn: ResourceFqdn,
        handler: HttpHandler,
        is_ssl: bool,
    ) -> Result<(), Error> {
        self.inner
            .lock()
            .await
            .add_handle(ext_port, fqdn, handler, is_ssl)
            .await
    }

    pub async fn get_hostname(&self) -> String {
        self.inner.lock().await.get_embassy_hostname()
    }

    pub async fn proxy(
        client: HttpClient,
        req: Request<Body>,
    ) -> Result<Response<Body>, HyperError> {
        if Method::CONNECT == req.method() {
            // Received an HTTP request like:
            // ```
            // CONNECT www.domain.com:443 HTTP/1.1s
            // Host: www.domain.com:443
            // Proxy-Connection: Keep-Alive
            // ```
            //
            // When HTTP method is CONNECT we should return an empty body
            // then we can eventually upgrade the connection and talk a new protocol.
            //
            // Note: only after client received an empty body with STATUS_OK can the
            // connection be upgraded, so we can't return a response inside
            // `on_upgrade` future.
            match host_addr_fqdn(&req) {
                Ok(host) => {
                    tokio::task::spawn(async move {
                        match hyper::upgrade::on(req).await {
                            Ok(upgraded) => match host {
                                ResourceFqdn::IpAddr(ip) => {
                                    if let Err(e) = Self::tunnel(upgraded, ip.to_string()).await {
                                        error!("server io error: {}", e);
                                    };
                                }
                                ResourceFqdn::Uri {
                                    full_uri,
                                    root: _,
                                    tld: _,
                                } => {
                                    if let Err(e) =
                                        Self::tunnel(upgraded, full_uri.to_string()).await
                                    {
                                        error!("server io error: {}", e);
                                    };
                                }
                            },
                            Err(e) => error!("upgrade error: {}", e),
                        }
                    });

                    Ok(Response::new(Body::empty()))
                }
                Err(e) => {
                    let err_txt = format!("CONNECT host is not socket addr: {:?}", &req.uri());
                    let mut resp = Response::new(Body::from(format!(
                        "CONNECT must be to a socket address: {}: {}",
                        err_txt, e
                    )));
                    *resp.status_mut() = http::StatusCode::BAD_REQUEST;

                    Ok(resp)
                }
            }
        } else {
            client.request(req).await
        }
    }

    // Create a TCP connection to host:port, build a tunnel between the connection and
    // the upgraded connection
    async fn tunnel(mut upgraded: Upgraded, addr: String) -> std::io::Result<()> {
        let mut server = TcpStream::connect(addr).await?;

        let (from_client, from_server) =
            tokio::io::copy_bidirectional(&mut upgraded, &mut server).await?;

        info!(
            "client wrote {} bytes and received {} bytes",
            from_client, from_server
        );

        Ok(())
    }
}
struct ProxyControllerInner {
    ssl_manager: SslManager,
    vhosts: VHOSTController,
    embassyd_fqdn: ResourceFqdn,
    docker_interfaces: BTreeMap<PackageId, PackageNetInfo>,
    docker_iface_lookups: BTreeMap<(PackageId, InterfaceId), ResourceFqdn>,
}

impl ProxyControllerInner {
    #[instrument]
    async fn init(
        embassyd_socket_addr: SocketAddr,
        embassyd_fqdn: ResourceFqdn,
        ssl_manager: SslManager,
    ) -> Result<Self, Error> {
        let inner = ProxyControllerInner {
            vhosts: VHOSTController::init(embassyd_socket_addr),
            ssl_manager,
            embassyd_fqdn,
            docker_interfaces: BTreeMap::new(),
            docker_iface_lookups: BTreeMap::new(),
        };

        Ok(inner)
    }

    async fn add_certificate_to_resolver(
        &mut self,
        hostname: ResourceFqdn,
        cert_data: (PKey<Private>, Vec<X509>),
    ) -> Result<(), Error> {
        self.vhosts
            .cert_resolver
            .add_certificate_to_resolver(hostname, cert_data)
            .await
            .map_err(|err| {
                Error::new(
                    eyre!("Unable to add ssl cert to the resolver: {}", err),
                    crate::ErrorKind::Network,
                )
            })?;

        Ok(())
    }

    async fn add_package_certificate_to_resolver(
        &mut self,
        resource_fqdn: ResourceFqdn,
        pkg_id: PackageId,
    ) -> Result<(), Error> {
        let package_cert = match resource_fqdn.clone() {
            ResourceFqdn::IpAddr(ip) => {
                self.ssl_manager
                    .certificate_for(&ip.to_string(), &pkg_id)
                    .await?
            }
            ResourceFqdn::Uri {
                full_uri: _,
                root,
                tld: _,
            } => self.ssl_manager.certificate_for(&root, &pkg_id).await?,
        };

        self.vhosts
            .cert_resolver
            .add_certificate_to_resolver(resource_fqdn, package_cert)
            .await
            .map_err(|err| {
                Error::new(
                    eyre!("Unable to add ssl cert to the resolver: {}", err),
                    crate::ErrorKind::Network,
                )
            })?;

        Ok(())
    }

    pub async fn add_handle(
        &mut self,
        external_svc_port: u16,
        fqdn: ResourceFqdn,
        svc_handler: HttpHandler,
        is_ssl: bool,
    ) -> Result<(), Error> {
        self.vhosts
            .add_server_or_handle(external_svc_port, fqdn, svc_handler, is_ssl)
            .await
    }

    #[instrument(skip(self, interfaces))]
    pub async fn add_docker_service<I: IntoIterator<Item = (InterfaceId, InterfaceMetadata)>>(
        &mut self,
        package: PackageId,
        docker_ipv4: Ipv4Addr,
        interfaces: I,
    ) -> Result<(), Error> {
        let mut interface_map = interfaces
            .into_iter()
            .filter(|(_, meta)| {
                // don't add stuff for anything we can't connect to over some flavor of http
                (meta.protocols.contains("http") || meta.protocols.contains("https"))
                // also don't add anything unless it has at least one exposed port
                        && !meta.lan_config.is_empty()
            })
            .collect::<BTreeMap<InterfaceId, InterfaceMetadata>>();

        for (id, meta) in interface_map.iter() {
            for (external_svc_port, lan_port_config) in meta.lan_config.iter() {
                let full_fqdn = ResourceFqdn::from_str(&meta.fqdn).unwrap();

                self.docker_iface_lookups
                    .insert((package.clone(), id.clone()), full_fqdn.clone());

                self.add_package_certificate_to_resolver(full_fqdn.clone(), package.clone())
                    .await?;

                let svc_handler =
                    Self::create_docker_handle(docker_ipv4.to_string(), lan_port_config.internal)
                        .await;

                self.add_handle(
                    external_svc_port.0,
                    full_fqdn.clone(),
                    svc_handler,
                    lan_port_config.ssl,
                )
                .await?;
            }
        }

        let docker_interface = self.docker_interfaces.entry(package.clone()).or_default();
        docker_interface.interfaces.append(&mut interface_map);

        Ok(())
    }

    async fn create_docker_handle(internal_ip: String, port: u16) -> HttpHandler {
        let svc_handler: HttpHandler = Arc::new(move |mut req| {
            let proxy_addr = internal_ip.clone();
            async move {
                let client = HttpClient::new();

                let uri_string = format!(
                    "http://{}:{}{}",
                    proxy_addr,
                    port,
                    req.uri()
                        .path_and_query()
                        .map(|x| x.as_str())
                        .unwrap_or("/")
                );

                let uri = uri_string.parse().unwrap();
                *req.uri_mut() = uri;

                ProxyController::proxy(client, req).await
            }
            .boxed()
        });

        svc_handler
    }

    #[instrument(skip(self))]
    pub async fn remove_docker_service(&mut self, package: &PackageId) -> Result<(), Error> {
        let mut server_removals: Vec<(u16, InterfaceId)> = Default::default();

        let net_info = match self.docker_interfaces.get(package) {
            Some(a) => a,
            None => return Ok(()),
        };

        for (id, meta) in &net_info.interfaces {
            for (service_ext_port, _lan_port_config) in meta.lan_config.iter() {
                if let Some(server) = self.vhosts.service_servers.get_mut(&service_ext_port.0) {
                    if let Some(fqdn) = self
                        .docker_iface_lookups
                        .get(&(package.clone(), id.clone()))
                    {
                        server.remove_svc_handler_mapping(fqdn.to_owned()).await?;
                        self.vhosts
                            .cert_resolver
                            .remove_cert(fqdn.to_owned())
                            .await?;

                        let mapping = server.svc_mapping.read().await;

                        if mapping.is_empty() {
                            server_removals.push((service_ext_port.0, id.to_owned()));
                        }
                    }
                }
            }
        }

        for (port, interface_id) in server_removals {
            if let Some(removed_server) = self.vhosts.service_servers.remove(&port) {
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
                self.docker_interfaces.remove(&package.clone());
                self.docker_iface_lookups
                    .remove(&(package.clone(), interface_id));
            }
        }
        Ok(())
    }

    pub fn get_embassy_hostname(&self) -> String {
        self.embassyd_fqdn.to_string()
    }
}
