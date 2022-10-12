use color_eyre::eyre::eyre;
use http::{Method, Request, Response};
use hyper::upgrade::Upgraded;

use hyper::Body;
use hyper::Error as HyperError;
use openssl::pkey::PKey;
use openssl::pkey::Private;
use openssl::x509::X509;
use tokio::net::TcpStream;
use tokio_rustls::rustls::server::ResolvesServerCertUsingSni;
use tokio_rustls::rustls::sign::any_supported_type;
use tokio_rustls::rustls::sign::CertifiedKey;
use tokio_rustls::rustls::sign::RSASigningKey;
use tokio_rustls::rustls::sign::RsaSigningKey;
use tokio_rustls::rustls::Certificate;
use tokio_rustls::rustls::PrivateKey;
use tokio_rustls::rustls::ResolvesServerCertUsingSNI;
use tokio_rustls::rustls::ServerConfig;

use std::collections::BTreeMap;
use std::net::{Ipv4Addr, SocketAddr};
use std::sync::Arc;

use crate::net::net_utils::host_addr;
use crate::net::ssl::SslManager;
use crate::net::vhost_controller::VHOSTController;
use crate::net::HttpClient;
use crate::net::HttpHandler;
use crate::net::InterfaceMetadata;
use crate::net::PackageNetInfo;
use models::{InterfaceId, PackageId};
use tokio::sync::Mutex;
use tracing::{error, info, instrument};

use crate::{Error, ResultExt};

pub struct ProxyController {
    inner: Mutex<ProxyControllerInner>,
}

impl ProxyController {
    pub async fn init(
        embassyd_addr: SocketAddr,
        embassy_hostname: String,
        ssl_manager: SslManager,
    ) -> Result<Self, Error> {
        Ok(ProxyController {
            inner: Mutex::new(
                ProxyControllerInner::init(embassyd_addr, embassy_hostname, ssl_manager).await?,
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

    pub async fn get_package_id(&self, fqdn: &String) -> Option<PackageId> {
        self.inner.lock().await.get_package_id(fqdn).await
    }

    pub async fn get_package_cert(
        &self,
        fqdn: &String,
        package_id: PackageId,
    ) -> Result<(PKey<Private>, Vec<X509>), Error> {
        self.inner
            .lock()
            .await
            .ssl_manager
            .certificate_for(fqdn, &package_id)
            .await
    }

    pub async fn add_handle(
        &self,
        ext_port: u16,
        fqdn: String,
        handler: HttpHandler,
    ) -> Result<(), Error> {
        self.inner
            .lock()
            .await
            .add_handle(ext_port, fqdn, handler)
            .await
    }

    pub async fn get_hostname(&self) -> String {
        self.inner.lock().await.get_hostname()
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
            match host_addr(&req) {
                Ok(host) => {
                    tokio::task::spawn(async move {
                        match hyper::upgrade::on(req).await {
                            Ok(upgraded) => {
                                if let Err(e) = Self::tunnel(upgraded, host).await {
                                    error!("server io error: {}", e);
                                };
                            }
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

    async fn add_certificate_to_resolver(&self, hostname: String) -> Result<(), Error> {
        self.inner
            .lock()
            .await
            .add_certificate_to_resolver(hostname)
            .await
    }
}
struct ProxyControllerInner {
    embassyd_addr: SocketAddr,
    ssl_manager: SslManager,
    vhosts: VHOSTController,
    embassy_hostname: String,
    docker_interfaces: BTreeMap<PackageId, PackageNetInfo>,
    docker_iface_lookups: BTreeMap<(PackageId, InterfaceId), String>,
}

impl ProxyControllerInner {
    #[instrument]
    async fn init(
        embassyd_addr: SocketAddr,
        embassy_hostname: String,
        ssl_manager: SslManager,
    ) -> Result<Self, Error> {
        let inner = ProxyControllerInner {
            embassyd_addr,
            vhosts: VHOSTController::init(embassyd_addr),
            ssl_manager,
            embassy_hostname,
            docker_interfaces: BTreeMap::new(),
            docker_iface_lookups: BTreeMap::new(),
        };

        // let emnbassyd_port_80_svc = EmbassyHTTPServer::new(embassyd_addr).await?;
        Ok(inner)
    }

    pub async fn get_package_id(&self, fqdn: &String) -> Option<PackageId> {
        self.vhosts.svc_dns_base_package_names.get(fqdn).cloned()
    }

    async fn add_certificate_to_resolver(&mut self, hostname: String) -> Result<(), Error> {
        let pkg_id = match self.get_package_id(&hostname).await {
            Some(pkg_id) => pkg_id,
            None => {
                return Err(Error::new(
                    eyre!("No found cert for {}", hostname.clone()),
                    crate::ErrorKind::Network,
                ))
            }
        };

        let package_cert = self.ssl_manager.certificate_for(&hostname, &pkg_id).await?;

        let cert_chain = package_cert.1;
        let private_keys = package_cert
            .0
            .private_key_to_der()
            .map_err(|err| Error::new(eyre!("err {}", err), crate::ErrorKind::BytesError))?;

        let cert = Certificate(
            cert_chain[0]
                .to_der()
                .map_err(|err| Error::new(eyre!("err: {}", err), crate::ErrorKind::BytesError))?,
        );

        let pre_sign_key = PrivateKey(private_keys);
        let actual_sign_key = any_supported_type(&pre_sign_key)
            .map_err(|err| Error::new(eyre!("{}", err), crate::ErrorKind::SignError))?;

        self.vhosts.cert_resolver
            .add(&hostname, CertifiedKey::new(vec![cert], actual_sign_key))
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
        fqdn: String,
        svc_handler: HttpHandler,
    ) -> Result<(), Error> {
        self.vhosts
            .add_server_or_handle(external_svc_port, fqdn, svc_handler)
            .await
    }

    #[instrument(skip(self, interfaces))]
    pub async fn add_docker_service<I: IntoIterator<Item = (InterfaceId, InterfaceMetadata)>>(
        &mut self,
        package: PackageId,
        docker_ipv4: Ipv4Addr,
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
                self.docker_iface_lookups
                    .insert((package.clone(), id.clone()), meta.fqdn.clone());

                let docker_addr = SocketAddr::from((docker_ipv4, lan_port_config.internal));

                if lan_port_config.ssl {
                    self.add_certificate_to_resolver(meta.fqdn.clone()).await?;
                }
                // info!("docker ip: {}", docker_addr);
                self.vhosts
                    .add_docker_svc_handle(external_svc_port.0, meta.fqdn.clone(), docker_addr)
                    .await?;
            }
        }

        match self.docker_interfaces.get_mut(&package) {
            None => {
                let info = PackageNetInfo {
                    interfaces: interface_map,
                };
                self.docker_interfaces.insert(package, info);
            }
            Some(p) => {
                p.interfaces.extend(interface_map);
            }
        };

        Ok(())
    }

    #[instrument(skip(self))]
    pub async fn remove_docker_service(&mut self, package: &PackageId) -> Result<(), Error> {
        let mut server_removal = false;
        let mut server_removal_port: u16 = 0;
        let mut removed_interface_id = InterfaceId::default();

        let package_interface_info = self.docker_interfaces.get(package);
        if let Some(net_info) = package_interface_info {
            for (id, meta) in &net_info.interfaces {
                for (service_ext_port, _lan_port_config) in meta.lan_config.iter() {
                    if let Some(server) = self.vhosts.service_servers.get_mut(&service_ext_port.0) {
                        if let Some(dns_base) = self
                            .docker_iface_lookups
                            .get(&(package.clone(), id.clone()))
                        {
                            server.remove_svc_mapping(dns_base.to_string()).await;

                            if server.svc_mapping.read().await.is_empty() {
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
            if let Some(removed_server) = self.vhosts.service_servers.remove(&server_removal_port) {
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
                    .remove(&(package.clone(), removed_interface_id));
            }
        }
        Ok(())
    }

    pub fn get_hostname(&self) -> String {
        self.embassy_hostname.to_owned()
    }
}
