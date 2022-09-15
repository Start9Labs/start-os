use color_eyre::eyre::eyre;
use helpers::NonDetachingJoinHandle;
use hyper::upgrade::Upgraded;
use std::collections::BTreeMap;
use std::fmt::format;
use std::net::{Ipv4Addr, SocketAddr};
use std::sync::Arc;

use http::{Method, StatusCode};
use hyper::service::{make_service_fn, service_fn};
use models::{InterfaceId, PackageId};
use tokio::net::TcpStream;
use tokio::sync::{oneshot, Mutex, RwLock};
use tracing::{error, info, instrument};

use crate::net::ssl::SslManager;
use crate::net::{InterfaceMetadata, PackageNetInfo};
//use crate::util::IntoDoubleEndedIterator;
use crate::{Error, ResultExt};

use hyper::{Body, Client, Error as HyperError, Request, Response, Server};

type HttpClient = Client<hyper::client::HttpConnector>;

static RES_NOT_FOUND: &[u8] = b"503 Service Unavailable";
static NO_HOST: &[u8] = b"No host header found";
struct EmbassyHTTPServer {
    docker_mapping: Arc<RwLock<BTreeMap<String, SocketAddr>>>,
    shutdown: oneshot::Sender<()>,
    handle: NonDetachingJoinHandle<()>,
}
impl EmbassyHTTPServer {
    async fn new(listener_addr: SocketAddr) -> Result<Self, Error> {
        let (tx, rx) = tokio::sync::oneshot::channel::<()>();
        let client = HttpClient::new();
        dbg!(listener_addr);
        let docker_service_mapping = Arc::new(RwLock::new(BTreeMap::<String, SocketAddr>::new()));

        let docker_service_mapping1 = docker_service_mapping.clone();

        let make_service = make_service_fn(move |_| {
            let client = client.clone();
            let docker_service_mapping = docker_service_mapping.clone();

            async move {
                let docker_service_mapping = docker_service_mapping.clone();

                Ok::<_, HyperError>(service_fn(move |mut req| {
                    let docker_service_mapping = docker_service_mapping.clone();
                    let client = client.clone();

                    async move {
                        let docker_service_mapping = docker_service_mapping.clone();

                        let host = Self::host_addr(&req);

                        match host {
                            Ok(host_str) => {
                                let dns_base =
                                    host_str.split(':').next().unwrap_or_default().to_string();

                                let docker_option = {
                                    let mapping = docker_service_mapping.read_owned().await;
                                    mapping.get(dbg!(&dns_base)).copied()
                                };

                                match docker_option {
                                    Some(docker_addr) => {
                                        let uri_string = format!(
                                            "http://{}{}",
                                            docker_addr,
                                            req.uri()
                                                .path_and_query()
                                                .map(|x| x.as_str())
                                                .unwrap_or("/")
                                        );

                                        dbg!(uri_string.clone());
                                         let uri = uri_string.parse().unwrap();
                                        *req.uri_mut() = uri;

                                        // Ok::<_, HyperError>(Response::new(Body::empty()))
                                        return Self::proxy(client, req).await;
                                    }
                                    None => Ok(res_not_found()),
                                }
                            }
                            Err(e) => Ok(no_host_found(e)),
                        }
                    }
                }))
            }
        });

        let handle = tokio::spawn(async move {
            let server = Server::bind(&listener_addr)
                .http1_preserve_header_case(true)
                .http1_title_case_headers(true)
                .serve(make_service)
                .with_graceful_shutdown({
                    async {
                        rx.await.ok();
                    }
                });

            if let Err(e) = server.await {
                error!("Spawning hyper server errorr: {}", e);
            }
        });

        Ok(Self {
            docker_mapping: docker_service_mapping1,
            handle: handle.into(),
            shutdown: tx,
        })
    }

    async fn add_docker_mapping(&mut self, dns_base: &String, docker_addr: SocketAddr) {
        let mut mapping = self.docker_mapping.write().await;

        mapping.insert(dns_base.to_string(), docker_addr);
    }

    async fn remove_docker_mapping(&mut self, dns_base: String) {
        let mut mapping = self.docker_mapping.write().await;

        mapping.remove(&dns_base);
    }

    async fn proxy(client: HttpClient, req: Request<Body>) -> Result<Response<Body>, hyper::Error> {
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
            match Self::host_addr(&req) {
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

    fn host_addr(req: &Request<Body>) -> Result<String, Error> {
        // uri.authority().o
        let host = req.headers().get(http::header::HOST);

        match host {
            Some(host) => {
                let host = host
                    .to_str()
                    .map_err(|e| Error::new(eyre!("{}", e), crate::ErrorKind::AsciiError))?
                    .to_string();

                Ok(host)
            }

            None => Err(Error::new(eyre!("No Host"), crate::ErrorKind::NoHost)),
        }
    }
}

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
                        && meta.lan_config.len() > 0
            })
            .collect::<BTreeMap<InterfaceId, InterfaceMetadata>>();

        for (id, meta) in interface_map.iter() {
            for (external_svc_port, lan_port_config) in meta.lan_config.iter() {
                let mut listener_addr = self.embassyd_addr;
                listener_addr.set_port(external_svc_port.0);
                info!("listener addr: {}", listener_addr);

                self.iface_lookups
                    .insert((package.clone(), id.clone()), meta.dns_base.clone());

                let docker_addr = SocketAddr::from((ipv4, lan_port_config.internal));
                info!("docker ip: {}", docker_addr);
                if let Some(server) = self.service_servers.get_mut(&external_svc_port.0) {
                    server.add_docker_mapping(dbg!(&meta.dns_base), docker_addr).await;
                } else {
                    let mut new_service_server = EmbassyHTTPServer::new(listener_addr).await?;
                    new_service_server
                        .add_docker_mapping(dbg!(&meta.dns_base), docker_addr)
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

/// HTTP status code 503
fn res_not_found() -> Response<Body> {
    Response::builder()
        .status(StatusCode::SERVICE_UNAVAILABLE)
        .body(RES_NOT_FOUND.into())
        .unwrap()
}

fn no_host_found(err: Error) -> Response<Body> {
    let err_txt = format!("{}: Error {}", String::from_utf8_lossy(NO_HOST), err);
    Response::builder()
        .status(StatusCode::BAD_REQUEST)
        .body(err_txt.into())
        .unwrap()
}
