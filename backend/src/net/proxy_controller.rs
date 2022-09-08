use color_eyre::eyre::eyre;
use helpers::NonDetachingJoinHandle;
use hyper::upgrade::Upgraded;
use libc::listen;
use std::collections::BTreeMap;
use std::net::{Ipv4Addr, SocketAddr};
use std::str::FromStr;
use std::sync::Arc;

use http::{Method, Request, Response};
use hyper::service::{make_service_fn, service_fn};
use models::{InterfaceId, PackageId};
use tokio::net::TcpStream;
use tokio::sync::{oneshot, Mutex, RwLock};
use tracing::{debug, error, info, instrument};

use crate::net::ssl::SslManager;
use crate::net::{InterfaceMetadata, PackageNetInfo};
use crate::{Error, ResultExt};

use hyper::{Body, Client, Error as HyperError, Server};

type HttpClient = Client<hyper::client::HttpConnector>;

struct EmbassyHTTPServer {
    mapping: Arc<RwLock<BTreeMap<String, SocketAddr>>>,
    shutdown: oneshot::Sender<()>,
    handle: NonDetachingJoinHandle<()>,
}

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
        self.inner.lock().await.remove(package).await
    }
}

struct ProxyControllerInner {
    servers: BTreeMap<u16, EmbassyHTTPServer>,
    iface_lookups: BTreeMap<(PackageId, InterfaceId), String>,
    interfaces: BTreeMap<PackageId, PackageNetInfo>,
}

impl ProxyControllerInner {
    #[instrument]
    async fn init(ssl_manager: &SslManager) -> Result<Self, Error> {
        let inner = ProxyControllerInner {
            interfaces: BTreeMap::new(),
            iface_lookups: BTreeMap::new(),
            servers: BTreeMap::new(),
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

        let client = Client::builder()
            .http1_title_case_headers(true)
            .http1_preserve_header_case(true)
            .build_http();

        for (id, meta) in interface_map.iter() {
            for (port, _lan_port_config) in meta.lan_config.iter() {
                let listener_addr =
                    SocketAddr::from_str(&format!("{}:{}", meta.dns_base.clone(), port.0))
                        .with_kind(crate::ErrorKind::InvalidIP)?;

                debug!("Listening on {}", listener_addr);
                let client = client.clone();
                let make_service = make_service_fn(move |_| {
                    let client = client.clone();
                    async move {
                        Ok::<_, HyperError>(service_fn(move |req| Self::proxy(client.clone(), req)))
                    }
                });

                let (tx, rx) = tokio::sync::oneshot::channel::<()>();

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

                self.iface_lookups
                    .insert((package.clone(), id.clone()), meta.dns_base.clone());

                if let Some(server) = self.servers.get_mut(&port.0) {
                    server
                        .mapping
                        .write()
                        .await
                        .insert(meta.dns_base.clone(), listener_addr);
                } else {
                    let mut mapping = BTreeMap::new();
                    mapping.insert(meta.dns_base.clone(), listener_addr);
                    let map_lock = Arc::new(RwLock::new(mapping));
                    self.servers.insert(
                        port.0,
                        EmbassyHTTPServer {
                            mapping: map_lock,
                            shutdown: tx,
                            handle: handle.into(),
                        },
                    );
                }

                info!("Listening on {}", listener_addr);
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
        let removed = self.interfaces.remove(package);
        if let Some(net_info) = removed {
            for (id, meta) in net_info.interfaces {
                for (service_ext_port, _lan_port_config) in meta.lan_config.iter() {
                    if let Some((_server_port, server)) =
                        self.servers.get_key_value(&service_ext_port.0)
                    {
                        let mut mapping = server.mapping.write().await;

                        if let Some(dns_base) =
                            self.iface_lookups.get(&(package.to_owned(), id.clone()))
                        {
                            let data = mapping.remove(dns_base);

                            match data {
                                Some(_) => (),
                                None => {
                                    async move {
                                        server.shutdown.send(()).map_err(|_| {
                                            Error::new(
                                                eyre!("Hyper server did not quit properly"),
                                                crate::ErrorKind::JoinError,
                                            )
                                        })?;
                                        server
                                            .handle
                                            .await
                                            .with_kind(crate::ErrorKind::JoinError)?;
                                    };
                                }
                            }
                        }
                        if mapping.len() == 0 {
                            self.servers.remove(&service_ext_port.0);
                        }
                    }
                }
            }
        }
        Ok(())
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
            if let Some(addr) = Self::host_addr(req.uri()) {
                tokio::task::spawn(async move {
                    match hyper::upgrade::on(req).await {
                        Ok(upgraded) => {
                            if let Err(e) = Self::tunnel(upgraded, addr).await {
                                error!("server io error: {}", e);
                            };
                        }
                        Err(e) => error!("upgrade error: {}", e),
                    }
                });

                Ok(Response::new(Body::empty()))
            } else {
                let err_txt = format!("CONNECT host is not socket addr: {:?}", req.uri());
                let mut resp = Response::new(Body::from(format!(
                    "CONNECT must be to a socket address: {}",
                    err_txt
                )));
                *resp.status_mut() = http::StatusCode::BAD_REQUEST;

                Ok(resp)
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

    fn host_addr(uri: &http::Uri) -> Option<String> {
        uri.authority().map(|auth| auth.to_string())
    }
}
