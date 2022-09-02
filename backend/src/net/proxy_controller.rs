use bytes::Bytes;
use http_body::Body;
use http_body::{combinators::BoxBody, Empty, Full};
use hyper::upgrade::Upgraded;
use std::collections::BTreeMap;
use std::net::{Ipv4Addr, SocketAddr};
use std::path::{Path, PathBuf};
use std::str::FromStr;

use http::{Method, Request, Response, StatusCode};
use hyper::client::conn::Builder;
use hyper::server::conn::Http;
use hyper::service::{make_service_fn, service_fn};
use models::{InterfaceId, PackageId};
use tokio::net::{TcpListener, TcpStream};
use tokio::sync::Mutex;
use tracing::{debug, instrument};

use crate::net::ssl::SslManager;
use crate::net::{InterfaceMetadata, PackageNetInfo};
use crate::{Error, ResultExt};

use hyper::{Error as HyperError, Recv};

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
    embassyd_addr: SocketAddr,
    interfaces: BTreeMap<PackageId, PackageNetInfo>,
}

impl ProxyControllerInner {
    #[instrument]
    async fn init(embassyd_addr: SocketAddr, ssl_manager: &SslManager) -> Result<Self, Error> {
        let inner = ProxyControllerInner {
            interfaces: BTreeMap::new(),
            embassyd_addr,
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
                let listener_addr = SocketAddr::from_str(&format!("{}:{}", meta.dns_base, port.0))
                    .with_kind(crate::ErrorKind::InvalidIP)?;

                let listener = TcpListener::bind(listener_addr).await?;
                debug!("Listening on {}", listener_addr);

                let make_service = make_service_fn(move |_| async move {
                    let (stream, _) = listener.accept().await?;
                    Ok::<_, HyperError>(service_fn(move |req| async move {
                        if let Err(err) = Http::new()
                            .http1_preserve_header_case(true)
                            .http1_title_case_headers(true)
                            .serve_connection(stream, service_fn(Self::proxy))
                            .with_upgrades()
                            .await
                        {
                            println!("Failed to serve connection: {:?}", err);
                        }
                    }))
                });
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

    async fn proxy(
        req: Request<Recv>,
    ) -> Result<Response<BoxBody<Bytes, hyper::Error>>, hyper::Error> {
        println!("req: {:?}", req);

        if Method::CONNECT == req.method() {
            // Received an HTTP request like:
            // ```
            // CONNECT www.domain.com:443 HTTP/1.1
            // Host: www.domain.com:443
            // Proxy-Connection: Keep-Alive
            // ```
            //
            // When HTTP method is CONNECT we should return an empty body
            // the n we can eventually upgrade the connection and talk a new protocol.
            //
            // Note: only after client received an empty body with STATUS_OK can the
            // connection be upgraded, so we can't return a response inside
            // `on_upgrade` future.
            if let Some(addr) = Self::host_addr(req.uri()) {
                tokio::task::spawn(async move {
                    match hyper::upgrade::on(req).await {
                        Ok(upgraded) => {
                            if let Err(e) = Self::tunnel(upgraded, addr).await {
                                eprintln!("server io error: {}", e);
                            };
                        }
                        Err(e) => eprintln!("upgrade error: {}", e),
                    }
                });

                Ok(Response::new(Self::empty()))
            } else {
                debug!("CONNECT host is not socket addr: {:?}", req.uri());
                let mut resp = Response::new(Self::full("CONNECT must be to a socket address"));
                *resp.status_mut() = http::StatusCode::BAD_REQUEST;

                Ok(resp)
            }
        } else {
            let host = req.uri().host().expect("uri has no host");
            let port = req.uri().port_u16().unwrap_or(80);
            let addr = format!("{}:{}", host, port);

            let stream = TcpStream::connect(addr).await.unwrap();

            let (mut sender, conn) = Builder::new()
                .http1_preserve_header_case(true)
                .http1_title_case_headers(true)
                .handshake(stream)
                .await?;
            tokio::task::spawn(async move {
                if let Err(err) = conn.await {
                    println!("Connection failed: {:?}", err);
                }
            });

            let resp = sender.send_request(req).await?;
            Ok(resp.map(|b| b.boxed()))
        }
    }

    // Create a TCP connection to host:port, build a tunnel between the connection and
    // the upgraded connection
    async fn tunnel(mut upgraded: Upgraded, addr: String) -> std::io::Result<()> {
        // Connect to remote server
        let mut server = TcpStream::connect(addr).await?;

        // Proxying data
        let (from_client, from_server) =
            tokio::io::copy_bidirectional(&mut upgraded, &mut server).await?;

        // Print message when done
        println!(
            "client wrote {} bytes and received {} bytes",
            from_client, from_server
        );

        Ok(())
    }

    /// HTTP status code 500
    fn server_error() -> Response<Body> {
        Response::builder()
            .status(StatusCode::INTERNAL_SERVER_ERROR)
            .body("".into())
            .unwrap()
    }

    fn host_addr(uri: &http::Uri) -> Option<String> {
        uri.authority().and_then(|auth| Some(auth.to_string()))
    }

    fn empty() -> BoxBody<Bytes, hyper::Error> {
        Empty::<Bytes>::new()
            .map_err(|never| match never {})
            .boxed()
    }

    fn full<T: Into<Bytes>>(chunk: T) -> BoxBody<Bytes, hyper::Error> {
        Full::new(chunk.into())
            .map_err(|never| match never {})
            .boxed()
    }
}
