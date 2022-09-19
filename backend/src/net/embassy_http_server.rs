use color_eyre::eyre::eyre;
use futures::future::BoxFuture;
use futures::FutureExt;
use helpers::NonDetachingJoinHandle;
use hyper::upgrade::Upgraded;
use std::collections::BTreeMap;
use std::net::IpAddr;

use std::net::SocketAddr;
use std::sync::Arc;

use http::{Method, StatusCode};
use hyper::service::{make_service_fn, service_fn};
use tokio::net::TcpStream;
use tokio::sync::{oneshot, RwLock};
use tracing::{error, info};

use crate::net::ssl::SslManager;
use crate::Error;

use hyper::{Body, Client, Error as HyperError, Request, Response, Server};

type HttpClient = Client<hyper::client::HttpConnector>;
type HttpHandler = Arc<
    dyn Fn(Request<Body>) -> BoxFuture<'static, Result<Response<Body>, HyperError>> + Send + Sync,
>;

static RES_NOT_FOUND: &[u8] = b"503 Service Unavailable";
static NO_HOST: &[u8] = b"No host header found";
pub struct EmbassyHTTPServer {
    pub docker_mapping: Arc<RwLock<BTreeMap<String, HttpHandler>>>,
    pub shutdown: oneshot::Sender<()>,
    pub handle: NonDetachingJoinHandle<()>,
}

impl EmbassyHTTPServer {
    pub async fn new(listener_addr: IpAddr, port: u16) -> Result<Self, Error> {
        let (tx, rx) = tokio::sync::oneshot::channel::<()>();

        let listener_socket_addr = SocketAddr::from((listener_addr, port));

        let docker_service_mapping = Arc::new(RwLock::new(BTreeMap::<String, HttpHandler>::new()));
        let docker_service_mapping1 = docker_service_mapping.clone();

        let make_service = make_service_fn(move |_| {
            let docker_service_mapping = docker_service_mapping.clone();

            async move {
                let docker_service_mapping = docker_service_mapping.clone();

                Ok::<_, HyperError>(service_fn(move |req| {
                    let docker_service_mapping = docker_service_mapping.clone();

                    async move {
                        let docker_service_mapping = docker_service_mapping.clone();

                        let host = Self::host_addr(&req);

                        match host {
                            Ok(host_str) => {
                                // host_str is a string like example.com:443, we just want the fqdn before the semi colon
                                let dns_base =
                                    host_str.split(':').next().unwrap_or_default().to_string();

                                let docker_handler_option = {
                                    let mapping = docker_service_mapping.read().await;
                                    mapping.get(&dns_base).cloned()
                                };

                                match docker_handler_option {
                                    Some(handler) => handler(req).await,
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
            let server = Server::bind(&listener_socket_addr)
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

    pub async fn add_docker_mapping(&mut self, dns_base: String, docker_addr: SocketAddr) {
        let docker_handler: HttpHandler = Arc::new(move |mut req| {
            async move {
                let client = HttpClient::new();

                let uri_string = format!(
                    "http://{}{}",
                    docker_addr,
                    req.uri()
                        .path_and_query()
                        .map(|x| x.as_str())
                        .unwrap_or("/")
                );

                let uri = uri_string.parse().unwrap();
                *req.uri_mut() = uri;

                // Ok::<_, HyperError>(Response::new(Body::empty()))
                return Self::proxy(client, req).await;
            }
            .boxed()
        });
        let mut mapping = self.docker_mapping.write().await;

        mapping.insert(dns_base.to_string(), docker_handler);
    }

    pub async fn remove_docker_mapping(&mut self, dns_base: String) {
        let mut mapping = self.docker_mapping.write().await;

        mapping.remove(&dns_base);
    }

    async fn proxy(client: HttpClient, req: Request<Body>) -> Result<Response<Body>, HyperError> {
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
