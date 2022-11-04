use std::collections::BTreeMap;
use std::net::{IpAddr, SocketAddr};
use std::sync::Arc;

use helpers::NonDetachingJoinHandle;
use http::StatusCode;
use hyper::server::conn::AddrIncoming;
use hyper::service::{make_service_fn, service_fn};
use hyper::{Body, Error as HyperError, Response, Server};
use tokio::sync::oneshot;
use tokio_rustls::rustls::ServerConfig;
use tracing::error;

use crate::net::cert_resolver::TlsAcceptor;
use crate::net::net_utils::{host_addr_fqdn, ResourceFqdn};
use crate::net::HttpHandler;
use crate::Error;

static RES_NOT_FOUND: &[u8] = b"503 Service Unavailable";
static NO_HOST: &[u8] = b"No host header found";

pub struct EmbassyServiceHTTPServer {
    pub svc_mapping: Arc<tokio::sync::RwLock<BTreeMap<ResourceFqdn, HttpHandler>>>,
    pub shutdown: oneshot::Sender<()>,
    pub handle: NonDetachingJoinHandle<()>,
    pub ssl_cfg: Option<Arc<ServerConfig>>,
}

impl EmbassyServiceHTTPServer {
    pub async fn new(
        listener_addr: IpAddr,
        port: u16,
        ssl_cfg: Option<Arc<ServerConfig>>,
    ) -> Result<Self, Error> {
        let (tx, rx) = tokio::sync::oneshot::channel::<()>();

        let listener_socket_addr = SocketAddr::from((listener_addr, port));

        let server_service_mapping = Arc::new(tokio::sync::RwLock::new(BTreeMap::<
            ResourceFqdn,
            HttpHandler,
        >::new()));

        let server_service_mapping1 = server_service_mapping.clone();

        let bare_make_service_fn = move || {
            let server_service_mapping = server_service_mapping.clone();

            async move {
                Ok::<_, HyperError>(service_fn(move |req| {
                    let mut server_service_mapping = server_service_mapping.clone();

                    async move {
                        server_service_mapping = server_service_mapping.clone();

                        let host = host_addr_fqdn(&req);
                        match host {
                            Ok(host_uri) => {
                                let res = {
                                    let mapping = server_service_mapping.read().await;

                                    let opt_handler = mapping.get(&host_uri).cloned();

                                    opt_handler
                                };
                                match res {
                                    Some(opt_handler) => {
                                        let response = opt_handler(req).await;

                                        match response {
                                            Ok(resp) => Ok::<Response<Body>, hyper::Error>(resp),
                                            Err(err) => Ok(respond_hyper_error(err)),
                                        }
                                    }
                                    None => Ok(res_not_found()),
                                }
                            }
                            Err(e) => Ok(no_host_found(e)),
                        }
                    }
                }))
            }
        };

        let inner_ssl_cfg = ssl_cfg.clone();
        let handle = tokio::spawn(async move {
            match inner_ssl_cfg {
                Some(cfg) => {
                    let incoming = AddrIncoming::bind(&listener_socket_addr).unwrap();

                    let server = Server::builder(TlsAcceptor::new(cfg, incoming))
                        .http1_preserve_header_case(true)
                        .http1_title_case_headers(true)
                        .serve(make_service_fn(|_| bare_make_service_fn()))
                        .with_graceful_shutdown({
                            async {
                                rx.await.ok();
                            }
                        });

                    if let Err(e) = server.await {
                        error!("Spawning hyper server errorr: {}", e);
                    }
                }
                None => {
                    let server = Server::bind(&listener_socket_addr)
                        .http1_preserve_header_case(true)
                        .http1_title_case_headers(true)
                        .serve(make_service_fn(|_| bare_make_service_fn()))
                        .with_graceful_shutdown({
                            async {
                                rx.await.ok();
                            }
                        });
                    if let Err(e) = server.await {
                        error!("Spawning hyper server errorr: {}", e);
                    }
                }
            };
        });

        Ok(Self {
            svc_mapping: server_service_mapping1,
            handle: handle.into(),
            shutdown: tx,
            ssl_cfg,
        })
    }

    pub async fn add_svc_handler_mapping(
        &mut self,
        fqdn: ResourceFqdn,
        svc_handle: HttpHandler,
    ) -> Result<(), Error> {
        let mut mapping = self.svc_mapping.write().await;

        mapping.insert(fqdn.clone(), svc_handle);

        Ok(())
    }

    pub async fn remove_svc_handler_mapping(&mut self, fqdn: ResourceFqdn) -> Result<(), Error> {
        let mut mapping = self.svc_mapping.write().await;

        mapping.remove(&fqdn);

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

fn respond_hyper_error(err: hyper::Error) -> Response<Body> {
    let err_txt = format!("{}: Error {}", String::from_utf8_lossy(NO_HOST), err);
    Response::builder()
        .status(StatusCode::BAD_REQUEST)
        .body(err_txt.into())
        .unwrap()
}
