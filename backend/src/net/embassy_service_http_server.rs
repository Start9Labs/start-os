use helpers::NonDetachingJoinHandle;
use std::collections::BTreeMap;
use std::net::IpAddr;

use std::net::SocketAddr;
use std::sync::Arc;

use http::StatusCode;
use hyper::service::{make_service_fn, service_fn};
use tokio::sync::{oneshot, RwLock};
use tracing::error;
use crate::Error;
use crate::net::HttpHandler;
use crate::net::net_utils::host_addr;
use hyper::{Body, Error as HyperError, Response, Server};



static RES_NOT_FOUND: &[u8] = b"503 Service Unavailable";
static NO_HOST: &[u8] = b"No host header found";

pub struct EmbassyServiceHTTPServer {
    // String: Virtual
    pub svc_mapping: Arc<RwLock<BTreeMap<String, HttpHandler>>>,
    pub shutdown: oneshot::Sender<()>,
    pub handle: NonDetachingJoinHandle<()>,
}

impl EmbassyServiceHTTPServer {
    pub async fn new(listener_addr: IpAddr, port: u16) -> Result<Self, Error> {
        let (tx, rx) = tokio::sync::oneshot::channel::<()>();

        let listener_socket_addr = SocketAddr::from((listener_addr, port));

        let server_service_mapping = Arc::new(RwLock::new(BTreeMap::<String, HttpHandler>::new()));
        let server_service_mapping1 = server_service_mapping.clone();

        let make_service = make_service_fn(move |_| {
            let server_service_mapping = server_service_mapping.clone();

            async move {
                let server_service_mapping = server_service_mapping.clone();

                Ok::<_, HyperError>(service_fn(move |req| {
                    let server_service_mapping = server_service_mapping.clone();

                    async move {
                        let server_service_mapping = server_service_mapping.clone();

                        let host = host_addr(&req);

                        match host {
                            Ok(host_str) => {
                                // host_str is a string like example.com:443, we just want the fqdn before the semi colon
                                let dns_base =
                                    host_str.split(':').next().unwrap_or_default().to_string();

                                let server_handler_option = {
                                    let mapping = server_service_mapping.read().await;
                                    mapping.get(&dns_base).cloned()
                                };

                                match server_handler_option {
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
            svc_mapping: server_service_mapping1,
            handle: handle.into(),
            shutdown: tx,
        })
    }

    pub async fn add_svc_mapping(&mut self, fqdn: String, svc_handle: HttpHandler) {
        let mut mapping = self.svc_mapping.write().await;
        mapping.insert(fqdn, svc_handle);
    }

    pub async fn remove_svc_mapping(&mut self, fqdn: String) {
        let mut mapping = self.svc_mapping.write().await;

        mapping.remove(&fqdn);
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
