use color_eyre::eyre::eyre;
use futures::ready;
use futures::Future;
use helpers::NonDetachingJoinHandle;
use hyper::server::accept::Accept;
use hyper::server::conn::AddrIncoming;
use hyper::server::conn::AddrStream;
use openssl::pkey::PKey;
use openssl::pkey::Private;
use openssl::x509::X509;
use std::collections::BTreeMap;
use std::io;
use std::net::IpAddr;
use tokio::io::AsyncRead;
use tokio::io::AsyncWrite;
use tokio::io::ReadBuf;
use tokio_rustls::rustls::server::ResolvesServerCert;
use tokio_rustls::rustls::sign::any_supported_type;
use tokio_rustls::rustls::sign::CertifiedKey;
use tokio_rustls::rustls::Certificate;
use tokio_rustls::rustls::PrivateKey;
use tokio_rustls::rustls::ServerConfig;

use std::net::SocketAddr;
use std::pin::Pin;
use std::sync::Arc;
use std::task::Context;
use std::task::Poll;

use crate::net::net_utils::host_addr;
use crate::net::HttpHandler;
use crate::Error;
use http::StatusCode;
use hyper::service::{make_service_fn, service_fn};
use hyper::{Body, Error as HyperError, Response, Server};
use tokio::sync::{oneshot, RwLock};
use tracing::error;

static RES_NOT_FOUND: &[u8] = b"503 Service Unavailable";
static NO_HOST: &[u8] = b"No host header found";

enum State {
    Handshaking(tokio_rustls::Accept<AddrStream>),
    Streaming(tokio_rustls::server::TlsStream<AddrStream>),
}

// tokio_rustls::server::TlsStream doesn't expose constructor methods,
// so we have to TlsAcceptor::accept and handshake to have access to it
// TlsStream implements AsyncRead/AsyncWrite handshaking tokio_rustls::Accept first
pub struct TlsStream {
    state: State,
}

impl TlsStream {
    fn new(stream: AddrStream, config: Arc<ServerConfig>) -> TlsStream {
        let accept = tokio_rustls::TlsAcceptor::from(config).accept(stream);
        TlsStream {
            state: State::Handshaking(accept),
        }
    }
}

impl AsyncRead for TlsStream {
    fn poll_read(
        self: Pin<&mut Self>,
        cx: &mut Context,
        buf: &mut ReadBuf,
    ) -> Poll<io::Result<()>> {
        let pin = self.get_mut();
        match pin.state {
            State::Handshaking(ref mut accept) => match ready!(Pin::new(accept).poll(cx)) {
                Ok(mut stream) => {
                    let result = Pin::new(&mut stream).poll_read(cx, buf);
                    pin.state = State::Streaming(stream);
                    result
                }
                Err(err) => Poll::Ready(Err(err)),
            },
            State::Streaming(ref mut stream) => Pin::new(stream).poll_read(cx, buf),
        }
    }
}

impl AsyncWrite for TlsStream {
    fn poll_write(
        self: Pin<&mut Self>,
        cx: &mut Context<'_>,
        buf: &[u8],
    ) -> Poll<io::Result<usize>> {
        let pin = self.get_mut();
        match pin.state {
            State::Handshaking(ref mut accept) => match ready!(Pin::new(accept).poll(cx)) {
                Ok(mut stream) => {
                    let result = Pin::new(&mut stream).poll_write(cx, buf);
                    pin.state = State::Streaming(stream);
                    result
                }
                Err(err) => Poll::Ready(Err(err)),
            },
            State::Streaming(ref mut stream) => Pin::new(stream).poll_write(cx, buf),
        }
    }

    fn poll_flush(mut self: Pin<&mut Self>, cx: &mut Context<'_>) -> Poll<io::Result<()>> {
        match self.state {
            State::Handshaking(_) => Poll::Ready(Ok(())),
            State::Streaming(ref mut stream) => Pin::new(stream).poll_flush(cx),
        }
    }

    fn poll_shutdown(mut self: Pin<&mut Self>, cx: &mut Context<'_>) -> Poll<io::Result<()>> {
        match self.state {
            State::Handshaking(_) => Poll::Ready(Ok(())),
            State::Streaming(ref mut stream) => Pin::new(stream).poll_shutdown(cx),
        }
    }
}

impl ResolvesServerCert for EmbassyCertResolver {
    fn resolve(
        &self,
        client_hello: tokio_rustls::rustls::server::ClientHello,
    ) -> Option<Arc<tokio_rustls::rustls::sign::CertifiedKey>> {
        let hostname = client_hello.server_name();

        match hostname {
            Some(hostname) => {
                dbg!("am i stopping here at resolving");
                let lock = self.cert_mapping.blocking_read();

                lock.get(hostname)
                    .map(|cert_key| Arc::new(cert_key.to_owned()))
            }
            None => None,
        }
    }
}

#[derive(Clone, Default)]
pub struct EmbassyCertResolver {
    cert_mapping: Arc<RwLock<BTreeMap<String, CertifiedKey>>>,
}

impl EmbassyCertResolver {
    pub fn new() -> Self {
        Self::default()
    }
    pub async fn add_certificate_to_resolver(
        &mut self,
        hostname: String,
        package_cert_data: (PKey<Private>, Vec<X509>),
    ) -> Result<(), Error> {
        let x509_cert_chain = package_cert_data.1;
        let private_keys = package_cert_data
            .0
            .private_key_to_der()
            .map_err(|err| Error::new(eyre!("err {}", err), crate::ErrorKind::BytesError))?;

        let mut full_rustls_certs = Vec::new();
        for cert in x509_cert_chain.iter() {
            let cert =
                Certificate(cert.to_der().map_err(|err| {
                    Error::new(eyre!("err: {}", err), crate::ErrorKind::BytesError)
                })?);

            full_rustls_certs.push(cert);
        }
        let pre_sign_key = PrivateKey(private_keys);
        let actual_sign_key = any_supported_type(&pre_sign_key)
            .map_err(|err| Error::new(eyre!("{}", err), crate::ErrorKind::SignError))?;

        let cert_key = CertifiedKey::new(full_rustls_certs, actual_sign_key);

        let mut lock = self.cert_mapping.write().await;

        lock.insert(hostname, cert_key);

        Ok(())
    }

    pub async fn remove_cert(&mut self, hostname: String) {
        let mut lock = self.cert_mapping.write().await;

        lock.remove(&hostname);
    }
}

pub struct TlsAcceptor {
    config: Arc<ServerConfig>,
    incoming: AddrIncoming,
}

impl TlsAcceptor {
    pub fn new(config: Arc<ServerConfig>, incoming: AddrIncoming) -> TlsAcceptor {
        TlsAcceptor { config, incoming }
    }
}

impl Accept for TlsAcceptor {
    type Conn = TlsStream;
    type Error = io::Error;

    fn poll_accept(
        self: Pin<&mut Self>,
        cx: &mut Context<'_>,
    ) -> Poll<Option<Result<Self::Conn, Self::Error>>> {
        let pin = self.get_mut();
        match ready!(Pin::new(&mut pin.incoming).poll_accept(cx)) {
            Some(Ok(sock)) => Poll::Ready(Some(Ok(TlsStream::new(sock, pin.config.clone())))),
            Some(Err(e)) => Poll::Ready(Some(Err(e))),
            None => Poll::Ready(None),
        }
    }
}

pub struct EmbassyServiceHTTPServer {
    // String: Virtual
    pub svc_mapping: Arc<RwLock<BTreeMap<String, HttpHandler>>>,
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

        let server_service_mapping = Arc::new(RwLock::new(BTreeMap::<String, HttpHandler>::new()));
        let server_service_mapping1 = server_service_mapping.clone();

        let bare_make_service_fn = move || {
            let server_service_mapping = server_service_mapping.clone();

            async move {
                let server_service_mapping = server_service_mapping.clone();

                Ok::<_, HyperError>(service_fn(move |req| {
                    dbg!(req.uri());

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
