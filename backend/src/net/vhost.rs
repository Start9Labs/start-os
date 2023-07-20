use std::collections::BTreeMap;
use std::convert::Infallible;
use std::net::{IpAddr, Ipv6Addr, SocketAddr};
use std::str::FromStr;
use std::sync::{Arc, Weak};
use std::time::Duration;

use color_eyre::eyre::eyre;
use helpers::NonDetachingJoinHandle;
use http::{Response, Uri};
use hyper::service::{make_service_fn, service_fn};
use hyper::Body;
use models::ResultExt;
use tokio::net::{TcpListener, TcpStream};
use tokio::sync::{Mutex, RwLock};
use tokio_rustls::rustls::server::Acceptor;
use tokio_rustls::rustls::{RootCertStore, ServerConfig};
use tokio_rustls::{LazyConfigAcceptor, TlsConnector};

use crate::net::keys::Key;
use crate::net::ssl::SslManager;
use crate::net::utils::SingleAccept;
use crate::util::io::{BackTrackingReader, TimeoutStream};
use crate::Error;

// not allowed: <=1024, >=32768, 5355, 5432, 9050, 6010, 9051, 5353

pub struct VHostController {
    ssl: Arc<SslManager>,
    servers: Mutex<BTreeMap<u16, VHostServer>>,
}
impl VHostController {
    pub fn new(ssl: Arc<SslManager>) -> Self {
        Self {
            ssl,
            servers: Mutex::new(BTreeMap::new()),
        }
    }
    pub async fn add(
        &self,
        key: Key,
        hostname: Option<String>,
        external: u16,
        target: SocketAddr,
        connect_ssl: Result<(), AlpnInfo>,
    ) -> Result<Arc<()>, Error> {
        let mut writable = self.servers.lock().await;
        let server = if let Some(server) = writable.remove(&external) {
            server
        } else {
            VHostServer::new(external, self.ssl.clone()).await?
        };
        let rc = server
            .add(
                hostname,
                TargetInfo {
                    addr: target,
                    connect_ssl,
                    key,
                },
            )
            .await;
        writable.insert(external, server);
        Ok(rc?)
    }
    pub async fn gc(&self, hostname: Option<String>, external: u16) -> Result<(), Error> {
        let mut writable = self.servers.lock().await;
        if let Some(server) = writable.remove(&external) {
            server.gc(hostname).await?;
            if !server.is_empty().await? {
                writable.insert(external, server);
            }
        }
        Ok(())
    }
}

#[derive(Clone, PartialEq, Eq, PartialOrd, Ord)]
struct TargetInfo {
    addr: SocketAddr,
    connect_ssl: Result<(), AlpnInfo>,
    key: Key,
}

#[derive(Clone, PartialEq, Eq, PartialOrd, Ord)]
pub enum AlpnInfo {
    Reflect,
    Specified(Vec<Vec<u8>>),
}

struct VHostServer {
    mapping: Weak<RwLock<BTreeMap<Option<String>, BTreeMap<TargetInfo, Weak<()>>>>>,
    _thread: NonDetachingJoinHandle<()>,
}
impl VHostServer {
    async fn new(port: u16, ssl: Arc<SslManager>) -> Result<Self, Error> {
        // check if port allowed
        let listener = TcpListener::bind(SocketAddr::new(Ipv6Addr::UNSPECIFIED.into(), port))
            .await
            .with_kind(crate::ErrorKind::Network)?;
        let mapping = Arc::new(RwLock::new(BTreeMap::new()));
        Ok(Self {
            mapping: Arc::downgrade(&mapping),
            _thread: tokio::spawn(async move {
                loop {
                    match listener.accept().await {
                        Ok((stream, _)) => {
                            let stream =
                                Box::pin(TimeoutStream::new(stream, Duration::from_secs(300)));
                            let mut stream = BackTrackingReader::new(stream);
                            stream.start_buffering();
                            let mapping = mapping.clone();
                            let ssl = ssl.clone();
                            tokio::spawn(async move {
                                if let Err(e) = async {
                                    let mid = match LazyConfigAcceptor::new(
                                        Acceptor::default(),
                                        &mut stream,
                                    )
                                    .await
                                    {
                                        Ok(a) => a,
                                        Err(_) => {
                                            stream.rewind();
                                            return hyper::server::Server::builder(
                                                SingleAccept::new(stream),
                                            )
                                            .serve(make_service_fn(|_| async {
                                                Ok::<_, Infallible>(service_fn(|req| async move {
                                                    let host = req
                                                        .headers()
                                                        .get(http::header::HOST)
                                                        .and_then(|host| host.to_str().ok());
                                                    let uri = Uri::from_parts({
                                                        let mut parts =
                                                            req.uri().to_owned().into_parts();
                                                        parts.authority = host
                                                            .map(FromStr::from_str)
                                                            .transpose()?;
                                                        parts
                                                    })?;
                                                    Response::builder()
                                                        .status(
                                                            http::StatusCode::TEMPORARY_REDIRECT,
                                                        )
                                                        .header(
                                                            http::header::LOCATION,
                                                            uri.to_string(),
                                                        )
                                                        .body(Body::default())
                                                }))
                                            }))
                                            .await
                                            .with_kind(crate::ErrorKind::Network);
                                        }
                                    };
                                    let target_name =
                                        mid.client_hello().server_name().map(|s| s.to_owned());
                                    let target = {
                                        let mapping = mapping.read().await;
                                        mapping
                                            .get(&target_name)
                                            .into_iter()
                                            .flatten()
                                            .find(|(_, rc)| rc.strong_count() > 0)
                                            .or_else(|| {
                                                if target_name
                                                    .map(|s| s.parse::<IpAddr>().is_ok())
                                                    .unwrap_or(true)
                                                {
                                                    mapping
                                                        .get(&None)
                                                        .into_iter()
                                                        .flatten()
                                                        .find(|(_, rc)| rc.strong_count() > 0)
                                                } else {
                                                    None
                                                }
                                            })
                                            .map(|(target, _)| target.clone())
                                    };
                                    if let Some(target) = target {
                                        let mut tcp_stream =
                                            TcpStream::connect(target.addr).await?;
                                        let key =
                                            ssl.with_certs(target.key, target.addr.ip()).await?;
                                        let cfg = ServerConfig::builder()
                                            .with_safe_defaults()
                                            .with_no_client_auth();
                                        let mut cfg =
                                            if mid.client_hello().signature_schemes().contains(
                                                &tokio_rustls::rustls::SignatureScheme::ED25519,
                                            ) {
                                                cfg.with_single_cert(
                                                    key.fullchain_ed25519()
                                                        .into_iter()
                                                        .map(|c| {
                                                            Ok(tokio_rustls::rustls::Certificate(
                                                                c.to_der()?,
                                                            ))
                                                        })
                                                        .collect::<Result<_, Error>>()?,
                                                    tokio_rustls::rustls::PrivateKey(
                                                        key.key()
                                                            .openssl_key_ed25519()
                                                            .private_key_to_der()?,
                                                    ),
                                                )
                                            } else {
                                                cfg.with_single_cert(
                                                    key.fullchain_nistp256()
                                                        .into_iter()
                                                        .map(|c| {
                                                            Ok(tokio_rustls::rustls::Certificate(
                                                                c.to_der()?,
                                                            ))
                                                        })
                                                        .collect::<Result<_, Error>>()?,
                                                    tokio_rustls::rustls::PrivateKey(
                                                        key.key()
                                                            .openssl_key_nistp256()
                                                            .private_key_to_der()?,
                                                    ),
                                                )
                                            }
                                            .with_kind(crate::ErrorKind::OpenSsl)?;
                                        match target.connect_ssl {
                                            Ok(()) => {
                                                let mut client_cfg =
                                                    tokio_rustls::rustls::ClientConfig::builder()
                                                        .with_safe_defaults()
                                                        .with_root_certificates({
                                                            let mut store = RootCertStore::empty();
                                                            store.add(
                                                        &tokio_rustls::rustls::Certificate(
                                                            key.root_ca().to_der()?,
                                                        ),
                                                    ).with_kind(crate::ErrorKind::OpenSsl)?;
                                                            store
                                                        })
                                                        .with_no_client_auth();
                                                client_cfg.alpn_protocols = mid
                                                    .client_hello()
                                                    .alpn()
                                                    .into_iter()
                                                    .flatten()
                                                    .map(|x| x.to_vec())
                                                    .collect();
                                                let mut target_stream =
                                                    TlsConnector::from(Arc::new(client_cfg))
                                                        .connect_with(
                                                            key.key()
                                                                .internal_address()
                                                                .as_str()
                                                                .try_into()
                                                                .with_kind(
                                                                    crate::ErrorKind::OpenSsl,
                                                                )?,
                                                            tcp_stream,
                                                            |conn| {
                                                                cfg.alpn_protocols.extend(
                                                                    conn.alpn_protocol()
                                                                        .into_iter()
                                                                        .map(|p| p.to_vec()),
                                                                )
                                                            },
                                                        )
                                                        .await
                                                        .with_kind(crate::ErrorKind::OpenSsl)?;
                                                let mut tls_stream =
                                                    mid.into_stream(Arc::new(cfg)).await?;
                                                tls_stream.get_mut().0.stop_buffering();
                                                tokio::io::copy_bidirectional(
                                                    &mut tls_stream,
                                                    &mut target_stream,
                                                )
                                                .await
                                            }
                                            Err(AlpnInfo::Reflect) => {
                                                for proto in
                                                    mid.client_hello().alpn().into_iter().flatten()
                                                {
                                                    cfg.alpn_protocols.push(proto.into());
                                                }
                                                let mut tls_stream =
                                                    mid.into_stream(Arc::new(cfg)).await?;
                                                tls_stream.get_mut().0.stop_buffering();
                                                tokio::io::copy_bidirectional(
                                                    &mut tls_stream,
                                                    &mut tcp_stream,
                                                )
                                                .await
                                            }
                                            Err(AlpnInfo::Specified(alpn)) => {
                                                cfg.alpn_protocols = alpn;
                                                let mut tls_stream =
                                                    mid.into_stream(Arc::new(cfg)).await?;
                                                tls_stream.get_mut().0.stop_buffering();
                                                tokio::io::copy_bidirectional(
                                                    &mut tls_stream,
                                                    &mut tcp_stream,
                                                )
                                                .await
                                            }
                                        }
                                        .map_or_else(
                                            |e| match e.kind() {
                                                std::io::ErrorKind::UnexpectedEof => Ok(()),
                                                _ => Err(e),
                                            },
                                            |_| Ok(()),
                                        )?;
                                    } else {
                                        // 503
                                    }
                                    Ok::<_, Error>(())
                                }
                                .await
                                {
                                    tracing::error!("Error in VHostController on port {port}: {e}");
                                    tracing::debug!("{e:?}")
                                }
                            });
                        }
                        Err(e) => {
                            tracing::error!("Error in VHostController on port {port}: {e}");
                            tracing::debug!("{e:?}");
                        }
                    }
                }
            })
            .into(),
        })
    }
    async fn add(&self, hostname: Option<String>, target: TargetInfo) -> Result<Arc<()>, Error> {
        if let Some(mapping) = Weak::upgrade(&self.mapping) {
            let mut writable = mapping.write().await;
            let mut targets = writable.remove(&hostname).unwrap_or_default();
            let rc = if let Some(rc) = Weak::upgrade(&targets.remove(&target).unwrap_or_default()) {
                rc
            } else {
                Arc::new(())
            };
            targets.insert(target, Arc::downgrade(&rc));
            writable.insert(hostname, targets);
            Ok(rc)
        } else {
            Err(Error::new(
                eyre!("VHost Service Thread has exited"),
                crate::ErrorKind::Network,
            ))
        }
    }
    async fn gc(&self, hostname: Option<String>) -> Result<(), Error> {
        if let Some(mapping) = Weak::upgrade(&self.mapping) {
            let mut writable = mapping.write().await;
            let mut targets = writable.remove(&hostname).unwrap_or_default();
            targets = targets
                .into_iter()
                .filter(|(_, rc)| rc.strong_count() > 0)
                .collect();
            if !targets.is_empty() {
                writable.insert(hostname, targets);
            }
            Ok(())
        } else {
            Err(Error::new(
                eyre!("VHost Service Thread has exited"),
                crate::ErrorKind::Network,
            ))
        }
    }
    async fn is_empty(&self) -> Result<bool, Error> {
        if let Some(mapping) = Weak::upgrade(&self.mapping) {
            Ok(mapping.read().await.is_empty())
        } else {
            Err(Error::new(
                eyre!("VHost Service Thread has exited"),
                crate::ErrorKind::Network,
            ))
        }
    }
}
