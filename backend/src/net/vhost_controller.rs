use std::collections::BTreeMap;
use std::net::SocketAddr;
use std::sync::Arc;

use helpers::NonDetachingJoinHandle;
use models::{InterfaceId, PackageId, ResultExt};
use sqlx::PgPool;
use tokio::net::{TcpListener, TcpStream};
use tokio::sync::{oneshot, RwLock};
use tokio_rustls::rustls::server::Acceptor;
use tokio_rustls::rustls::ServerConfig;
use tokio_rustls::LazyConfigAcceptor;

use crate::net::keys::KeyInfo;
use crate::net::net_utils::ResourceFqdn;
use crate::Error;

// not allowed: <=1024, >=32768, 5355, 5432, 9050, 6010, 9051, 5353

pub struct VHostController {
    servers: RwLock<BTreeMap<u16, VHostServer>>,
}
impl VHostController {
    pub fn new() -> Self {
        Self {
            servers: RwLock::new(BTreeMap::new()),
        }
    }
    pub async fn add(
        &self,
        hostname: &ResourceFqdn,
        port: u16,
        target: TargetInfo,
    ) -> Result<(), Error> {
        let mut writable = self.servers.write().await;
        let server = if let Some(server) = writable.remove(&port) {
            server
        } else {
            VHostServer::new(port).await?
        };
        server.add(hostname, target).await;
        writable.insert(port, server);
        Ok(())
    }
    pub async fn remove(hostname: &ResourceFqdn, port: u16, target: &TargetInfo) {}
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord)]
pub struct TargetInfo {
    pub addr: SocketAddr,
    pub serves_ssl: bool,
    pub interface: Option<(PackageId, InterfaceId)>,
    pub key_info: KeyInfo,
}

struct VHostServer {
    mapping: Arc<RwLock<BTreeMap<ResourceFqdn, Vec<TargetInfo>>>>,
    _thread: NonDetachingJoinHandle<()>,
}
impl VHostServer {
    async fn new(port: u16) -> Result<Self, Error> {
        // check if port allowed
        let mut listener = TcpListener::bind(SocketAddr::new([0, 0, 0, 0].into(), port))
            .await
            .with_kind(crate::ErrorKind::Network)?;
        let mapping = Arc::new(RwLock::new(BTreeMap::new()));
        Ok(Self {
            mapping: mapping.clone(),
            _thread: tokio::spawn(async move {
                loop {
                    match listener.accept().await {
                        Ok((stream, _)) => {
                            let mapping = mapping.clone();
                            tokio::spawn(async move {
                                if let Err(e) = async {
                                    let mid = LazyConfigAcceptor::new(Acceptor::default(), stream)
                                        .await?;
                                    let target_name = mid
                                        .client_hello()
                                        .server_name()
                                        .map(|s| s.parse())
                                        .transpose()?
                                        .unwrap_or(ResourceFqdn::IpAddr);
                                    if let Some(target) = mapping
                                        .read()
                                        .await
                                        .get(&target_name)
                                        .and_then(|t| t.get(0))
                                        .cloned()
                                    {
                                        let mut tcp_stream =
                                            TcpStream::connect(target.addr).await?;
                                        let mut tls_stream = mid
                                            .into_stream(Arc::new(
                                                ServerConfig::builder()
                                                    .with_safe_default_cipher_suites()
                                                    .with_safe_default_kx_groups()
                                                    .with_safe_default_protocol_versions()
                                                    .with_kind(crate::ErrorKind::OpenSsl)?
                                                    .with_no_client_auth()
                                                    .with_single_cert(
                                                        target.key_info.fullchain().into_iter().map(|c| {
                                                            Ok(tokio_rustls::rustls::Certificate(
                                                                c.to_der()?,
                                                            ))
                                                        }).collect::<Result<_, Error>>()?,
                                                        tokio_rustls::rustls::PrivateKey(
                                                            target.key_info
                                                                .key()
                                                                .openssl_key()
                                                                .private_key_to_der()?,
                                                        ),
                                                    )
                                                    .with_kind(crate::ErrorKind::OpenSsl)?,
                                            ))
                                            .await?;
                                        tokio::io::copy_bidirectional(
                                            &mut tls_stream,
                                            &mut tcp_stream,
                                        )
                                        .await?;
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
    async fn add(&self, hostname: &ResourceFqdn, target: TargetInfo) {
        let mut writable = self.mapping.write().await;
        let mut targets = writable.remove(hostname).unwrap_or_default();
        targets.push(target);
        writable.insert(hostname.clone(), targets);
    }
    async fn remove(&self, hostname: &ResourceFqdn, target: &TargetInfo) {
        let mut writable = self.mapping.write().await;
        let mut targets = writable.remove(hostname).unwrap_or_default();
        if let Some((idx, _)) = targets.iter().enumerate().find(|(_, x)| *x == target) {
            targets.swap_remove(idx);
        }
        if !targets.is_empty() {
            writable.insert(hostname.clone(), targets);
        }
    }
    async fn is_empty(&self) -> bool {
        self.mapping.read().await.is_empty()
    }
}
