use std::collections::BTreeMap;
use std::net::SocketAddr;
use std::sync::Arc;

use helpers::NonDetachingJoinHandle;
use models::ResultExt;
use sqlx::PgPool;
use tokio::net::TcpListener;
use tokio::sync::RwLock;
use tokio_rustls::rustls::server::Acceptor;
use tokio_rustls::{LazyConfigAcceptor, TlsAcceptor};

use crate::net::net_utils::ResourceFqdn;
use crate::net::ssl::SslManager;
use crate::Error;

// not allowed: <=1024, >=32768, 5355, 5432, 9050, 6010, 9051, 5353

pub struct VHostController {
    servers: BTreeMap<u16, VHostServer>,
}

struct VHostServer {
    mapping: Arc<RwLock<BTreeMap<ResourceFqdn, (SocketAddr, bool)>>>,
    _thread: NonDetachingJoinHandle<()>,
}
impl VHostServer {
    async fn new(pool: PgPool, port: u16) -> Result<Self, Error> {
        // check if port allowed
        // let ssl = SslManager::init(pool, handle)
        let mut listener = TcpListener::bind(SocketAddr::new([0, 0, 0, 0].into(), port))
            .await
            .with_kind(crate::ErrorKind::Network)?;
        Ok(Self {
            mapping: Default::default(),
            _thread: tokio::spawn(async move {
                loop {
                    match listener.accept().await {
                        Ok((stream, _)) => {
                            tokio::spawn(async move {
                                if let Err(e) = async {
                                    let mid = LazyConfigAcceptor::new(Acceptor::default(), stream)
                                        .await?;
                                    let target = mid
                                        .client_hello()
                                        .server_name()
                                        .map(|s| s.parse())
                                        .transpose()?
                                        .unwrap_or(ResourceFqdn::IpAddr);

                                    Ok(())
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
                    if let Err(e) = async {
                        let (stream, _) = listener.accept().await?;
                    }
                    .await
                    {
                        tracing::error!("Error in VHostController on port {port}: {e}");
                        tracing::debug!("{e:?}")
                    }
                }
            })
            .into(),
        })
    }
}
