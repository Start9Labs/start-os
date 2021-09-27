use std::net::{IpAddr, SocketAddr};
use std::ops::Deref;
use std::path::Path;
use std::sync::Arc;

use rpc_toolkit::yajrc::RpcError;
use rpc_toolkit::Context;
use serde::Deserialize;
use tokio::fs::File;
use tokio::sync::broadcast::Sender;
use url::Host;

use crate::shutdown::Shutdown;
use crate::util::io::from_toml_async_reader;
use crate::util::AsyncFileExt;
use crate::{Error, ResultExt};

#[derive(Debug, Default, Deserialize)]
#[serde(rename_all = "kebab-case")]
pub struct DiagnosticContextConfig {
    pub bind_rpc: Option<SocketAddr>,
    pub zfs_pool_name: Option<String>,
}
impl DiagnosticContextConfig {
    pub async fn load<P: AsRef<Path>>(path: Option<P>) -> Result<Self, Error> {
        let cfg_path = path
            .as_ref()
            .map(|p| p.as_ref())
            .unwrap_or(Path::new(crate::CONFIG_PATH));
        if let Some(f) = File::maybe_open(cfg_path)
            .await
            .with_ctx(|_| (crate::ErrorKind::Filesystem, cfg_path.display().to_string()))?
        {
            from_toml_async_reader(f).await
        } else {
            Ok(Self::default())
        }
    }
    pub fn zfs_pool_name(&self) -> &str {
        self.zfs_pool_name
            .as_ref()
            .map(|s| s.as_str())
            .unwrap_or("embassy-data")
    }
}

pub struct DiagnosticContextSeed {
    pub bind_rpc: SocketAddr,
    pub shutdown: Sender<Option<Shutdown>>,
    pub error: Arc<RpcError>,
    pub zfs_pool_name: Arc<String>,
}

#[derive(Clone)]
pub struct DiagnosticContext(Arc<DiagnosticContextSeed>);
impl DiagnosticContext {
    pub async fn init<P: AsRef<Path>>(path: Option<P>, error: Error) -> Result<Self, Error> {
        let cfg = DiagnosticContextConfig::load(path).await?;

        let (shutdown, _) = tokio::sync::broadcast::channel(1);

        Ok(Self(Arc::new(DiagnosticContextSeed {
            bind_rpc: cfg.bind_rpc.unwrap_or(([127, 0, 0, 1], 5959).into()),
            shutdown,
            error: Arc::new(error.into()),
            zfs_pool_name: Arc::new(cfg.zfs_pool_name().to_owned()),
        })))
    }
}

impl Context for DiagnosticContext {
    fn host(&self) -> Host<&str> {
        match self.0.bind_rpc.ip() {
            IpAddr::V4(a) => Host::Ipv4(a),
            IpAddr::V6(a) => Host::Ipv6(a),
        }
    }
    fn port(&self) -> u16 {
        self.0.bind_rpc.port()
    }
}
impl Deref for DiagnosticContext {
    type Target = DiagnosticContextSeed;
    fn deref(&self) -> &Self::Target {
        &*self.0
    }
}
