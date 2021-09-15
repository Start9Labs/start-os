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

use crate::util::io::from_toml_async_reader;
use crate::util::AsyncFileExt;
use crate::{Error, ResultExt};

#[derive(Debug, Default, Deserialize)]
#[serde(rename_all = "kebab-case")]
pub struct RecoveryContextConfig {
    pub bind_rpc: Option<SocketAddr>,
}
impl RecoveryContextConfig {
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
}

pub struct RecoveryContextSeed {
    pub bind_rpc: SocketAddr,
    pub shutdown: Sender<()>,
    pub error: Arc<RpcError>,
}

#[derive(Clone)]
pub struct RecoveryContext(Arc<RecoveryContextSeed>);
impl RecoveryContext {
    pub async fn init<P: AsRef<Path>>(path: Option<P>, error: Error) -> Result<Self, Error> {
        let cfg = RecoveryContextConfig::load(path).await?;

        let (shutdown, _) = tokio::sync::broadcast::channel(1);

        Ok(Self(Arc::new(RecoveryContextSeed {
            bind_rpc: cfg.bind_rpc.unwrap_or(([127, 0, 0, 1], 5959).into()),
            shutdown,
            error: Arc::new(error.into()),
        })))
    }
}

impl Context for RecoveryContext {
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
impl Deref for RecoveryContext {
    type Target = RecoveryContextSeed;
    fn deref(&self) -> &Self::Target {
        &*self.0
    }
}
