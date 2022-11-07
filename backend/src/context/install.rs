use std::net::{IpAddr, SocketAddr};
use std::ops::Deref;
use std::path::Path;
use std::sync::Arc;

use rpc_toolkit::Context;
use serde::Deserialize;
use tokio::sync::broadcast::Sender;
use tracing::instrument;
use url::Host;

use crate::util::config::load_config_from_paths;
use crate::Error;

#[derive(Debug, Default, Deserialize)]
#[serde(rename_all = "kebab-case")]
pub struct InstallContextConfig {
    pub bind_rpc: Option<SocketAddr>,
}
impl InstallContextConfig {
    #[instrument(skip(path))]
    pub async fn load<P: AsRef<Path> + Send + 'static>(path: Option<P>) -> Result<Self, Error> {
        tokio::task::spawn_blocking(move || {
            load_config_from_paths(
                path.as_ref()
                    .into_iter()
                    .map(|p| p.as_ref())
                    .chain(std::iter::once(Path::new(
                        "/media/embassy/config/config.yaml",
                    )))
                    .chain(std::iter::once(Path::new(crate::util::config::CONFIG_PATH))),
            )
        })
        .await
        .unwrap()
    }
}

pub struct InstallContextSeed {
    pub bind_rpc: SocketAddr,
    pub shutdown: Sender<()>,
}

#[derive(Clone)]
pub struct InstallContext(Arc<InstallContextSeed>);
impl InstallContext {
    #[instrument(skip(path))]
    pub async fn init<P: AsRef<Path> + Send + 'static>(path: Option<P>) -> Result<Self, Error> {
        let cfg = InstallContextConfig::load(path.as_ref().map(|p| p.as_ref().to_owned())).await?;
        let (shutdown, _) = tokio::sync::broadcast::channel(1);
        Ok(Self(Arc::new(InstallContextSeed {
            bind_rpc: cfg.bind_rpc.unwrap_or(([127, 0, 0, 1], 5959).into()),
            shutdown,
        })))
    }
}

impl Context for InstallContext {
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
impl Deref for InstallContext {
    type Target = InstallContextSeed;
    fn deref(&self) -> &Self::Target {
        &*self.0
    }
}
