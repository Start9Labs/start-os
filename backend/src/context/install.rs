use std::ops::Deref;
use std::path::Path;
use std::sync::Arc;

use rpc_toolkit::Context;
use serde::Deserialize;
use tokio::sync::broadcast::Sender;
use tracing::instrument;

use crate::os_install::find_eth_iface;
use crate::util::config::load_config_from_paths;
use crate::Error;

#[derive(Debug, Default, Deserialize)]
#[serde(rename_all = "kebab-case")]
pub struct InstallContextConfig {}
impl InstallContextConfig {
    #[instrument(skip(path))]
    pub async fn load<P: AsRef<Path> + Send + 'static>(path: Option<P>) -> Result<Self, Error> {
        tokio::task::spawn_blocking(move || {
            load_config_from_paths(
                path.as_ref()
                    .into_iter()
                    .map(|p| p.as_ref())
                    .chain(std::iter::once(Path::new(crate::util::config::CONFIG_PATH))),
            )
        })
        .await
        .unwrap()
    }
}

pub struct InstallContextSeed {
    pub ethernet_interface: String,
    pub shutdown: Sender<()>,
}

#[derive(Clone)]
pub struct InstallContext(Arc<InstallContextSeed>);
impl InstallContext {
    #[instrument(skip(path))]
    pub async fn init<P: AsRef<Path> + Send + 'static>(path: Option<P>) -> Result<Self, Error> {
        let _cfg = InstallContextConfig::load(path.as_ref().map(|p| p.as_ref().to_owned())).await?;
        let (shutdown, _) = tokio::sync::broadcast::channel(1);
        Ok(Self(Arc::new(InstallContextSeed {
            ethernet_interface: find_eth_iface().await?,
            shutdown,
        })))
    }
}

impl Context for InstallContext {}
impl Deref for InstallContext {
    type Target = InstallContextSeed;
    fn deref(&self) -> &Self::Target {
        &*self.0
    }
}
