use std::ops::Deref;
use std::sync::Arc;

use rpc_toolkit::Context;
use tokio::sync::broadcast::Sender;
use tracing::instrument;

use crate::net::utils::find_eth_iface;
use crate::Error;

pub struct InstallContextSeed {
    pub ethernet_interface: String,
    pub shutdown: Sender<()>,
}

#[derive(Clone)]
pub struct InstallContext(Arc<InstallContextSeed>);
impl InstallContext {
    #[instrument(skip_all)]
    pub async fn init() -> Result<Self, Error> {
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
