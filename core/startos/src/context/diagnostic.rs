use std::ops::Deref;
use std::path::PathBuf;
use std::sync::Arc;

use rpc_toolkit::yajrc::RpcError;
use rpc_toolkit::Context;
use tokio::sync::broadcast::Sender;
use tracing::instrument;

use crate::context::config::ServerConfig;
use crate::rpc_continuations::RpcContinuations;
use crate::shutdown::Shutdown;
use crate::Error;

pub struct DiagnosticContextSeed {
    pub datadir: PathBuf,
    pub shutdown: Sender<Option<Shutdown>>,
    pub error: Arc<RpcError>,
    pub disk_guid: Option<Arc<String>>,
    pub rpc_continuations: RpcContinuations,
}

#[derive(Clone)]
pub struct DiagnosticContext(Arc<DiagnosticContextSeed>);
impl DiagnosticContext {
    #[instrument(skip_all)]
    pub fn init(
        config: &ServerConfig,
        disk_guid: Option<Arc<String>>,
        error: Error,
    ) -> Result<Self, Error> {
        tracing::error!("Error: {}: Starting diagnostic UI", error);
        tracing::debug!("{:?}", error);

        let (shutdown, _) = tokio::sync::broadcast::channel(1);

        Ok(Self(Arc::new(DiagnosticContextSeed {
            datadir: config.datadir().to_owned(),
            shutdown,
            disk_guid,
            error: Arc::new(error.into()),
            rpc_continuations: RpcContinuations::new(),
        })))
    }
}
impl AsRef<RpcContinuations> for DiagnosticContext {
    fn as_ref(&self) -> &RpcContinuations {
        &self.rpc_continuations
    }
}
impl Context for DiagnosticContext {}
impl Deref for DiagnosticContext {
    type Target = DiagnosticContextSeed;
    fn deref(&self) -> &Self::Target {
        &*self.0
    }
}
