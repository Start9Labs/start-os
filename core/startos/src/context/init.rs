use std::ops::Deref;
use std::sync::Arc;

use rpc_toolkit::Context;
use tokio::sync::broadcast::Sender;
use tracing::instrument;

use crate::progress::FullProgressTracker;
use crate::rpc_continuations::RpcContinuations;
use crate::Error;

pub struct InitContextSeed {
    pub progress: FullProgressTracker,
    pub shutdown: Sender<()>,
    pub rpc_continuations: RpcContinuations,
}

#[derive(Clone)]
pub struct InitContext(Arc<InitContextSeed>);
impl InitContext {
    #[instrument(skip_all)]
    pub async fn init() -> Result<Self, Error> {
        let (shutdown, _) = tokio::sync::broadcast::channel(1);
        Ok(Self(Arc::new(InitContextSeed {
            progress: FullProgressTracker::new(),
            shutdown,
            rpc_continuations: RpcContinuations::new(),
        })))
    }
}

impl AsRef<RpcContinuations> for InitContext {
    fn as_ref(&self) -> &RpcContinuations {
        &self.rpc_continuations
    }
}

impl Context for InitContext {}
impl Deref for InitContext {
    type Target = InitContextSeed;
    fn deref(&self) -> &Self::Target {
        &*self.0
    }
}
