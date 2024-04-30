use std::ops::Deref;
use std::path::Path;
use std::sync::Arc;

use patch_db::PatchDb;
use rpc_toolkit::Context;
use tokio::sync::broadcast::Sender;
use tracing::instrument;

use crate::context::config::ServerConfig;
use crate::prelude::*;
use crate::registry::server::RegistryDatabase;
use crate::rpc_continuations::RpcContinuations;

pub struct RegistryContextSeed {
    pub db: TypedPatchDb<RegistryDatabase>,
    pub rpc_continuations: RpcContinuations,
    pub shutdown: Sender<()>,
}

#[derive(Clone)]
pub struct RegistryContext(Arc<RegistryContextSeed>);
impl RegistryContext {
    #[instrument(skip_all)]
    pub async fn init(config: &ServerConfig) -> Result<Self, Error> {
        let (shutdown, _) = tokio::sync::broadcast::channel(1);
        let db_path = config.registry_db.as_deref().unwrap_or_else(|| {
            config
                .datadir
                .as_deref()
                .unwrap_or_else(|| Path::new("registry.db"))
        });
        let db = TypedPatchDb::<RegistryDatabase>::load_or_init(
            PatchDb::open(&db_path).await?,
            || async { Ok(Default::default()) },
        )
        .await?;
        Ok(Self(Arc::new(RegistryContextSeed {
            db,
            rpc_continuations: RpcContinuations::new(),
            shutdown,
        })))
    }
}

impl Context for RegistryContext {}
impl Deref for RegistryContext {
    type Target = RegistryContextSeed;
    fn deref(&self) -> &Self::Target {
        &*self.0
    }
}
