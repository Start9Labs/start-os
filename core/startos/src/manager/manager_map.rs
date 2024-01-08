use std::collections::BTreeMap;
use std::sync::Arc;

use color_eyre::eyre::eyre;
use tokio::sync::RwLock;
use tracing::instrument;

use crate::context::RpcContext;
use crate::install::PKG_ARCHIVE_DIR;
use crate::prelude::*;
use crate::s9pk::manifest::PackageId;
use crate::s9pk::S9pk;

/// This is the structure to contain all the service managers
#[derive(Default)]
pub struct ManagerMap(RwLock<BTreeMap<PackageId, Arc<Manager>>>);
impl ManagerMap {
    #[instrument(skip_all)]
    pub async fn init(&self, ctx: &RpcContext) -> Result<(), Error> {
        let mut res = BTreeMap::new();
        let mut dir =
            tokio::fs::read_dir(ctx.datadir.join(PKG_ARCHIVE_DIR).join("installed")).await?;
        while let Some(file) = dir.next_entry().await? {
            match S9pk::open(file.path()).await {
                Ok(s9pk) => {
                    res.insert(
                        s9pk.as_manifest().id.clone(),
                        Arc::new(Manager::new(ctx.clone(), s9pk).await?),
                    );
                }
                Err(e) => {
                    tracing::error!("Error loading installed package as service: {e}");
                    tracing::debug!("{e:?}");
                }
            }
        }
        *self.0.write().await = res;
        Ok(())
    }

    #[instrument(skip_all)]
    pub async fn install(&self, ctx: &RpcContext, s9pk: S9pk) -> Result<Arc<Manager>, Error> {
        let mut lock = self.0.write().await;
        let id = s9pk.as_manifest().id.clone();
        if let Some(manager) = lock.remove(&id) {
            manager.upgrade(s9pk).await?;
            Ok(manager)
        } else {
            let manager = Arc::new(Manager::new(ctx.clone(), s9pk).await?);
            lock.insert(id, manager.clone());
            Ok(manager)
        }
    }

    /// This is ran during the cleanup, so when we are uninstalling the service
    #[instrument(skip_all)]
    pub async fn remove(&self, id: &PackageId) {
        if let Some(man) = self.0.write().await.remove(id) {
            man.exit().await;
        }
    }

    /// Used during a shutdown
    #[instrument(skip_all)]
    pub async fn empty(&self) -> Result<(), Error> {
        let res =
            futures::future::join_all(std::mem::take(&mut *self.0.write().await).into_iter().map(
                |(id, man)| async move {
                    tracing::debug!("Manager for {id} shutting down");
                    man.shutdown().await?;
                    tracing::debug!("Manager for {id} is shutdown");
                    if let Err(e) = Arc::try_unwrap(man) {
                        tracing::trace!(
                            "Manager for {} still has {} other open references",
                            id,
                            Arc::strong_count(&e) - 1
                        );
                    }
                    Ok::<_, Error>(())
                },
            ))
            .await;
        res.into_iter().fold(Ok(()), |res, x| match (res, x) {
            (Ok(()), x) => x,
            (Err(e), Ok(())) => Err(e),
            (Err(e1), Err(e2)) => Err(Error::new(eyre!("{}, {}", e1.source, e2.source), e1.kind)),
        })
    }

    #[instrument(skip_all)]
    pub async fn get(&self, id: &PackageId) -> Option<Arc<Manager>> {
        self.0.read().await.get(id).cloned()
    }
}
