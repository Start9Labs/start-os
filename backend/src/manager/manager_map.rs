use std::collections::BTreeMap;
use std::sync::Arc;

use color_eyre::eyre::eyre;
use sqlx::{Executor, Postgres};
use tokio::sync::RwLock;
use tracing::instrument;

use super::Manager;
use crate::context::RpcContext;
use crate::prelude::*;
use crate::s9pk::manifest::{Manifest, PackageId};
use crate::util::Version;
use crate::Error;

/// This is the structure to contain all the service managers
#[derive(Default)]
pub struct ManagerMap(RwLock<BTreeMap<(PackageId, Version), Arc<Manager>>>);
impl ManagerMap {
    #[instrument(skip_all)]
    pub async fn init<Ex>(
        &self,
        ctx: &RpcContext,
        peeked: &Peeked,
        secrets: &mut Ex,
    ) -> Result<(), Error>
    where
        for<'a> &'a mut Ex: Executor<'a, Database = Postgres>,
    {
        let mut res = BTreeMap::new();
        for package in peeked.as_package_data().keys()? {
            let man: Manifest = if let Some(manifest) = peeked
                .as_package_data()
                .as_idx(&package)
                .and_then(|x| x.as_installed())
                .map(|x| x.as_manifest().de())
            {
                manifest?
            } else {
                continue;
            };

            res.insert(
                (package, man.version.clone()),
                Arc::new(Manager::new(ctx.clone(), man).await?),
            );
        }
        *self.0.write().await = res;
        Ok(())
    }

    /// Used during the install process
    #[instrument(skip_all)]
    pub async fn add(&self, ctx: RpcContext, manifest: Manifest) -> Result<(), Error> {
        let mut lock = self.0.write().await;
        let id = (manifest.id.clone(), manifest.version.clone());
        if let Some(man) = lock.remove(&id) {
            man.exit().await;
        }
        lock.insert(id, Arc::new(Manager::new(ctx, manifest).await?));
        Ok(())
    }

    /// This is ran during the cleanup, so when we are uninstalling the service
    #[instrument(skip_all)]
    pub async fn remove(&self, id: &(PackageId, Version)) {
        if let Some(man) = self.0.write().await.remove(id) {
            man.exit().await;
        }
    }

    /// Used during a shutdown
    #[instrument(skip_all)]
    pub async fn empty(&self) -> Result<(), Error> {
        let res =
            futures::future::join_all(std::mem::take(&mut *self.0.write().await).into_iter().map(
                |((id, version), man)| async move {
                    tracing::debug!("Manager for {}@{} shutting down", id, version);
                    man.shutdown().await;
                    tracing::debug!("Manager for {}@{} is shutdown", id, version);
                    if let Err(e) = Arc::try_unwrap(man) {
                        tracing::trace!(
                            "Manager for {}@{} still has {} other open references",
                            id,
                            version,
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
    pub async fn get(&self, id: &(PackageId, Version)) -> Option<Arc<Manager>> {
        self.0.read().await.get(id).cloned()
    }
}
