use std::collections::BTreeMap;
use std::sync::Arc;

use reqwest::Url;
use tokio::sync::RwLock;
use tracing::instrument;

use super::Manager;
use crate::context::RpcContext;
use crate::prelude::*;
use crate::s9pk::manifest::{Manifest, PackageId};

#[derive(Default)]
pub struct ManagerMap(RwLock<BTreeMap<PackageId, Arc<Manager>>>);
impl ManagerMap {
    #[instrument(skip(self, ctx))]
    pub async fn init(&self, ctx: &RpcContext) -> Result<(), Error> {
        let mut res = BTreeMap::new();
        for item_res in ctx
            .db
            .peek()
            .await?
            .into_package_data()
            .into_entries()?
            .into_iter()
            .map(|(id, value)| {
                let installed = value.expect_into_installed()?;
                Ok::<_, Error>((
                    id,
                    installed.as_manifest().clone().de()?,
                    installed.as_installed().as_marketplace_url().clone().de()?,
                ))
            })
        {
            let (id, manifest, marketplace_url) = item_res?;
            res.insert(
                id,
                Arc::new(Manager::new(ctx.clone(), manifest, marketplace_url).await?),
            );
        }
        *self.0.write().await = res;
        Ok(())
    }

    #[instrument(skip(self, ctx))]
    pub async fn add(
        &self,
        ctx: RpcContext,
        manifest: Manifest,
        marketplace_url: Option<Url>,
    ) -> Result<(), Error> {
        let mut lock = self.0.write().await;
        if let Some(man) = lock.remove(&manifest.id) {
            man.exit().await;
        }
        lock.insert(
            manifest.id.clone(),
            Arc::new(Manager::new(ctx, manifest, marketplace_url).await?),
        );
        Ok(())
    }

    #[instrument(skip(self))]
    pub async fn remove(&self, id: &PackageId) -> Option<Arc<Manager>> {
        self.0.write().await.remove(id)
    }

    #[instrument(skip(self))]
    pub async fn empty(&self) -> Result<(), Error> {
        let res =
            futures::future::join_all(std::mem::take(&mut *self.0.write().await).into_iter().map(
                |(id, man)| async move {
                    tracing::debug!("Manager for {id} shutting down");
                    man.exit().await;
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

    #[instrument(skip(self))]
    pub async fn get(&self, id: &PackageId) -> Option<Arc<Manager>> {
        self.0.read().await.get(id).cloned()
    }
}
