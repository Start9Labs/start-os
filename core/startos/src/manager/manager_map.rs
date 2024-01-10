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
use crate::service::start_stop::StartStop;
use crate::service::Service;

/// This is the structure to contain all the services
#[derive(Default)]
pub struct ServiceMap(RwLock<BTreeMap<PackageId, Arc<Service>>>);
impl ServiceMap {
    #[instrument(skip_all)]
    pub async fn init(&self, ctx: &RpcContext) -> Result<(), Error> {
        let mut res = BTreeMap::new();
        let s9pk_dir = ctx.datadir.join(PKG_ARCHIVE_DIR).join("installed");
        for (id, entry) in ctx.db.peek().await.as_package_data().as_entries()? {
            if let Some(i) = entry.as_installed() {
                match S9pk::open(s9pk_dir.join(&id).with_extension("s9pk")).await {
                    Ok(s9pk) => {
                        res.insert(
                            s9pk.as_manifest().id.clone(),
                            Arc::new(
                                Service::new(
                                    ctx.clone(),
                                    s9pk,
                                    if i.as_status().as_main().de()?.running() {
                                        StartStop::Start
                                    } else {
                                        StartStop::Stop
                                    },
                                )
                                .await?,
                            ),
                        );
                    }
                    Err(e) => {
                        tracing::error!("Error loading installed package as service: {e}");
                        tracing::debug!("{e:?}");
                        // TODO: add package broken state
                    }
                }
            }
        }
        *self.0.write().await = res;
        Ok(())
    }

    #[instrument(skip_all)]
    pub async fn install(&self, ctx: &RpcContext, s9pk: S9pk) -> Result<Arc<Service>, Error> {
        let mut lock = self.0.write().await;
        let id = s9pk.as_manifest().id.clone();
        if let Some(service) = lock.get(&id).cloned() {
            drop(lock);
            service.upgrade(s9pk).await?;
            Ok(service)
        } else {
            let service = Arc::new(Service::new(ctx.clone(), s9pk, StartStop::Stop).await?);
            lock.insert(id, service.clone());
            drop(lock);
            service.install().await?;
            Ok(service)
        }
    }

    /// This is ran during the cleanup, so when we are uninstalling the service
    #[instrument(skip_all)]
    pub async fn uninstall(&self, id: &PackageId) -> Result<(), Error> {
        if let Some(service) = self.0.write().await.remove(id) {
            service.uninstall().await?;
        }
        Ok(())
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
    pub async fn get(&self, id: &PackageId) -> Option<Arc<Service>> {
        self.0.read().await.get(id).cloned()
    }
}
