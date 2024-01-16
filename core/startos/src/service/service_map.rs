use std::collections::BTreeMap;
use std::sync::Arc;

use color_eyre::eyre::eyre;
use tokio::fs::File;
use tokio::sync::RwLock;
use tracing::instrument;

use crate::context::RpcContext;
use crate::install::progress::{InstallProgress, InstallProgressTracker};
use crate::install::PKG_ARCHIVE_DIR;
use crate::prelude::*;
use crate::s9pk::manifest::PackageId;
use crate::s9pk::merkle_archive::source::FileSource;
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
                if let Err(e) = async {
                    let s9pk = S9pk::open(s9pk_dir.join(&id).with_extension("s9pk")).await?;
                    let id = s9pk.as_manifest().id.clone();
                    if let Some(service) = Service::load(ctx.clone(), s9pk, entry).await? {
                        res.insert(id, Arc::new(service));
                    }
                    Ok::<_, Error>(())
                }
                .await
                {
                    tracing::error!("Error loading installed package as service: {e}");
                    tracing::debug!("{e:?}");
                }
            }
        }
        *self.0.write().await = res;
        Ok(())
    }

    #[instrument(skip_all)]
    pub async fn install<S: FileSource>(
        &self,
        ctx: &RpcContext,
        s9pk: S9pk<S>,
    ) -> Result<Arc<Service>, Error> {
        let id = s9pk.as_manifest().id.clone();
        let download_path = ctx
            .datadir
            .join(PKG_ARCHIVE_DIR)
            .join("downloading")
            .join(&id)
            .with_extension("s9pk");

        let progress = Arc::new(InstallProgress::new(s9pk.size()));
        progress
            .track_download_during(ctx.db.clone(), &id, || async {
                let mut progress_writer = InstallProgressTracker::new(
                    crate::util::io::create_file(&download_path).await?,
                    progress.clone(),
                );
                s9pk.serialize(&mut progress_writer, true).await?;
                progress_writer.into_inner().sync_all().await?;
                progress.download_complete();
                Ok(())
            })
            .await?;

        let installed_path = ctx
            .datadir
            .join(PKG_ARCHIVE_DIR)
            .join("installed")
            .join(&id)
            .with_extension("s9pk");

        crate::util::io::rename(&download_path, &installed_path).await?;

        let s9pk = S9pk::open(&installed_path).await?;

        let mut lock = self.0.write().await;
        if let Some(service) = lock.get(&id).cloned() {
            drop(lock);
            service.update(s9pk).await?;
            Ok(service)
        } else {
            let service = Arc::new(
                Service::load(
                    ctx.clone(),
                    s9pk,
                    ctx.db
                        .peek()
                        .await
                        .as_package_data()
                        .as_idx(&id)
                        .or_not_found(&id)?,
                )
                .await?.ok_or_else(|| Error::new(eyre!("PackageDataEntry must not be in `removing` or `restoring` state in db for Service to be `load`ed"), ErrorKind::Incoherent))?,
            );
            lock.insert(id, service.clone());
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
