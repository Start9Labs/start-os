use std::path::PathBuf;
use std::sync::Arc;

use color_eyre::eyre::eyre;
use futures::future::BoxFuture;
use futures::{Future, FutureExt};
use imbl::OrdMap;
use tokio::sync::{Mutex, OwnedRwLockReadGuard, OwnedRwLockWriteGuard, RwLock};
use tracing::instrument;

use crate::context::RpcContext;
use crate::db::model::{
    PackageDataEntry, PackageDataEntryInstalled, PackageDataEntryInstalling,
    PackageDataEntryRestoring, PackageDataEntryUpdating, StaticFiles,
};
use crate::disk::mount::guard::GenericMountGuard;
use crate::install::progress::{InstallProgress, InstallProgressTracker};
use crate::install::PKG_ARCHIVE_DIR;
use crate::notifications::NotificationLevel;
use crate::prelude::*;
use crate::s9pk::manifest::PackageId;
use crate::s9pk::merkle_archive::source::FileSource;
use crate::s9pk::S9pk;
use crate::service::Service;

pub type DownloadInstallFuture = BoxFuture<'static, Result<InstallFuture, Error>>;
pub type InstallFuture = BoxFuture<'static, Result<(), Error>>;

/// This is the structure to contain all the services
#[derive(Default)]
pub struct ServiceMap(Mutex<OrdMap<PackageId, Arc<RwLock<Option<Service>>>>>);
impl ServiceMap {
    async fn entry(&self, id: &PackageId) -> Arc<RwLock<Option<Service>>> {
        self.0
            .lock()
            .await
            .entry(id.clone())
            .or_insert_with(|| Arc::new(RwLock::new(None)))
            .clone()
    }

    #[instrument(skip_all)]
    pub async fn get(&self, id: &PackageId) -> OwnedRwLockReadGuard<Option<Service>> {
        self.entry(id).await.read_owned().await
    }

    #[instrument(skip_all)]
    pub async fn get_mut(&self, id: &PackageId) -> OwnedRwLockWriteGuard<Option<Service>> {
        self.entry(id).await.write_owned().await
    }

    #[instrument(skip_all)]
    pub async fn init(&self, ctx: &RpcContext) -> Result<(), Error> {
        for id in ctx.db.peek().await.as_package_data().keys()? {
            if let Err(e) = self.load(ctx, &id).await {
                tracing::error!("Error loading installed package as service: {e}");
                tracing::debug!("{e:?}");
            }
        }
        Ok(())
    }

    #[instrument(skip_all)]
    pub async fn load(&self, ctx: &RpcContext, id: &PackageId) -> Result<(), Error> {
        let mut shutdown_err = Ok(());
        let mut service = self.get_mut(id).await;
        if let Some(service) = service.take() {
            shutdown_err = service.shutdown().await;
        }
        // TODO: retry on error?
        *service = Service::load(ctx, id).await?;
        shutdown_err?;
        Ok(())
    }

    #[instrument(skip_all)]
    pub async fn install<S: FileSource>(
        &self,
        ctx: RpcContext,
        mut s9pk: S9pk<S>,
        recovery_source: Option<impl GenericMountGuard>,
    ) -> Result<DownloadInstallFuture, Error> {
        let manifest = Arc::new(s9pk.as_manifest().clone());
        let id = manifest.id.clone();
        let icon = s9pk.icon_data_url().await?;
        let mut service = self.get_mut(&id).await;

        let install_progress = Arc::new(InstallProgress::new(s9pk.size()));
        let restoring = recovery_source.is_some();

        let mut reload_guard = ServiceReloadGuard::new(
            ctx.clone(),
            id.clone(),
            if recovery_source.is_none() {
                if service.is_none() {
                    "Install"
                } else {
                    "Update"
                }
            } else {
                "Restore"
            },
        );

        reload_guard
            .handle(ctx.db.mutate({
                let manifest = manifest.clone();
                let id = id.clone();
                let install_progress = install_progress.clone();
                move |db| {
                    let pde = match db
                        .as_package_data()
                        .as_idx(&id)
                        .map(|x| x.de())
                        .transpose()?
                    {
                        Some(PackageDataEntry::Installed(PackageDataEntryInstalled {
                            installed,
                            static_files,
                            ..
                        })) => PackageDataEntry::Updating(PackageDataEntryUpdating {
                            install_progress,
                            installed,
                            manifest: (*manifest).clone(),
                            static_files,
                        }),
                        None if restoring => {
                            PackageDataEntry::Restoring(PackageDataEntryRestoring {
                                install_progress,
                                static_files: StaticFiles::local(
                                    &manifest.id,
                                    &manifest.version,
                                    icon,
                                ),
                                manifest: (*manifest).clone(),
                            })
                        }
                        None => PackageDataEntry::Installing(PackageDataEntryInstalling {
                            install_progress,
                            static_files: StaticFiles::local(&manifest.id, &manifest.version, icon),
                            manifest: (*manifest).clone(),
                        }),
                        _ => {
                            return Err(Error::new(
                                eyre!("Cannot install over a package in a transient state"),
                                crate::ErrorKind::InvalidRequest,
                            ))
                        }
                    };
                    db.as_package_data_mut().insert(&manifest.id, &pde)
                }
            }))
            .await?;

        Ok(async move {
            let installed_path = reload_guard
                .handle(async {
                    let download_path = ctx
                        .datadir
                        .join(PKG_ARCHIVE_DIR)
                        .join("downloading")
                        .join(&id)
                        .with_extension("s9pk");

                    install_progress
                        .track_download_during(ctx.db.clone(), &id, || async {
                            let mut progress_writer = InstallProgressTracker::new(
                                crate::util::io::create_file(&download_path).await?,
                                install_progress.clone(),
                            );
                            s9pk.serialize(&mut progress_writer, true).await?;
                            progress_writer.into_inner().sync_all().await?;
                            install_progress.download_complete();
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

                    Ok::<PathBuf, Error>(installed_path)
                })
                .await?;
            Ok(reload_guard
                .handle_last(async move {
                    let s9pk = S9pk::open(&installed_path, Some(&id)).await?;
                    let prev = if let Some(service) = service.take() {
                        ensure_code!(
                            recovery_source.is_none(),
                            ErrorKind::InvalidRequest,
                            "cannot restore over existing package"
                        );
                        let version = service
                            .seed
                            .persistent_container
                            .s9pk
                            .as_manifest()
                            .version
                            .clone();
                        service
                            .uninstall(Some(s9pk.as_manifest().version.clone()))
                            .await?;
                        Some(version)
                    } else {
                        None
                    };
                    if let Some(recovery_source) = recovery_source {
                        *service = Some(Service::restore(ctx, s9pk, recovery_source).await?);
                    } else {
                        *service = Some(Service::install(ctx, s9pk, prev).await?);
                    }
                    Ok(())
                })
                .boxed())
        }
        .boxed())
    }

    /// This is ran during the cleanup, so when we are uninstalling the service
    #[instrument(skip_all)]
    pub async fn uninstall(&self, ctx: &RpcContext, id: &PackageId) -> Result<(), Error> {
        if let Some(service) = self.get_mut(id).await.take() {
            ServiceReloadGuard::new(ctx.clone(), id.clone(), "Uninstall")
                .handle_last(service.uninstall(None))
                .await?;
        }
        Ok(())
    }

    pub async fn shutdown_all(&self) -> Result<(), Error> {
        let lock = self.0.lock().await;
        let mut futs = Vec::with_capacity(lock.len());
        for service in lock.values().cloned() {
            futs.push(async move {
                if let Some(service) = service.write_owned().await.take() {
                    service.shutdown().await?
                }
                Ok::<_, Error>(())
            });
        }
        drop(lock);
        let mut errors = ErrorCollection::new();
        for res in futures::future::join_all(futs).await {
            errors.handle(res);
        }
        errors.into_result()
    }
}

pub struct ServiceReloadGuard(Option<ServiceReloadInfo>);
impl Drop for ServiceReloadGuard {
    fn drop(&mut self) {
        if let Some(info) = self.0.take() {
            tokio::spawn(info.reload(None));
        }
    }
}
impl ServiceReloadGuard {
    pub fn new(ctx: RpcContext, id: PackageId, operation: &'static str) -> Self {
        Self(Some(ServiceReloadInfo { ctx, id, operation }))
    }
    pub async fn handle<T>(
        &mut self,
        operation: impl Future<Output = Result<T, Error>>,
    ) -> Result<T, Error> {
        let mut errors = ErrorCollection::new();
        match operation.await {
            Ok(a) => {
                self.0.take();
                Ok(a)
            }
            Err(e) => {
                if let Some(info) = self.0.take() {
                    errors.handle(info.reload(Some(e.clone_output())).await);
                }
                errors.handle::<(), _>(Err(e));
                errors.into_result().map(|_| unreachable!()) // TODO: there's gotta be a more elegant way?
            }
        }
    }
    pub async fn handle_last<T>(
        mut self,
        operation: impl Future<Output = Result<T, Error>>,
    ) -> Result<T, Error> {
        let res = self.handle(operation).await;
        self.0.take();
        res
    }
}

struct ServiceReloadInfo {
    ctx: RpcContext,
    id: PackageId,
    operation: &'static str,
}
impl ServiceReloadInfo {
    async fn reload(self, error: Option<Error>) -> Result<(), Error> {
        self.ctx.services.load(&self.ctx, &self.id).await?;
        if let Some(error) = error {
            self.ctx
                .notification_manager
                .notify(
                    self.ctx.db.clone(),
                    Some(self.id.clone()),
                    NotificationLevel::Error,
                    format!("{} Failed", self.operation),
                    error.to_string(),
                    (),
                    None,
                )
                .await?;
        }
        Ok(())
    }
}
