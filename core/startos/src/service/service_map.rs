use std::sync::Arc;
use std::time::Duration;

use color_eyre::eyre::eyre;
use futures::future::BoxFuture;
use futures::{Future, FutureExt};
use helpers::NonDetachingJoinHandle;
use imbl::OrdMap;
use imbl_value::InternedString;
use tokio::sync::{Mutex, OwnedRwLockReadGuard, OwnedRwLockWriteGuard, RwLock};
use tracing::instrument;

use crate::context::RpcContext;
use crate::db::model::{
    InstalledPackageInfo, PackageDataEntry, PackageDataEntryInstalled, PackageDataEntryInstalling,
    PackageDataEntryRestoring, PackageDataEntryUpdating, StaticFiles,
};
use crate::disk::mount::guard::GenericMountGuard;
use crate::install::PKG_ARCHIVE_DIR;
use crate::notifications::NotificationLevel;
use crate::prelude::*;
use crate::progress::{
    FullProgressTracker, FullProgressTrackerHandle, PhaseProgressTrackerHandle,
    ProgressTrackerWriter,
};
use crate::s9pk::manifest::PackageId;
use crate::s9pk::merkle_archive::source::FileSource;
use crate::s9pk::S9pk;
use crate::service::{LoadDisposition, Service};

pub type DownloadInstallFuture = BoxFuture<'static, Result<InstallFuture, Error>>;
pub type InstallFuture = BoxFuture<'static, Result<(), Error>>;

pub(super) struct InstallProgressHandles {
    pub(super) finalization_progress: PhaseProgressTrackerHandle,
    pub(super) progress_handle: FullProgressTrackerHandle,
}

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
        for id in ctx.db.peek().await.as_public().as_package_data().keys()? {
            if let Err(e) = self.load(ctx, &id, LoadDisposition::Retry).await {
                tracing::error!("Error loading installed package as service: {e}");
                tracing::debug!("{e:?}");
            }
        }
        Ok(())
    }

    #[instrument(skip_all)]
    pub async fn load(
        &self,
        ctx: &RpcContext,
        id: &PackageId,
        disposition: LoadDisposition,
    ) -> Result<(), Error> {
        let mut shutdown_err = Ok(());
        let mut service = self.get_mut(id).await;
        if let Some(service) = service.take() {
            shutdown_err = service.shutdown().await;
        }
        // TODO: retry on error?
        *service = Service::load(ctx, id, disposition).await?;
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

        let op_name = if recovery_source.is_none() {
            if service.is_none() {
                "Install"
            } else {
                "Update"
            }
        } else {
            "Restore"
        };

        let size = s9pk.size();
        let mut progress = FullProgressTracker::new();
        let download_progress_contribution = size.unwrap_or(60);
        let progress_handle = progress.handle();
        let mut download_progress = progress_handle.add_phase(
            InternedString::intern("Download"),
            Some(download_progress_contribution),
        );
        if let Some(size) = size {
            download_progress.set_total(size);
        }
        let mut finalization_progress = progress_handle.add_phase(
            InternedString::intern(op_name),
            Some(download_progress_contribution / 2),
        );
        let restoring = recovery_source.is_some();

        let mut reload_guard = ServiceReloadGuard::new(ctx.clone(), id.clone(), op_name);

        reload_guard
            .handle(ctx.db.mutate({
                let manifest = manifest.clone();
                let id = id.clone();
                let install_progress = progress.snapshot();
                move |db| {
                    let pde = match db
                        .as_public()
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
                    db.as_public_mut()
                        .as_package_data_mut()
                        .insert(&manifest.id, &pde)
                }
            }))
            .await?;

        Ok(async move {
            let (installed_path, sync_progress_task) = reload_guard
                .handle(async {
                    let download_path = ctx
                        .datadir
                        .join(PKG_ARCHIVE_DIR)
                        .join("downloading")
                        .join(&id)
                        .with_extension("s9pk");

                    let deref_id = id.clone();
                    let sync_progress_task =
                        NonDetachingJoinHandle::from(tokio::spawn(progress.sync_to_db(
                            ctx.db.clone(),
                            move |v| {
                                v.as_public_mut()
                                    .as_package_data_mut()
                                    .as_idx_mut(&deref_id)
                                    .and_then(|e| e.as_install_progress_mut())
                            },
                            Some(Duration::from_millis(100)),
                        )));

                    let mut progress_writer = ProgressTrackerWriter::new(
                        crate::util::io::create_file(&download_path).await?,
                        download_progress,
                    );
                    s9pk.serialize(&mut progress_writer, true).await?;
                    let (file, mut download_progress) = progress_writer.into_inner();
                    file.sync_all().await?;
                    download_progress.complete();

                    let installed_path = ctx
                        .datadir
                        .join(PKG_ARCHIVE_DIR)
                        .join("installed")
                        .join(&id)
                        .with_extension("s9pk");

                    crate::util::io::rename(&download_path, &installed_path).await?;

                    Ok::<_, Error>((installed_path, sync_progress_task))
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
                        finalization_progress.complete();
                        progress_handle.complete();
                        Some(version)
                    } else {
                        None
                    };
                    if let Some(recovery_source) = recovery_source {
                        *service = Some(
                            Service::restore(
                                ctx,
                                s9pk,
                                recovery_source,
                                Some(InstallProgressHandles {
                                    finalization_progress,
                                    progress_handle,
                                }),
                            )
                            .await?,
                        );
                    } else {
                        *service = Some(
                            Service::install(
                                ctx,
                                s9pk,
                                prev,
                                Some(InstallProgressHandles {
                                    finalization_progress,
                                    progress_handle,
                                }),
                            )
                            .await?,
                        );
                    }
                    sync_progress_task.await.map_err(|_| {
                        Error::new(eyre!("progress sync task panicked"), ErrorKind::Unknown)
                    })??;
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
            Ok(a) => Ok(a),
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
        self.ctx
            .services
            .load(&self.ctx, &self.id, LoadDisposition::Undo)
            .await?;
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
