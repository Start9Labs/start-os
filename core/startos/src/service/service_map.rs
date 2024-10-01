use std::sync::Arc;
use std::time::Duration;

use color_eyre::eyre::eyre;
use futures::future::BoxFuture;
use futures::{Future, FutureExt};
use helpers::NonDetachingJoinHandle;
use imbl::OrdMap;
use imbl_value::InternedString;
use models::ErrorData;
use tokio::sync::{Mutex, OwnedRwLockReadGuard, OwnedRwLockWriteGuard, RwLock};
use tracing::instrument;

use crate::context::RpcContext;
use crate::db::model::package::{
    InstallingInfo, InstallingState, PackageDataEntry, PackageState, UpdatingState,
};
use crate::disk::mount::guard::GenericMountGuard;
use crate::install::PKG_ARCHIVE_DIR;
use crate::notifications::{notify, NotificationLevel};
use crate::prelude::*;
use crate::progress::{FullProgressTracker, PhaseProgressTrackerHandle, ProgressTrackerWriter};
use crate::s9pk::manifest::PackageId;
use crate::s9pk::merkle_archive::source::FileSource;
use crate::s9pk::S9pk;
use crate::service::start_stop::StartStop;
use crate::service::{LoadDisposition, Service, ServiceRef};
use crate::status::MainStatus;
use crate::util::serde::Pem;

pub type DownloadInstallFuture = BoxFuture<'static, Result<InstallFuture, Error>>;
pub type InstallFuture = BoxFuture<'static, Result<(), Error>>;

pub struct InstallProgressHandles {
    pub finalization_progress: PhaseProgressTrackerHandle,
    pub progress: FullProgressTracker,
}

/// This is the structure to contain all the services
#[derive(Default)]
pub struct ServiceMap(Mutex<OrdMap<PackageId, Arc<RwLock<Option<ServiceRef>>>>>);
impl ServiceMap {
    async fn entry(&self, id: &PackageId) -> Arc<RwLock<Option<ServiceRef>>> {
        let mut lock = self.0.lock().await;
        lock.entry(id.clone())
            .or_insert_with(|| Arc::new(RwLock::new(None)))
            .clone()
    }

    #[instrument(skip_all)]
    pub async fn get(&self, id: &PackageId) -> OwnedRwLockReadGuard<Option<ServiceRef>> {
        self.entry(id).await.read_owned().await
    }

    #[instrument(skip_all)]
    pub async fn get_mut(&self, id: &PackageId) -> OwnedRwLockWriteGuard<Option<ServiceRef>> {
        self.entry(id).await.write_owned().await
    }

    #[instrument(skip_all)]
    pub async fn init(
        &self,
        ctx: &RpcContext,
        mut progress: PhaseProgressTrackerHandle,
    ) -> Result<(), Error> {
        progress.start();
        let ids = ctx.db.peek().await.as_public().as_package_data().keys()?;
        progress.set_total(ids.len() as u64);
        for id in ids {
            if let Err(e) = self.load(ctx, &id, LoadDisposition::Retry).await {
                tracing::error!("Error loading installed package as service: {e}");
                tracing::debug!("{e:?}");
            }
            progress += 1;
        }
        progress.complete();
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
        match Service::load(ctx, id, disposition).await {
            Ok(s) => *service = s.into(),
            Err(e) => {
                let e = ErrorData::from(e);
                ctx.db
                    .mutate(|db| {
                        if let Some(pde) = db.as_public_mut().as_package_data_mut().as_idx_mut(id) {
                            pde.as_status_mut().map_mutate(|s| {
                                Ok(MainStatus::Error {
                                    on_rebuild: if s.running() {
                                        StartStop::Start
                                    } else {
                                        StartStop::Stop
                                    },
                                    message: e.details,
                                    debug: Some(e.debug),
                                })
                            })?;
                        }
                        Ok(())
                    })
                    .await?;
            }
        }
        shutdown_err?;
        Ok(())
    }

    #[instrument(skip_all)]
    pub async fn install<F, Fut, S: FileSource + Clone>(
        &self,
        ctx: RpcContext,
        s9pk: F,
        recovery_source: Option<impl GenericMountGuard>,
        progress: Option<FullProgressTracker>,
    ) -> Result<DownloadInstallFuture, Error>
    where
        F: FnOnce() -> Fut,
        Fut: Future<Output = Result<S9pk<S>, Error>>,
        S: FileSource + Clone,
    {
        let mut s9pk = s9pk().await?;
        s9pk.validate_and_filter(ctx.s9pk_arch)?;
        let manifest = s9pk.as_manifest().clone();
        let id = manifest.id.clone();
        let icon = s9pk.icon_data_url().await?;
        let developer_key = s9pk.as_archive().signer();
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
        let progress = progress.unwrap_or_else(|| FullProgressTracker::new());
        let download_progress_contribution = size.unwrap_or(60);
        let mut download_progress = progress.add_phase(
            InternedString::intern("Download"),
            Some(download_progress_contribution),
        );
        if let Some(size) = size {
            download_progress.set_total(size);
        }
        let mut finalization_progress = progress.add_phase(
            InternedString::intern(op_name),
            Some(download_progress_contribution / 2),
        );
        let restoring = recovery_source.is_some();

        let mut reload_guard = ServiceRefReloadGuard::new(ctx.clone(), id.clone(), op_name);

        reload_guard
            .handle(ctx.db.mutate({
                let manifest = manifest.clone();
                let id = id.clone();
                let install_progress = progress.snapshot();
                move |db| {
                    if let Some(pde) = db.as_public_mut().as_package_data_mut().as_idx_mut(&id) {
                        let prev = pde.as_state_info().expect_installed()?.de()?;
                        pde.as_state_info_mut()
                            .ser(&PackageState::Updating(UpdatingState {
                                manifest: prev.manifest,
                                installing_info: InstallingInfo {
                                    new_manifest: manifest,
                                    progress: install_progress,
                                },
                            }))?;
                    } else {
                        let installing = InstallingState {
                            installing_info: InstallingInfo {
                                new_manifest: manifest,
                                progress: install_progress,
                            },
                        };
                        db.as_public_mut().as_package_data_mut().insert(
                            &id,
                            &PackageDataEntry {
                                state_info: if restoring {
                                    PackageState::Restoring(installing)
                                } else {
                                    PackageState::Installing(installing)
                                },
                                data_version: None,
                                status: MainStatus::Stopped,
                                registry: None,
                                developer_key: Pem::new(developer_key),
                                icon,
                                last_backup: None,
                                current_dependencies: Default::default(),
                                actions: Default::default(),
                                requested_actions: Default::default(),
                                service_interfaces: Default::default(),
                                hosts: Default::default(),
                                store_exposed_dependents: Default::default(),
                            },
                        )?;
                    };
                    Ok(())
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
                        NonDetachingJoinHandle::from(tokio::spawn(progress.clone().sync_to_db(
                            ctx.db.clone(),
                            move |v| {
                                v.as_public_mut()
                                    .as_package_data_mut()
                                    .as_idx_mut(&deref_id)
                                    .and_then(|e| e.as_state_info_mut().as_installing_info_mut())
                                    .map(|i| i.as_progress_mut())
                            },
                            Some(Duration::from_millis(100)),
                        )));

                    download_progress.start();
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
                    finalization_progress.start();
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
                        progress.complete();
                        Some(version)
                    } else {
                        None
                    };
                    *service = Some(
                        Service::install(
                            ctx,
                            s9pk,
                            prev,
                            recovery_source,
                            Some(InstallProgressHandles {
                                finalization_progress,
                                progress,
                            }),
                        )
                        .await?
                        .into(),
                    );
                    drop(service);

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
        let mut guard = self.get_mut(id).await;
        if let Some(service) = guard.take() {
            ServiceRefReloadGuard::new(ctx.clone(), id.clone(), "Uninstall")
                .handle_last(async move {
                    let res = service.uninstall(None).await;
                    drop(guard);
                    res
                })
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

pub struct ServiceRefReloadGuard(Option<ServiceRefReloadInfo>);
impl Drop for ServiceRefReloadGuard {
    fn drop(&mut self) {
        if let Some(info) = self.0.take() {
            tokio::spawn(info.reload(None));
        }
    }
}
impl ServiceRefReloadGuard {
    pub fn new(ctx: RpcContext, id: PackageId, operation: &'static str) -> Self {
        Self(Some(ServiceRefReloadInfo { ctx, id, operation }))
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

struct ServiceRefReloadInfo {
    ctx: RpcContext,
    id: PackageId,
    operation: &'static str,
}
impl ServiceRefReloadInfo {
    async fn reload(self, error: Option<Error>) -> Result<(), Error> {
        self.ctx
            .services
            .load(&self.ctx, &self.id, LoadDisposition::Undo)
            .await?;
        if let Some(error) = error {
            let error_string = error.to_string();
            self.ctx
                .db
                .mutate(|db| {
                    notify(
                        db,
                        Some(self.id.clone()),
                        NotificationLevel::Error,
                        format!("{} Failed", self.operation),
                        error_string,
                        (),
                    )
                })
                .await?;
        }
        Ok(())
    }
}
