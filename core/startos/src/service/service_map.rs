use std::path::{Path, PathBuf};
use std::sync::Arc;
use std::time::Duration;

use color_eyre::eyre::eyre;
use exver::VersionRange;
use futures::future::{BoxFuture, Fuse};
use futures::stream::FuturesUnordered;
use futures::{Future, FutureExt, StreamExt, TryFutureExt};
use helpers::NonDetachingJoinHandle;
use imbl::OrdMap;
use models::ErrorData;
use tokio::sync::{oneshot, Mutex, OwnedRwLockReadGuard, OwnedRwLockWriteGuard, RwLock};
use tracing::instrument;
use url::Url;

use crate::context::RpcContext;
use crate::db::model::package::{
    InstallingInfo, InstallingState, PackageDataEntry, PackageState, UpdatingState,
};
use crate::disk::mount::guard::GenericMountGuard;
use crate::install::PKG_ARCHIVE_DIR;
use crate::notifications::{notify, NotificationLevel};
use crate::prelude::*;
use crate::progress::{
    FullProgressTracker, PhaseProgressTrackerHandle, ProgressTrackerWriter, ProgressUnits,
};
use crate::registry::signer::commitment::merkle_archive::MerkleArchiveCommitment;
use crate::s9pk::manifest::PackageId;
use crate::s9pk::merkle_archive::source::FileSource;
use crate::s9pk::S9pk;
use crate::service::rpc::ExitParams;
use crate::service::start_stop::StartStop;
use crate::service::{LoadDisposition, Service, ServiceRef};
use crate::status::MainStatus;
use crate::util::serde::{Base32, Pem};
use crate::DATA_DIR;

pub type DownloadInstallFuture = BoxFuture<'static, Result<InstallFuture, Error>>;
pub type InstallFuture = BoxFuture<'static, Result<(), Error>>;

pub struct InstallProgressHandles {
    pub finalization_progress: PhaseProgressTrackerHandle,
    pub progress: FullProgressTracker,
}

fn s9pk_download_path(commitment: &MerkleArchiveCommitment) -> PathBuf {
    Path::new(DATA_DIR)
        .join(PKG_ARCHIVE_DIR)
        .join("downloading")
        .join(Base32(commitment.root_sighash.0).to_lower_string())
        .with_extension("s9pk")
}

fn s9pk_installed_path(commitment: &MerkleArchiveCommitment) -> PathBuf {
    Path::new(DATA_DIR)
        .join(PKG_ARCHIVE_DIR)
        .join("installed")
        .join(Base32(commitment.root_sighash.0).to_lower_string())
        .with_extension("s9pk")
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
        progress.set_units(Some(ProgressUnits::Steps));
        let mut jobs = FuturesUnordered::new();
        for id in &ids {
            jobs.push(self.load(ctx, id, LoadDisposition::Retry));
        }
        while let Some(res) = jobs.next().await {
            if let Err(e) = res {
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
            shutdown_err = service.shutdown(None).await;
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
                    .await
                    .result?;
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
        registry: Option<Url>,
        recovery_source: Option<impl GenericMountGuard>,
        progress: Option<FullProgressTracker>,
    ) -> Result<DownloadInstallFuture, Error>
    where
        F: FnOnce() -> Fut,
        Fut: Future<Output = Result<S9pk<S>, Error>>,
        S: FileSource + Clone,
    {
        let progress = progress.unwrap_or_else(|| FullProgressTracker::new());
        let mut validate_progress = progress.add_phase("Validating Headers".into(), Some(1));
        let mut unpack_progress = progress.add_phase("Unpacking".into(), Some(100));

        let mut s9pk = s9pk().await?;
        validate_progress.start();
        s9pk.validate_and_filter(ctx.s9pk_arch)?;
        validate_progress.complete();
        let commitment = s9pk.as_archive().commitment().await?;
        let mut installed_path = s9pk_installed_path(&commitment);
        while tokio::fs::metadata(&installed_path).await.is_ok() {
            let prev = installed_path.file_stem().unwrap_or_default();
            installed_path.set_file_name(prev.to_string_lossy().into_owned() + "x.s9pk");
            // append an x if already exists to avoid reference counting when reinstalling same s9pk
        }
        let manifest = s9pk.as_manifest().clone();
        let id = manifest.id.clone();
        let icon = s9pk.icon_data_url().await?;
        let developer_key = s9pk.as_archive().signer();
        let mut service = self.get_mut(&id).await;
        let size = s9pk.size();
        if let Some(size) = size {
            unpack_progress.set_total(size);
        }
        let op_name = if recovery_source.is_none() {
            if service.is_none() {
                "Installing"
            } else {
                "Updating"
            }
        } else {
            "Restoring"
        };
        let mut finalization_progress = progress.add_phase(op_name.into(), Some(50));
        let restoring = recovery_source.is_some();

        let (cancel_send, cancel_recv) = oneshot::channel();
        ctx.cancellable_installs
            .mutate(|c| c.insert(id.clone(), cancel_send));

        let mut reload_guard =
            ServiceRefReloadCancelGuard::new(ctx.clone(), id.clone(), op_name, Some(cancel_recv));

        reload_guard
            .handle(async {
                ctx.db
                    .mutate({
                        let installed_path = installed_path.clone();
                        let manifest = manifest.clone();
                        let id = id.clone();
                        let install_progress = progress.snapshot();
                        let registry = registry.clone();
                        move |db| {
                            if let Some(pde) =
                                db.as_public_mut().as_package_data_mut().as_idx_mut(&id)
                            {
                                let prev = pde.as_state_info().expect_installed()?.de()?;
                                pde.as_state_info_mut().ser(&PackageState::Updating(
                                    UpdatingState {
                                        manifest: prev.manifest,
                                        s9pk: installed_path,
                                        installing_info: InstallingInfo {
                                            new_manifest: manifest,
                                            progress: install_progress,
                                        },
                                    },
                                ))?;
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
                                        s9pk: installed_path,
                                        status: MainStatus::Stopped,
                                        registry,
                                        developer_key: Pem::new(developer_key),
                                        icon,
                                        last_backup: None,
                                        current_dependencies: Default::default(),
                                        actions: Default::default(),
                                        tasks: Default::default(),
                                        service_interfaces: Default::default(),
                                        hosts: Default::default(),
                                        store_exposed_dependents: Default::default(),
                                    },
                                )?;
                            };
                            Ok(())
                        }
                    })
                    .await
                    .result
            })
            .await?;

        Ok(async move {
            let sync_progress_task = reload_guard
                .handle(async {
                    let download_path = s9pk_download_path(&commitment);

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

                    unpack_progress.start();
                    let mut progress_writer = ProgressTrackerWriter::new(
                        crate::util::io::create_file(&download_path).await?,
                        unpack_progress,
                    );
                    s9pk.serialize(&mut progress_writer, true).await?;
                    let (file, mut unpack_progress) = progress_writer.into_inner();
                    file.sync_all().await?;
                    unpack_progress.complete();

                    crate::util::io::rename(&download_path, &installed_path).await?;

                    Ok::<_, Error>(sync_progress_task)
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
                        let prev_version = service
                            .seed
                            .persistent_container
                            .s9pk
                            .as_manifest()
                            .version
                            .clone();
                        let prev_can_migrate_to = &service
                            .seed
                            .persistent_container
                            .s9pk
                            .as_manifest()
                            .can_migrate_to;
                        let next_version = &s9pk.as_manifest().version;
                        let next_can_migrate_from = &s9pk.as_manifest().can_migrate_from;
                        let uninit = if prev_version.satisfies(next_can_migrate_from) {
                            ExitParams::target_version(&*prev_version)
                        } else if next_version.satisfies(prev_can_migrate_to) {
                            ExitParams::target_version(&s9pk.as_manifest().version)
                        } else {
                            ExitParams::target_range(&VersionRange::and(
                                prev_can_migrate_to.clone(),
                                next_can_migrate_from.clone(),
                            ))
                        };
                        let run_state = service
                            .seed
                            .persistent_container
                            .state
                            .borrow()
                            .desired_state;
                        service.uninstall(uninit, false, false).await?;
                        progress.complete();
                        Some(run_state)
                    } else {
                        None
                    };
                    let new_service = Service::install(
                        ctx,
                        s9pk,
                        &installed_path,
                        &registry,
                        prev,
                        recovery_source,
                        Some(InstallProgressHandles {
                            finalization_progress,
                            progress,
                        }),
                    )
                    .await?;
                    *service = Some(new_service.into());
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
    pub async fn uninstall(
        &self,
        ctx: RpcContext,
        id: PackageId,
        soft: bool,
        force: bool,
    ) -> Result<impl Future<Output = Result<(), Error>> + Send, Error> {
        let mut guard = self.get_mut(&id).await;
        ctx.db
            .mutate(|db| {
                let entry = db
                    .as_public_mut()
                    .as_package_data_mut()
                    .as_idx_mut(&id)
                    .or_not_found(&id)?;
                entry.as_state_info_mut().map_mutate(|s| match s {
                    PackageState::Installed(s) => Ok(PackageState::Removing(s)),
                    _ => Err(Error::new(
                        eyre!("Package {id} is not installed."),
                        crate::ErrorKind::NotFound,
                    )),
                })
            })
            .await
            .result?;
        Ok(async move {
            ServiceRefReloadCancelGuard::new(ctx.clone(), id.clone(), "Uninstall", None)
                .handle_last(async move {
                    if let Some(service) = guard.take() {
                        let res = service
                            .uninstall(ExitParams::uninstall(), soft, force)
                            .await;
                        drop(guard);
                        res
                    } else {
                        if force {
                            super::uninstall::cleanup(&ctx, &id, soft).await?;
                            Ok(())
                        } else {
                            Err(Error::new(
                                eyre!(
                                    "service {id} failed to initialize - cannot remove gracefully"
                                ),
                                ErrorKind::Uninitialized,
                            ))
                        }
                    }
                })
                .await?;

            Ok(())
        }
        .or_else(|e: Error| e.wait().map(Err)))
    }

    pub async fn shutdown_all(&self) -> Result<(), Error> {
        let lock = self.0.lock().await;
        let mut futs = Vec::with_capacity(lock.len());
        for service in lock.values().cloned() {
            futs.push(async move {
                if let Some(service) = service.write_owned().await.take() {
                    service.shutdown(None).await?
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

pub struct ServiceRefReloadCancelGuard(
    Option<ServiceRefReloadInfo>,
    Option<Fuse<oneshot::Receiver<()>>>,
);
impl Drop for ServiceRefReloadCancelGuard {
    fn drop(&mut self) {
        if let Some(info) = self.0.take() {
            tokio::spawn(info.reload(None));
        }
    }
}
impl ServiceRefReloadCancelGuard {
    pub fn new(
        ctx: RpcContext,
        id: PackageId,
        operation: &'static str,
        cancel: Option<oneshot::Receiver<()>>,
    ) -> Self {
        Self(
            Some(ServiceRefReloadInfo { ctx, id, operation }),
            cancel.map(|c| c.fuse()),
        )
    }

    pub async fn handle<T>(
        &mut self,
        operation: impl Future<Output = Result<T, Error>>,
    ) -> Result<T, Error> {
        let res = async {
            if let Some(cancel) = self.1.as_mut() {
                tokio::select! {
                    res = operation => res,
                    _ = cancel => Err(Error::new(eyre!("Operation Cancelled"), ErrorKind::Cancelled)),
                }
            } else {
                operation.await
            }
        }.await;
        match res {
            Ok(a) => Ok(a),
            Err(e) => {
                if let Some(info) = self.0.take() {
                    let task_e = e.clone_output();
                    Err(e.with_task(tokio::spawn(async move {
                        info.reload(Some(task_e)).await.log_err();
                    })))
                } else {
                    Err(e)
                }
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
                .await
                .result?;
        }
        Ok(())
    }
}
