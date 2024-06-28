use std::ops::Deref;
use std::sync::{Arc, Weak};
use std::time::Duration;

use chrono::{DateTime, Utc};
use clap::Parser;
use futures::future::BoxFuture;
use imbl::OrdMap;
use models::{HealthCheckId, PackageId, ProcedureName};
use persistent_container::PersistentContainer;
use rpc_toolkit::{from_fn_async, CallRemoteHandler, Empty, HandlerArgs, HandlerFor};
use serde::{Deserialize, Serialize};
use start_stop::StartStop;
use tokio::fs::File;
use tokio::sync::Notify;
use ts_rs::TS;

use crate::context::{CliContext, RpcContext};
use crate::db::model::package::{
    InstalledState, PackageDataEntry, PackageState, PackageStateMatchModelRef, UpdatingState,
};
use crate::disk::mount::guard::GenericMountGuard;
use crate::install::PKG_ARCHIVE_DIR;
use crate::lxc::ContainerId;
use crate::prelude::*;
use crate::progress::{NamedProgress, Progress};
use crate::rpc_continuations::Guid;
use crate::s9pk::S9pk;
use crate::service::service_map::InstallProgressHandles;
use crate::service::transition::TransitionKind;
use crate::status::health_check::HealthCheckResult;
use crate::status::MainStatus;
use crate::util::actor::background::BackgroundJobQueue;
use crate::util::actor::concurrent::ConcurrentActor;
use crate::util::actor::Actor;
use crate::util::serde::Pem;
use crate::volume::data_dir;

mod action;
pub mod cli;
mod config;
mod control;
mod dependencies;
pub mod persistent_container;
mod properties;
mod rpc;
pub mod service_effect_handler;
pub mod service_map;
mod start_stop;
mod transition;
mod util;

pub use service_map::ServiceMap;

pub const HEALTH_CHECK_COOLDOWN_SECONDS: u64 = 15;
pub const HEALTH_CHECK_GRACE_PERIOD_SECONDS: u64 = 5;
pub const SYNC_RETRY_COOLDOWN_SECONDS: u64 = 10;

pub type Task<'a> = BoxFuture<'a, Result<(), Error>>;

/// TODO
pub enum BackupReturn {
    TODO,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
pub enum LoadDisposition {
    Retry,
    Undo,
}

pub struct ServiceRef(Arc<Service>);
impl ServiceRef {
    pub fn weak(&self) -> Weak<Service> {
        Arc::downgrade(&self.0)
    }
    pub async fn uninstall(
        self,
        target_version: Option<models::VersionString>,
    ) -> Result<(), Error> {
        self.seed
            .persistent_container
            .execute(
                Guid::new(),
                ProcedureName::Uninit,
                to_value(&target_version)?,
                None,
            ) // TODO timeout
            .await?;
        let id = self.seed.persistent_container.s9pk.as_manifest().id.clone();
        let ctx = self.seed.ctx.clone();
        self.shutdown().await?;
        if target_version.is_none() {
            ctx.db
                .mutate(|d| d.as_public_mut().as_package_data_mut().remove(&id))
                .await?;
        }
        Ok(())
    }
    pub async fn shutdown(self) -> Result<(), Error> {
        if let Some((hdl, shutdown)) = self.seed.persistent_container.rpc_server.send_replace(None)
        {
            self.seed
                .persistent_container
                .rpc_client
                .request(rpc::Exit, Empty {})
                .await?;
            shutdown.shutdown();
            hdl.await.with_kind(ErrorKind::Cancelled)?;
        }
        let service = Arc::try_unwrap(self.0).map_err(|_| {
            Error::new(
                eyre!("ServiceActor held somewhere after actor shutdown"),
                ErrorKind::Unknown,
            )
        })?;
        service
            .actor
            .shutdown(crate::util::actor::PendingMessageStrategy::FinishAll { timeout: None }) // TODO timeout
            .await;
        Arc::try_unwrap(service.seed)
            .map_err(|_| {
                Error::new(
                    eyre!("ServiceActorSeed held somewhere after actor shutdown"),
                    ErrorKind::Unknown,
                )
            })?
            .persistent_container
            .exit()
            .await?;
        Ok(())
    }
}
impl Deref for ServiceRef {
    type Target = Service;
    fn deref(&self) -> &Self::Target {
        &*self.0
    }
}
impl From<Service> for ServiceRef {
    fn from(value: Service) -> Self {
        Self(Arc::new(value))
    }
}

pub struct Service {
    actor: ConcurrentActor<ServiceActor>,
    seed: Arc<ServiceActorSeed>,
}
impl Service {
    #[instrument(skip_all)]
    async fn new(ctx: RpcContext, s9pk: S9pk, start: StartStop) -> Result<ServiceRef, Error> {
        let id = s9pk.as_manifest().id.clone();
        let persistent_container = PersistentContainer::new(
            &ctx, s9pk,
            start,
            // desired_state.subscribe(),
            // temp_desired_state.subscribe(),
        )
        .await?;
        let seed = Arc::new(ServiceActorSeed {
            id,
            persistent_container,
            ctx,
            synchronized: Arc::new(Notify::new()),
        });
        let service: ServiceRef = Self {
            actor: ConcurrentActor::new(ServiceActor(seed.clone())),
            seed,
        }
        .into();
        service
            .seed
            .persistent_container
            .init(service.weak())
            .await?;
        Ok(service)
    }

    #[instrument(skip_all)]
    pub async fn load(
        ctx: &RpcContext,
        id: &PackageId,
        disposition: LoadDisposition,
    ) -> Result<Option<ServiceRef>, Error> {
        let handle_installed = {
            let ctx = ctx.clone();
            move |s9pk: S9pk, i: Model<PackageDataEntry>| async move {
                for volume_id in &s9pk.as_manifest().volumes {
                    let tmp_path =
                        data_dir(&ctx.datadir, &s9pk.as_manifest().id.clone(), volume_id);
                    if tokio::fs::metadata(&tmp_path).await.is_err() {
                        tokio::fs::create_dir_all(&tmp_path).await?;
                    }
                }
                let start_stop = if i.as_status().as_main().de()?.running() {
                    StartStop::Start
                } else {
                    StartStop::Stop
                };
                Self::new(ctx, s9pk, start_stop).await.map(Some)
            }
        };
        let s9pk_dir = ctx.datadir.join(PKG_ARCHIVE_DIR).join("installed"); // TODO: make this based on hash
        let s9pk_path = s9pk_dir.join(id).with_extension("s9pk");
        let Some(entry) = ctx
            .db
            .peek()
            .await
            .into_public()
            .into_package_data()
            .into_idx(id)
        else {
            return Ok(None);
        };
        match entry.as_state_info().as_match() {
            PackageStateMatchModelRef::Installing(_) => {
                if disposition == LoadDisposition::Retry {
                    if let Ok(s9pk) = S9pk::open(s9pk_path, Some(id)).await.map_err(|e| {
                        tracing::error!("Error opening s9pk for install: {e}");
                        tracing::debug!("{e:?}")
                    }) {
                        if let Ok(service) = Self::install(ctx.clone(), s9pk, None, None)
                            .await
                            .map_err(|e| {
                                tracing::error!("Error installing service: {e}");
                                tracing::debug!("{e:?}")
                            })
                        {
                            return Ok(Some(service));
                        }
                    }
                }
                // TODO: delete s9pk?
                ctx.db
                    .mutate(|v| v.as_public_mut().as_package_data_mut().remove(id))
                    .await?;
                Ok(None)
            }
            PackageStateMatchModelRef::Updating(s) => {
                if disposition == LoadDisposition::Retry
                    && s.as_installing_info()
                        .as_progress()
                        .de()?
                        .phases
                        .iter()
                        .any(|NamedProgress { name, progress }| {
                            name.eq_ignore_ascii_case("download")
                                && progress == &Progress::Complete(true)
                        })
                {
                    if let Ok(s9pk) = S9pk::open(&s9pk_path, Some(id)).await.map_err(|e| {
                        tracing::error!("Error opening s9pk for update: {e}");
                        tracing::debug!("{e:?}")
                    }) {
                        if let Ok(service) = Self::install(
                            ctx.clone(),
                            s9pk,
                            Some(s.as_manifest().as_version().de()?),
                            None,
                        )
                        .await
                        .map_err(|e| {
                            tracing::error!("Error installing service: {e}");
                            tracing::debug!("{e:?}")
                        }) {
                            return Ok(Some(service));
                        }
                    }
                }
                let s9pk = S9pk::open(s9pk_path, Some(id)).await?;
                ctx.db
                    .mutate({
                        |db| {
                            db.as_public_mut()
                                .as_package_data_mut()
                                .as_idx_mut(id)
                                .or_not_found(id)?
                                .as_state_info_mut()
                                .map_mutate(|s| {
                                    if let PackageState::Updating(UpdatingState {
                                        manifest, ..
                                    }) = s
                                    {
                                        Ok(PackageState::Installed(InstalledState { manifest }))
                                    } else {
                                        Err(Error::new(eyre!("Race condition detected - package state changed during load"), ErrorKind::Database))
                                    }
                                })
                        }
                    })
                    .await?;
                handle_installed(s9pk, entry).await
            }
            PackageStateMatchModelRef::Removing(_) | PackageStateMatchModelRef::Restoring(_) => {
                if let Ok(s9pk) = S9pk::open(s9pk_path, Some(id)).await.map_err(|e| {
                    tracing::error!("Error opening s9pk for removal: {e}");
                    tracing::debug!("{e:?}")
                }) {
                    if let Ok(service) = Self::new(ctx.clone(), s9pk, StartStop::Stop)
                        .await
                        .map_err(|e| {
                            tracing::error!("Error loading service for removal: {e}");
                            tracing::debug!("{e:?}")
                        })
                    {
                        match ServiceRef::from(service).uninstall(None).await {
                            Err(e) => {
                                tracing::error!("Error uninstalling service: {e}");
                                tracing::debug!("{e:?}")
                            }
                            Ok(()) => return Ok(None),
                        }
                    }
                }

                ctx.db
                    .mutate(|v| v.as_public_mut().as_package_data_mut().remove(id))
                    .await?;

                Ok(None)
            }
            PackageStateMatchModelRef::Installed(_) => {
                handle_installed(S9pk::open(s9pk_path, Some(id)).await?, entry).await
            }
            PackageStateMatchModelRef::Error(e) => Err(Error::new(
                eyre!("Failed to parse PackageDataEntry, found {e:?}"),
                ErrorKind::Deserialization,
            )),
        }
    }

    #[instrument(skip_all)]
    pub async fn install(
        ctx: RpcContext,
        s9pk: S9pk,
        src_version: Option<models::VersionString>,
        progress: Option<InstallProgressHandles>,
    ) -> Result<ServiceRef, Error> {
        let manifest = s9pk.as_manifest().clone();
        let developer_key = s9pk.as_archive().signer();
        let icon = s9pk.icon_data_url().await?;
        let service = Self::new(ctx.clone(), s9pk, StartStop::Stop).await?;
        service
            .seed
            .persistent_container
            .execute(
                Guid::new(),
                ProcedureName::Init,
                to_value(&src_version)?,
                None,
            ) // TODO timeout
            .await
            .with_kind(ErrorKind::MigrationFailed)?; // TODO: handle cancellation
        if let Some(mut progress) = progress {
            progress.finalization_progress.complete();
            progress.progress.complete();
            tokio::task::yield_now().await;
        }
        ctx.db
            .mutate(|d| {
                let entry = d
                    .as_public_mut()
                    .as_package_data_mut()
                    .as_idx_mut(&manifest.id)
                    .or_not_found(&manifest.id)?;
                if !manifest.has_config {
                    entry.as_status_mut().as_configured_mut().ser(&true)?;
                }
                entry
                    .as_state_info_mut()
                    .ser(&PackageState::Installed(InstalledState { manifest }))?;
                entry.as_developer_key_mut().ser(&Pem::new(developer_key))?;
                entry.as_icon_mut().ser(&icon)?;
                // TODO: marketplace url
                // TODO: dependency info
                Ok(())
            })
            .await?;
        Ok(service)
    }

    pub async fn restore(
        ctx: RpcContext,
        s9pk: S9pk,
        backup_source: impl GenericMountGuard,
        progress: Option<InstallProgressHandles>,
    ) -> Result<ServiceRef, Error> {
        let service = Service::install(ctx.clone(), s9pk, None, progress).await?;

        service
            .actor
            .send(
                Guid::new(),
                transition::restore::Restore {
                    path: backup_source.path().to_path_buf(),
                },
            )
            .await??;
        Ok(service)
    }

    #[instrument(skip_all)]
    pub async fn backup(&self, guard: impl GenericMountGuard) -> Result<(), Error> {
        let id = &self.seed.id;
        let mut file = File::create(guard.path().join(id).with_extension("s9pk")).await?;
        self.seed
            .persistent_container
            .s9pk
            .clone()
            .serialize(&mut file, true)
            .await?;
        drop(file);
        self.actor
            .send(
                Guid::new(),
                transition::backup::Backup {
                    path: guard.path().to_path_buf(),
                },
            )
            .await??;
        Ok(())
    }

    pub fn container_id(&self) -> Result<ContainerId, Error> {
        let id = &self.seed.id;
        let container_id = (*self
            .seed
            .persistent_container
            .lxc_container
            .get()
            .or_not_found(format!("container for {id}"))?
            .guid)
            .clone();
        Ok(container_id)
    }
}

#[derive(Debug, Clone)]
pub struct RunningStatus {
    health: OrdMap<HealthCheckId, HealthCheckResult>,
    started: DateTime<Utc>,
}

struct ServiceActorSeed {
    ctx: RpcContext,
    id: PackageId,
    /// Needed to interact with the container for the service
    persistent_container: PersistentContainer,
    /// This is notified every time the background job created in ServiceActor::init responds to a change
    synchronized: Arc<Notify>,
}

impl ServiceActorSeed {
    /// Used to indicate that we have finished the task of starting the service
    pub fn started(&self) {
        self.persistent_container.state.send_modify(|state| {
            state.running_status =
                Some(
                    state
                        .running_status
                        .take()
                        .unwrap_or_else(|| RunningStatus {
                            health: Default::default(),
                            started: Utc::now(),
                        }),
                );
        });
    }
    /// Used to indicate that we have finished the task of stopping the service
    pub fn stopped(&self) {
        self.persistent_container.state.send_modify(|state| {
            state.running_status = None;
        });
    }
}
#[derive(Clone)]
struct ServiceActor(Arc<ServiceActorSeed>);

impl Actor for ServiceActor {
    fn init(&mut self, jobs: &BackgroundJobQueue) {
        let seed = self.0.clone();
        jobs.add_job(async move {
            let id = seed.id.clone();
            let mut current = seed.persistent_container.state.subscribe();

            loop {
                let kinds = current.borrow().kinds();

                if let Err(e) = async {
                    let main_status = match (
                        kinds.transition_state,
                        kinds.desired_state,
                        kinds.running_status,
                    ) {
                        (Some(TransitionKind::Restarting), StartStop::Stop, Some(_)) => {
                            seed.persistent_container.stop().await?;
                            MainStatus::Restarting
                        }
                        (Some(TransitionKind::Restarting), StartStop::Start, _) => {
                            seed.persistent_container.start().await?;
                            MainStatus::Restarting
                        }
                        (Some(TransitionKind::Restarting), _, _) => MainStatus::Restarting,
                        (Some(TransitionKind::Restoring), _, _) => MainStatus::Restoring,
                        (Some(TransitionKind::BackingUp), StartStop::Stop, Some(status)) => {
                            seed.persistent_container.stop().await?;
                            MainStatus::BackingUp {
                                started: Some(status.started),
                                health: status.health.clone(),
                            }
                        }
                        (Some(TransitionKind::BackingUp), StartStop::Start, _) => {
                            seed.persistent_container.start().await?;
                            MainStatus::BackingUp {
                                started: None,
                                health: OrdMap::new(),
                            }
                        }
                        (Some(TransitionKind::BackingUp), _, _) => MainStatus::BackingUp {
                            started: None,
                            health: OrdMap::new(),
                        },
                        (None, StartStop::Stop, None) => MainStatus::Stopped,
                        (None, StartStop::Stop, Some(_)) => MainStatus::Stopping {
                            timeout: seed.persistent_container.stop().await?.into(),
                        },
                        (None, StartStop::Start, Some(status)) => MainStatus::Running {
                            started: status.started,
                            health: status.health.clone(),
                        },
                        (None, StartStop::Start, None) => {
                            seed.persistent_container.start().await?;
                            MainStatus::Starting
                        }
                    };
                    seed.ctx
                        .db
                        .mutate(|d| {
                            if let Some(i) = d.as_public_mut().as_package_data_mut().as_idx_mut(&id)
                            {
                                let previous = i.as_status().as_main().de()?;
                                let previous_health = previous.health();
                                let previous_started = previous.started();
                                let mut main_status = main_status;
                                match &mut main_status {
                                    &mut MainStatus::Running { ref mut health, .. }
                                    | &mut MainStatus::BackingUp { ref mut health, .. } => {
                                        *health = previous_health.unwrap_or(health).clone();
                                    }
                                    _ => (),
                                };
                                match &mut main_status {
                                    MainStatus::Running {
                                        ref mut started, ..
                                    } => {
                                        *started = previous_started.unwrap_or(*started);
                                    }
                                    MainStatus::BackingUp {
                                        ref mut started, ..
                                    } => {
                                        *started = previous_started.map(Some).unwrap_or(*started);
                                    }
                                    _ => (),
                                };
                                i.as_status_mut().as_main_mut().ser(&main_status)?;
                            }
                            Ok(())
                        })
                        .await?;

                    Ok::<_, Error>(())
                }
                .await
                {
                    tracing::error!("error synchronizing state of service: {e}");
                    tracing::debug!("{e:?}");

                    seed.synchronized.notify_waiters();

                    tracing::error!("Retrying in {}s...", SYNC_RETRY_COOLDOWN_SECONDS);
                    tokio::time::sleep(Duration::from_secs(SYNC_RETRY_COOLDOWN_SECONDS)).await;
                    continue;
                }

                seed.synchronized.notify_waiters();

                tokio::select! {
                    _ = current.changed() => (),
                }
            }
        })
    }
}

#[derive(Deserialize, Serialize, Parser, TS)]
pub struct ConnectParams {
    pub id: PackageId,
}

pub async fn connect_rpc(
    ctx: RpcContext,
    ConnectParams { id }: ConnectParams,
) -> Result<Guid, Error> {
    let id_ref = &id;
    crate::lxc::connect(
        &ctx,
        ctx.services
            .get(&id)
            .await
            .as_ref()
            .or_not_found(lazy_format!("service for {id_ref}"))?
            .seed
            .persistent_container
            .lxc_container
            .get()
            .or_not_found(lazy_format!("container for {id_ref}"))?,
    )
    .await
}

pub async fn connect_rpc_cli(
    HandlerArgs {
        context,
        parent_method,
        method,
        params,
        inherited_params,
        raw_params,
    }: HandlerArgs<CliContext, ConnectParams>,
) -> Result<(), Error> {
    let ctx = context.clone();
    let guid = CallRemoteHandler::<CliContext, _, _>::new(from_fn_async(connect_rpc))
        .handle_async(HandlerArgs {
            context,
            parent_method,
            method,
            params: rpc_toolkit::util::Flat(params, Empty {}),
            inherited_params,
            raw_params,
        })
        .await?;

    crate::lxc::connect_cli(&ctx, guid).await
}
