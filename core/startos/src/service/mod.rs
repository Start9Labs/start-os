use std::borrow::Borrow;
use std::path::Path;
use std::sync::Arc;
use std::time::Duration;

use chrono::{DateTime, Utc};
use futures::future::BoxFuture;
use futures::Future;
use imbl::OrdMap;
use imbl_value::{InOMap, InternedString};
use models::{ActionId, HealthCheckId, PackageId, ProcedureName};
use persistent_container::PersistentContainer;
use start_stop::StartStop;
use tokio::sync::{watch, Mutex, Notify};

use crate::action::ActionResult;
use crate::config::action::ConfigRes;
use crate::config::ConfigurationError;
use crate::context::RpcContext;
use crate::db::model::{
    InstalledPackageInfo, PackageDataEntry, PackageDataEntryInstalled, PackageDataEntryMatchModel,
};
use crate::disk::mount::backup::BackupMountGuard;
use crate::disk::mount::guard::{GenericMountGuard, TmpMountGuard};
use crate::install::progress::InstallProgress;
use crate::install::PKG_ARCHIVE_DIR;
use crate::prelude::*;
use crate::s9pk;
use crate::s9pk::S9pk;
use crate::service::transition::{TempDesiredState, TransitionKind, TransitionState};
use crate::status::health_check::HealthCheckResult;
use crate::status::MainStatus;
use crate::util::actor::{Actor, BackgroundJobs, SimpleActor};
use crate::volume::data_dir;

mod config;
mod control;
pub mod persistent_container;
mod rpc;
mod service_effect_handler;
mod service_map;
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

pub struct Service {
    actor: SimpleActor<ServiceActor>,
    seed: Arc<ServiceActorSeed>,
}
impl Service {
    async fn new(ctx: RpcContext, s9pk: S9pk, start: StartStop) -> Result<Self, Error> {
        let id = s9pk.as_manifest().id.clone();
        let desired_state = watch::channel(start).0;
        let temp_desired_state = TempDesiredState(Arc::new(watch::channel(None).0));
        let persistent_container = PersistentContainer::init(
            &ctx,
            s9pk,
            desired_state.subscribe(),
            temp_desired_state.subscribe(),
        )
        .await?;
        let seed = Arc::new(ServiceActorSeed {
            id,
            running_status: persistent_container.running_status.subscribe(),
            persistent_container,
            ctx,
            desired_state,
            temp_desired_state,
            transition_state: Arc::new(watch::channel(None).0),
            synchronized: Arc::new(Notify::new()),
        });
        Ok(Self {
            actor: SimpleActor::new(ServiceActor(seed.clone())),
            seed,
        })
    }

    pub async fn load(ctx: &RpcContext, id: &PackageId) -> Result<Option<Self>, Error> {
        let handle_installed = {
            let ctx = ctx.clone();
            move |s9pk: S9pk, i: Model<InstalledPackageInfo>| async move {
                for volume_id in &s9pk.as_manifest().volumes {
                    let tmp_path =
                        data_dir(&ctx.datadir, &s9pk.as_manifest().id.clone(), volume_id);
                    if tokio::fs::metadata(&tmp_path).await.is_ok() {
                        tokio::fs::remove_dir_all(&tmp_path).await?;
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
        let s9pk_path = ctx
            .datadir
            .join(PKG_ARCHIVE_DIR)
            .join("installed")
            .join(&id)
            .with_extension("s9pk");
        match ctx
            .db
            .peek()
            .await
            .into_package_data()
            .into_idx(id)
            .map(|pde| pde.into_match())
        {
            Some(PackageDataEntryMatchModel::Installing(_)) => {
                if let Ok(s9pk) = S9pk::open(s9pk_path, Some(id)).await {
                    Self::install(ctx.clone(), s9pk, None).await.map(Some)
                } else {
                    // TODO: delete s9pk?
                    Ok(None)
                }
            }
            Some(PackageDataEntryMatchModel::Updating(e)) => {
                if e.as_install_progress()
                    .de()?
                    .download_complete
                    .load(std::sync::atomic::Ordering::Relaxed)
                {
                    Self::install(
                        ctx.clone(),
                        S9pk::open(s9pk_path, Some(id)).await?,
                        Some(e.as_installed().as_manifest().as_version().de()?),
                    )
                    .await
                    .map(Some)
                } else {
                    let s9pk = S9pk::open(s9pk_path, Some(id)).await?;
                    ctx.db
                        .mutate({
                            let manifest = s9pk.as_manifest().clone();
                            |db| {
                                db.as_package_data_mut()
                                    .as_idx_mut(&manifest.id)
                                    .or_not_found(&manifest.id)?
                                    .ser(&PackageDataEntry::Installed(PackageDataEntryInstalled {
                                        static_files: e.as_static_files().de()?,
                                        manifest,
                                        installed: e.as_installed().de()?,
                                    }))
                            }
                        })
                        .await?;
                    handle_installed(s9pk, e.as_installed().clone()).await
                }
            }
            Some(PackageDataEntryMatchModel::Removing(_))
            | Some(PackageDataEntryMatchModel::Restoring(_)) => {
                Self::new(
                    ctx.clone(),
                    S9pk::open(s9pk_path, Some(id)).await?,
                    StartStop::Stop,
                )
                .await?
                .uninstall(None)
                .await?;
                Ok(None)
            }
            Some(PackageDataEntryMatchModel::Installed(i)) => {
                handle_installed(
                    S9pk::open(s9pk_path, Some(id)).await?,
                    i.as_installed().clone(),
                )
                .await
            }
            Some(PackageDataEntryMatchModel::Error(e)) => Err(Error::new(
                eyre!("Failed to parse PackageDataEntry, found {e:?}"),
                ErrorKind::Deserialization,
            )),
            None => Ok(None),
        }
    }

    pub async fn install(
        ctx: RpcContext,
        s9pk: S9pk,
        src_version: Option<models::Version>,
    ) -> Result<Self, Error> {
        todo!()
    }

    pub async fn restore(
        ctx: RpcContext,
        s9pk: S9pk,
        guard: impl GenericMountGuard,
    ) -> Result<Self, Error> {
        todo!()
    }

    pub async fn get_config(&self) -> Result<ConfigRes, Error> {
        let container = self.seed.persistent_container.borrow().clone();
        container
            .execute::<ConfigRes>(
                ProcedureName::GetConfig,
                Value::Null,
                Some(Duration::from_secs(30)),
            )
            .await?
            .map_err(|e| Error::new(eyre!("{}", e.1), ErrorKind::ConfigGen))
    }

    pub async fn action(&self, id: ActionId, input: Value) -> Result<ActionResult, Error> {
        let container = self.seed.persistent_container.borrow().clone();
        container
            .execute::<ActionResult>(
                ProcedureName::Action(id),
                input,
                Some(Duration::from_secs(30)),
            )
            .await?
            .map_err(|e| Error::new(eyre!("{}", e.1), ErrorKind::Action))
    }

    pub async fn shutdown(self) -> Result<(), Error> {
        todo!()
    }

    pub async fn uninstall(self, target_version: Option<models::Version>) -> Result<(), Error> {
        todo!()
    }
    pub async fn backup(
        &self,
        guard: Arc<Mutex<BackupMountGuard<TmpMountGuard>>>,
    ) -> Result<BackupReturn, Error> {
        todo!()
    }
}

#[derive(Clone)]
struct RunningStatus {
    health: OrdMap<HealthCheckId, HealthCheckResult>,
    started: DateTime<Utc>,
}

struct ServiceActorSeed {
    ctx: RpcContext,
    id: PackageId,
    persistent_container: PersistentContainer,
    desired_state: watch::Sender<StartStop>,
    temp_desired_state: TempDesiredState,
    transition_state: Arc<watch::Sender<Option<TransitionState>>>,
    running_status: watch::Receiver<Option<RunningStatus>>,
    synchronized: Arc<Notify>,
}



struct ServiceActor(Arc<ServiceActorSeed>);
impl Actor for ServiceActor {
    fn init(&mut self, jobs: &mut BackgroundJobs) {
        let seed = self.0.clone();
        jobs.add_job(async move {
            let id = seed.id.clone();
            let mut current = seed.persistent_container.current_state.subscribe();
            let mut desired = seed.desired_state.subscribe();
            let mut temp_desired = seed.temp_desired_state.subscribe();
            let mut transition = seed.transition_state.subscribe();
            let mut running = seed.running_status.clone();
            loop {
                let (desired_state, current_state, transition_kind, running_status) = (
                    temp_desired.borrow().unwrap_or(*desired.borrow()),
                    *current.borrow(),
                    transition.borrow().as_ref().map(|t| t.kind()),
                    running.borrow().clone(),
                );

                if let Err(e) = async {
                    seed.ctx
                        .db
                        .mutate(|d| {
                            if let Some(i) = d
                                .as_package_data_mut()
                                .as_idx_mut(&id)
                                .and_then(|p| p.as_installed_mut())
                            {
                                i.as_status_mut().as_main_mut().ser(&match (
                                    transition_kind,
                                    desired_state,
                                    current_state,
                                    running_status,
                                ) {
                                    (Some(TransitionKind::Restarting), _, _, _) => {
                                        MainStatus::Restarting
                                    }
                                    (Some(TransitionKind::BackingUp), _, _, Some(status)) => {
                                        MainStatus::BackingUp {
                                            started: Some(status.started),
                                            health: status.health.clone(),
                                        }
                                    }
                                    (Some(TransitionKind::BackingUp), _, _, None) => {
                                        MainStatus::BackingUp {
                                            started: None,
                                            health: OrdMap::new(),
                                        }
                                    }
                                    (None, StartStop::Stop, StartStop::Stop, _) => {
                                        MainStatus::Stopped
                                    }
                                    (None, StartStop::Stop, StartStop::Start, _) => {
                                        MainStatus::Stopping
                                    }
                                    (None, StartStop::Start, StartStop::Stop, _) => {
                                        MainStatus::Starting
                                    }
                                    (None, StartStop::Start, StartStop::Start, None) => {
                                        MainStatus::Starting
                                    }
                                    (None, StartStop::Start, StartStop::Start, Some(status)) => {
                                        MainStatus::Running {
                                            started: status.started,
                                            health: status.health.clone(),
                                        }
                                    }
                                })?;
                            }
                            Ok(())
                        })
                        .await?;
                    match (desired_state, current_state) {
                        (StartStop::Start, StartStop::Stop) => {
                            seed.persistent_container.start().await
                        }
                        (StartStop::Stop, StartStop::Start) => {
                            seed.persistent_container
                                .stop(todo!("s9pk sigterm timeout"))
                                .await
                        }
                        _ => Ok(()),
                    }
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
                    _ = desired.changed() => (),
                    _ = temp_desired.changed() => (),
                    _ = transition.changed() => (),
                    _ = running.changed() => (),
                }
            }
        })
    }
}
