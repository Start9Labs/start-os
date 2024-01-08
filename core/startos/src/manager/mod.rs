use std::borrow::Borrow;
use std::sync::Arc;
use std::time::Duration;

use chrono::{DateTime, Utc};
use imbl::OrdMap;
use models::HealthCheckId;
use persistent_container::PersistentContainer;
use start_stop::StartStop;
use tokio::sync::{watch, Notify};

use crate::context::RpcContext;
use crate::manager::transition::{TempDesiredState, TransitionKind, TransitionState};
use crate::prelude::*;
use crate::s9pk::S9pk;
use crate::status::health_check::HealthCheckResult;
use crate::status::MainStatus;
use crate::util::actor::{Actor, BackgroundJobs, SimpleActor};

mod control;
mod manager_map;
pub mod persistent_container;
mod rpc;
mod service_effects_service;
mod start_stop;
mod transition;
mod util;

pub const HEALTH_CHECK_COOLDOWN_SECONDS: u64 = 15;
pub const HEALTH_CHECK_GRACE_PERIOD_SECONDS: u64 = 5;
pub const SYNC_RETRY_COOLDOWN_SECONDS: u64 = 10;

pub struct Service {
    actor: SimpleActor<ServiceActor>,
    seed: Arc<ServiceActorSeed>,
}

#[derive(Clone)]
struct RunningStatus {
    health: OrdMap<HealthCheckId, HealthCheckResult>,
    started: DateTime<Utc>,
}

struct ServiceActorSeed {
    ctx: RpcContext,
    package: S9pk,
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
        let id = self.0.package.as_manifest().id.clone();
        let ctx = self.0.ctx.clone();
        let current = self.0.persistent_container.current_state.subscribe();
        let desired = self.0.desired_state.subscribe();
        let temp_desired = self.0.temp_desired_state.subscribe();
        let transition = self.0.transition_state.subscribe();
        let running = self.0.running_status.clone();
        let synchronized = self.0.synchronized.clone();
        jobs.add_job(async move {
            loop {
                let (desired_state, current_state, transition_kind, running_status) = (
                    temp_desired.borrow().unwrap_or(*desired.borrow()),
                    *current.borrow(),
                    transition.borrow().as_ref().map(|t| t.kind()),
                    running.borrow().clone(),
                );

                if let Err(e) = async {
                    ctx.db
                        .mutate(|d| {
                            if let Some(i) = d
                                .as_package_data_mut()
                                .as_idx_mut(&id)
                                .and_then(|p| p.as_installed_mut())
                            {
                                i.as_status_mut().as_main_mut().ser(
                                    match (
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
                                        (
                                            None,
                                            StartStop::Start,
                                            StartStop::Start,
                                            Some(status),
                                        ) => MainStatus::Running {
                                            started: status.started,
                                            health: status.health.clone(),
                                        },
                                    },
                                )?;
                            }
                            Ok(())
                        })
                        .await?;
                    match (desired_state, current_state) {
                        (StartStop::Start, StartStop::Stop) => {
                            self.persistent_container.start().await
                        }
                        (StartStop::Stop, StartStop::Start) => {
                            self.persistent_container
                                .stop(todo!("s9pk sigterm timeout"))
                                .await
                        }
                        _ => Ok(()),
                    }?;
                } {
                    tracing::error!("error synchronizing state of service: {e}");
                    tracing::debug!("{e:?}");

                    synchronized.notify_waiters();

                    tracing::error!("Retrying in {}s...", SYNC_RETRY_COOLDOWN_SECONDS);
                    tokio::time::sleep(Duration::from_secs(SYNC_RETRY_COOLDOWN_SECONDS)).await;
                    continue;
                }

                synchronized.notify_waiters();

                futures::future::select_all([
                    current.changed(),
                    desired.changed(),
                    temp_desired.changed(),
                    transition.changed(),
                    running.changed(),
                ])
                .await;
            }
        })
    }
}
