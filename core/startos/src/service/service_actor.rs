use std::sync::Arc;
use std::time::Duration;

use imbl::OrdMap;
use models::PackageId;

use super::start_stop::StartStop;

use crate::prelude::*;
use crate::service::transition::TransitionKind;
use crate::service::SYNC_RETRY_COOLDOWN_SECONDS;
use crate::status::MainStatus;
use crate::util::actor::background::BackgroundJobQueue;
use crate::util::actor::Actor;

use super::ServiceActorSeed;

#[derive(Clone)]
pub(super) struct ServiceActor(pub(super) Arc<ServiceActorSeed>);

enum ServiceActorLoopNext {
    Wait,
    DontWait,
}

impl Actor for ServiceActor {
    fn init(&mut self, jobs: &BackgroundJobQueue) {
        let seed = self.0.clone();
        jobs.add_job(async move {
            let id = seed.id.clone();
            let mut current = seed.persistent_container.state.subscribe();

            loop {
                match service_actor_loop(&current, &seed, &id).await {
                    ServiceActorLoopNext::Wait => tokio::select! {
                        _ = current.changed() => (),
                    },
                    ServiceActorLoopNext::DontWait => (),
                }
            }
        })
    }
}

async fn service_actor_loop(
    current: &tokio::sync::watch::Receiver<super::persistent_container::ServiceState>,
    seed: &Arc<ServiceActorSeed>,
    id: &PackageId,
) -> ServiceActorLoopNext {
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
            (None, StartStop::Stop, Some(_)) => {
                let task_seed = seed.clone();
                seed.ctx
                    .db
                    .mutate(|d| {
                        if let Some(i) = d.as_public_mut().as_package_data_mut().as_idx_mut(&id) {
                            i.as_status_mut().as_main_mut().ser(&MainStatus::Stopping)?;
                        }
                        Ok(())
                    })
                    .await?;
                task_seed.persistent_container.stop().await?;
                MainStatus::Stopped
            }
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
                if let Some(i) = d.as_public_mut().as_package_data_mut().as_idx_mut(&id) {
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
        return ServiceActorLoopNext::DontWait;
    }
    seed.synchronized.notify_waiters();

    ServiceActorLoopNext::Wait
}
