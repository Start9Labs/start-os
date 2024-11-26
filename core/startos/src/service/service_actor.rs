use std::sync::Arc;
use std::time::Duration;

use imbl::vector;

use super::start_stop::StartStop;
use super::ServiceActorSeed;
use crate::prelude::*;
use crate::service::persistent_container::ServiceStateKinds;
use crate::service::transition::TransitionKind;
use crate::service::SYNC_RETRY_COOLDOWN_SECONDS;
use crate::status::MainStatus;
use crate::util::actor::background::BackgroundJobQueue;
use crate::util::actor::Actor;

#[derive(Clone)]
pub(super) struct ServiceActor(pub(super) Arc<ServiceActorSeed>);

enum ServiceActorLoopNext {
    Wait,
    DontWait,
}

impl Actor for ServiceActor {
    fn init(&mut self, jobs: &BackgroundJobQueue) {
        let seed = self.0.clone();
        let mut current = seed.persistent_container.state.subscribe();
        jobs.add_job(async move {
            let _ = current.wait_for(|s| s.rt_initialized).await;

            loop {
                match service_actor_loop(&current, &seed).await {
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
) -> ServiceActorLoopNext {
    let id = &seed.id;
    let kinds = current.borrow().kinds();
    if let Err(e) = async {
        let major_changes_state = seed
            .ctx
            .db
            .mutate(|d| {
                if let Some(i) = d.as_public_mut().as_package_data_mut().as_idx_mut(&id) {
                    let previous = i.as_status().de()?;
                    let main_status = match &kinds {
                        ServiceStateKinds {
                            transition_state: Some(TransitionKind::Restarting),
                            ..
                        } => MainStatus::Restarting,
                        ServiceStateKinds {
                            transition_state: Some(TransitionKind::Restoring),
                            ..
                        } => MainStatus::Restoring,
                        ServiceStateKinds {
                            transition_state: Some(TransitionKind::BackingUp),
                            ..
                        } => previous.backing_up(),
                        ServiceStateKinds {
                            running_status: Some(status),
                            desired_state: StartStop::Start,
                            ..
                        } => MainStatus::Running {
                            started: status.started,
                            health: previous.health().cloned().unwrap_or_default(),
                        },
                        ServiceStateKinds {
                            running_status: None,
                            desired_state: StartStop::Start,
                            ..
                        } => MainStatus::Starting {
                            health: previous.health().cloned().unwrap_or_default(),
                        },
                        ServiceStateKinds {
                            running_status: Some(_),
                            desired_state: StartStop::Stop,
                            ..
                        } => MainStatus::Stopping,
                        ServiceStateKinds {
                            running_status: None,
                            desired_state: StartStop::Stop,
                            ..
                        } => MainStatus::Stopped,
                    };
                    let previous = i.as_status().de()?;
                    i.as_status_mut().ser(&main_status)?;
                    return Ok(previous
                        .major_changes(&main_status)
                        .then_some((previous, main_status)));
                }
                Ok(None)
            })
            .await?;
        if let Some((previous, new_state)) = major_changes_state {
            if let Some(callbacks) = seed.ctx.callbacks.get_status(id) {
                callbacks
                    .call(vector![to_value(&previous)?, to_value(&new_state)?])
                    .await?;
            }
        }
        seed.synchronized.notify_waiters();

        match kinds {
            ServiceStateKinds {
                running_status: None,
                desired_state: StartStop::Start,
                ..
            } => {
                seed.persistent_container.start().await?;
            }
            ServiceStateKinds {
                running_status: Some(_),
                desired_state: StartStop::Stop,
                ..
            } => {
                seed.persistent_container.stop().await?;
                seed.persistent_container
                    .state
                    .send_if_modified(|s| s.running_status.take().is_some());
            }
            _ => (),
        };

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
