use std::sync::Arc;
use std::time::Duration;

use futures::FutureExt;
use futures::future::{BoxFuture, Either};
use imbl::vector;

use super::ServiceActorSeed;
use super::start_stop::StartStop;
use crate::prelude::*;
use crate::service::SYNC_RETRY_COOLDOWN_SECONDS;
use crate::service::persistent_container::ServiceStateKinds;
use crate::service::transition::TransitionKind;
use crate::status::MainStatus;
use crate::util::actor::Actor;
use crate::util::actor::background::BackgroundJobQueue;

#[derive(Clone)]
pub(super) struct ServiceActor(pub(super) Arc<ServiceActorSeed>);

impl Actor for ServiceActor {
    fn init(&mut self, jobs: &BackgroundJobQueue) {
        let seed = self.0.clone();
        let mut current = seed.persistent_container.state.subscribe();
        jobs.add_job(async move {
            let _ = current.wait_for(|s| s.rt_initialized).await;
            let mut start_stop_task: Option<Either<_, _>> = None;

            loop {
                let wait = match service_actor_loop(&current, &seed, &mut start_stop_task).await {
                    Ok(()) => Either::Right(current.changed().then(|res| async move {
                        match res {
                            Ok(()) => (),
                            Err(_) => futures::future::pending().await,
                        }
                    })),
                    Err(e) => {
                        tracing::error!("error synchronizing state of service: {e}");
                        tracing::debug!("{e:?}");

                        seed.synchronized.notify_waiters();

                        tracing::error!("Retrying in {}s...", SYNC_RETRY_COOLDOWN_SECONDS);
                        Either::Left(tokio::time::sleep(Duration::from_secs(
                            SYNC_RETRY_COOLDOWN_SECONDS,
                        )))
                    }
                };
                tokio::pin!(wait);
                let start_stop_handler = async {
                    match &mut start_stop_task {
                        Some(task) => {
                            let err = task.await.log_err().is_none(); // TODO: ideally this error should be sent to service logs
                            start_stop_task.take();
                            if err {
                                tokio::time::sleep(Duration::from_secs(
                                    SYNC_RETRY_COOLDOWN_SECONDS,
                                ))
                                .await;
                            }
                        }
                        _ => futures::future::pending().await,
                    }
                };
                tokio::pin!(start_stop_handler);
                futures::future::select(wait, start_stop_handler).await;
            }
        });
    }
}

async fn service_actor_loop<'a>(
    current: &tokio::sync::watch::Receiver<super::persistent_container::ServiceState>,
    seed: &'a Arc<ServiceActorSeed>,
    start_stop_task: &mut Option<
        Either<BoxFuture<'a, Result<(), Error>>, BoxFuture<'a, Result<(), Error>>>,
    >,
) -> Result<(), Error> {
    let id = &seed.id;
    let kinds = current.borrow().kinds();

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
                i.as_status_mut().ser(&main_status)?;
                return Ok(previous
                    .major_changes(&main_status)
                    .then_some((previous, main_status)));
            }
            Ok(None)
        })
        .await
        .result?;

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
            let task = start_stop_task
                .take()
                .filter(|task| matches!(task, Either::Right(_)));
            *start_stop_task = Some(
                task.unwrap_or_else(|| Either::Right(seed.persistent_container.start().boxed())),
            );
        }
        ServiceStateKinds {
            running_status: Some(_),
            desired_state: StartStop::Stop,
            ..
        } => {
            let task = start_stop_task
                .take()
                .filter(|task| matches!(task, Either::Left(_)));
            *start_stop_task = Some(task.unwrap_or_else(|| {
                Either::Left(
                    async {
                        seed.persistent_container.stop().await?;
                        seed.persistent_container
                            .state
                            .send_if_modified(|s| s.running_status.take().is_some());
                        Ok::<_, Error>(())
                    }
                    .boxed(),
                )
            }));
        }
        _ => (),
    };
    Ok(())
}
