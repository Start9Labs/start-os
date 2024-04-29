use futures::FutureExt;

use super::TempDesiredRestore;
use crate::prelude::*;
use crate::service::config::GetConfig;
use crate::service::dependencies::DependencyConfig;
use crate::service::transition::{TransitionKind, TransitionState};
use crate::service::{Service, ServiceActor};
use crate::util::actor::background::BackgroundJobQueue;
use crate::util::actor::{ConflictBuilder, Handler};
use crate::util::future::RemoteCancellable;

pub(super) struct Restart;
impl Handler<Restart> for ServiceActor {
    type Response = ();
    fn conflicts_with(_: &Restart) -> ConflictBuilder<Self> {
        ConflictBuilder::everything()
            .except::<GetConfig>()
            .except::<DependencyConfig>()
    }
    async fn handle(&mut self, _: Restart, jobs: &BackgroundJobQueue) -> Self::Response {
        // So Need a handle to just a single field in the state
        let temp = TempDesiredRestore::new(&self.0.persistent_container.state);
        let mut current = self.0.persistent_container.state.subscribe();
        let transition = RemoteCancellable::new(async move {
            temp.stop();
            current
                .wait_for(|s| s.running_status.is_none())
                .await
                .with_kind(ErrorKind::Unknown)?;
            if temp.restore().is_start() {
                current
                    .wait_for(|s| s.running_status.is_some())
                    .await
                    .with_kind(ErrorKind::Unknown)?;
            }
            drop(temp);
            Ok::<_, Error>(())
        });
        let cancel_handle = transition.cancellation_handle();
        jobs.add_job(transition.map(|x| {
            if let Some(Err(err)) = x {
                tracing::debug!("{:?}", err);
                tracing::warn!("{}", err);
            }
        }));
        let notified = self.0.synchronized.notified();

        let mut old = None;
        self.0.persistent_container.state.send_modify(|s| {
            old = std::mem::replace(
                &mut s.transition_state,
                Some(TransitionState {
                    kind: TransitionKind::Restarting,
                    cancel_handle,
                }),
            )
        });
        if let Some(t) = old {
            t.abort().await;
        }
        notified.await;

        self.0.persistent_container.state.send_modify(|s| {
            s.transition_state.take();
        });
    }
}
impl Service {
    #[instrument(skip_all)]
    pub async fn restart(&self) -> Result<(), Error> {
        self.actor.send(Restart).await
    }
}
