use std::sync::Arc;

use futures::FutureExt;

use crate::prelude::*;
use crate::service::transition::{TransitionKind, TransitionState};
use crate::service::{Service, ServiceActor};
use crate::util::actor::{BackgroundJobs, Handler};
use crate::util::future::RemoteCancellable;

use super::TempDesiredState;

struct Restart;
#[async_trait::async_trait]
impl Handler<Restart> for ServiceActor {
    type Response = ();
    async fn handle(&mut self, _: Restart, jobs: &mut BackgroundJobs) -> Self::Response {
        // So Need a handle to just a single field in the state
        let temp = TempDesiredState::new(&self.0.persistent_container.state);
        let mut current = self.0.persistent_container.state.subscribe();
        let transition = RemoteCancellable::new(async move {
            temp.stop();
            current.wait_for(|s| s.running_status.is_none()).await;
            temp.start();
            current.wait_for(|s| s.running_status.is_some()).await;
            drop(temp);
        });
        let cancel_handle = transition.cancellation_handle();
        jobs.add_job(transition.map(|_| ()));
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
        notified.await
    }
}
impl Service {
    pub async fn restart(&self) -> Result<(), Error> {
        self.actor.send(Restart).await
    }
}
