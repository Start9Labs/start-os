use futures::FutureExt;

use crate::prelude::*;
use crate::service::start_stop::StartStop;
use crate::service::transition::{TransitionKind, TransitionState};
use crate::service::{Service, ServiceActor};
use crate::util::actor::{BackgroundJobs, Handler};
use crate::util::future::RemoteCancellable;

struct Restart;
#[async_trait::async_trait]
impl Handler<Restart> for ServiceActor {
    type Response = ();
    async fn handle(&mut self, _: Restart, jobs: &mut BackgroundJobs) -> Self::Response {
        let temp = self.0.temp_desired_state.clone();
        let current = self.0.persistent_container.current_state.subscribe();
        let transition = RemoteCancellable::new(async move {
            temp.stop();
            current.wait_for(|s| *s == StartStop::Stop).await;
            temp.start();
            current.wait_for(|s| *s == StartStop::Start).await;
        });
        let cancel_handle = transition.cancellation_handle();
        jobs.add_job(transition.map(|_| ()));
        let notified = self.0.synchronized.notified();
        if let Some(t) = self.0.transition_state.send_replace(Some(TransitionState {
            kind: TransitionKind::Restarting,
            cancel_handle,
        })) {
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
