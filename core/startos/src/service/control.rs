use crate::prelude::*;
use crate::service::start_stop::StartStop;
use crate::service::transition::TransitionKind;
use crate::service::{Service, ServiceActor};
use crate::util::actor::{BackgroundJobs, Handler};

struct Start;
#[async_trait::async_trait]
impl Handler<Start> for ServiceActor {
    type Response = ();
    async fn handle(&mut self, _: Start, _: &mut BackgroundJobs) -> Self::Response {
        self.0.desired_state.send_replace(StartStop::Start);
        self.0.synchronized.notified().await
    }
}
impl Service {
    pub async fn start(&self) -> Result<(), Error> {
        self.actor.send(Start).await
    }
}

struct Stop;
#[async_trait::async_trait]
impl Handler<Stop> for ServiceActor {
    type Response = ();
    async fn handle(&mut self, _: Stop, _: &mut BackgroundJobs) -> Self::Response {
        self.0.desired_state.send_replace(StartStop::Stop);
        if self.0.transition_state.borrow().as_ref().map(|t| t.kind())
            == Some(TransitionKind::Restarting)
        {
            if let Some(restart) = self.0.transition_state.send_replace(None) {
                restart.abort().await;
            } else {
                #[cfg(feature = "unstable")]
                unreachable!()
            }
        }
        self.0.synchronized.notified().await
    }
}
impl Service {
    pub async fn stop(&self) -> Result<(), Error> {
        self.actor.send(Stop).await
    }
}
