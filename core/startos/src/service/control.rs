use futures::future::OptionFuture;

use crate::prelude::*;
use crate::rpc_continuations::Guid;
use crate::service::action::RunAction;
use crate::service::start_stop::StartStop;
use crate::service::transition::TransitionKind;
use crate::service::{Service, ServiceActor};
use crate::util::actor::background::BackgroundJobQueue;
use crate::util::actor::{ConflictBuilder, Handler};

pub(super) struct Start;
impl Handler<Start> for ServiceActor {
    type Response = ();
    fn conflicts_with(_: &Start) -> ConflictBuilder<Self> {
        ConflictBuilder::everything().except::<RunAction>()
    }
    async fn handle(&mut self, _: Guid, _: Start, _: &BackgroundJobQueue) -> Self::Response {
        self.0.persistent_container.state.send_modify(|x| {
            x.desired_state = StartStop::Start;
        });
        self.0.synchronized.notified().await
    }
}
impl Service {
    pub async fn start(&self, id: Guid) -> Result<(), Error> {
        self.actor.send(id, Start).await
    }
}

pub(super) struct Stop {
    pub wait: bool,
}
impl Handler<Stop> for ServiceActor {
    type Response = ();
    fn conflicts_with(_: &Stop) -> ConflictBuilder<Self> {
        ConflictBuilder::everything().except::<RunAction>()
    }
    async fn handle(
        &mut self,
        _: Guid,
        Stop { wait }: Stop,
        _: &BackgroundJobQueue,
    ) -> Self::Response {
        let mut transition_state = None;
        self.0.persistent_container.state.send_modify(|x| {
            x.desired_state = StartStop::Stop;
            if x.transition_state.as_ref().map(|x| x.kind()) == Some(TransitionKind::Restarting) {
                transition_state = std::mem::take(&mut x.transition_state);
            }
        });
        let notif = if wait {
            Some(self.0.synchronized.notified())
        } else {
            None
        };
        if let Some(restart) = transition_state {
            restart.abort().await;
        }
        OptionFuture::from(notif).await;
    }
}
impl Service {
    pub async fn stop(&self, id: Guid, wait: bool) -> Result<(), Error> {
        self.actor.send(id, Stop { wait }).await
    }
}
