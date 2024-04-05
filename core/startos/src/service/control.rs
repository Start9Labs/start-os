use crate::prelude::*;
use crate::service::config::GetConfig;
use crate::service::dependencies::DependencyConfig;
use crate::service::start_stop::StartStop;
use crate::service::transition::TransitionKind;
use crate::service::{Service, ServiceActor};
use crate::util::actor::background::BackgroundJobQueue;
use crate::util::actor::{ConflictBuilder, Handler};

pub(super) struct Start;
impl Handler<Start> for ServiceActor {
    type Response = ();
    fn conflicts_with(_: &Start) -> ConflictBuilder<Self> {
        ConflictBuilder::everything()
            .except::<GetConfig>()
            .except::<DependencyConfig>()
    }
    async fn handle(&mut self, _: Start, _: &BackgroundJobQueue) -> Self::Response {
        self.0.persistent_container.state.send_modify(|x| {
            x.desired_state = StartStop::Start;
        });
        self.0.synchronized.notified().await
    }
}
impl Service {
    pub async fn start(&self) -> Result<(), Error> {
        self.actor.send(Start).await
    }
}

struct Stop;
impl Handler<Stop> for ServiceActor {
    type Response = ();
    fn conflicts_with(_: &Stop) -> ConflictBuilder<Self> {
        ConflictBuilder::everything()
            .except::<GetConfig>()
            .except::<DependencyConfig>()
    }
    async fn handle(&mut self, _: Stop, _: &BackgroundJobQueue) -> Self::Response {
        let mut transition_state = None;
        self.0.persistent_container.state.send_modify(|x| {
            x.desired_state = StartStop::Stop;
            if x.transition_state.as_ref().map(|x| x.kind()) == Some(TransitionKind::Restarting) {
                transition_state = std::mem::take(&mut x.transition_state);
            }
        });
        if let Some(restart) = transition_state {
            restart.abort().await;
        }
        self.0.synchronized.notified().await
    }
}
impl Service {
    pub async fn stop(&self) -> Result<(), Error> {
        self.actor.send(Stop).await
    }
}
