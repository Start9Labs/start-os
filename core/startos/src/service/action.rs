use std::time::Duration;

use models::{ActionId, ProcedureName};

use crate::action::ActionResult;
use crate::prelude::*;
use crate::rpc_continuations::Guid;
use crate::service::config::GetConfig;
use crate::service::dependencies::DependencyConfig;
use crate::service::{Service, ServiceActor};
use crate::util::actor::background::BackgroundJobQueue;
use crate::util::actor::{ConflictBuilder, Handler};

pub(super) struct Action {
    id: ActionId,
    input: Value,
}
impl Handler<Action> for ServiceActor {
    type Response = Result<ActionResult, Error>;
    fn conflicts_with(_: &Action) -> ConflictBuilder<Self> {
        ConflictBuilder::everything()
            .except::<GetConfig>()
            .except::<DependencyConfig>()
    }
    async fn handle(
        &mut self,
        id: Guid,
        Action {
            id: action_id,
            input,
        }: Action,
        _: &BackgroundJobQueue,
    ) -> Self::Response {
        let container = &self.0.persistent_container;
        container
            .execute::<ActionResult>(
                id,
                ProcedureName::RunAction(action_id),
                input,
                Some(Duration::from_secs(30)),
            )
            .await
            .with_kind(ErrorKind::Action)
    }
}

impl Service {
    pub async fn action(
        &self,
        id: Guid,
        action_id: ActionId,
        input: Value,
    ) -> Result<ActionResult, Error> {
        self.actor
            .send(
                id,
                Action {
                    id: action_id,
                    input,
                },
            )
            .await?
    }
}
