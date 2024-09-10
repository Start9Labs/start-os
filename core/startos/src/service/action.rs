use std::time::Duration;

use imbl_value::json;
use models::{ActionId, ProcedureName};

use crate::action::{ActionInput, ActionResult};
use crate::prelude::*;
use crate::rpc_continuations::Guid;
use crate::service::{Service, ServiceActor};
use crate::util::actor::background::BackgroundJobQueue;
use crate::util::actor::{ConflictBuilder, Handler};

pub(super) struct GetActionInput {
    id: ActionId,
}
impl Handler<GetActionInput> for ServiceActor {
    type Response = Result<Option<ActionInput>, Error>;
    fn conflicts_with(_: &GetActionInput) -> ConflictBuilder<Self> {
        ConflictBuilder::nothing()
    }
    async fn handle(
        &mut self,
        id: Guid,
        GetActionInput { id: action_id }: GetActionInput,
        _: &BackgroundJobQueue,
    ) -> Self::Response {
        let container = &self.0.persistent_container;
        container
            .execute::<Option<ActionInput>>(
                id,
                ProcedureName::GetActionInput(action_id),
                Value::Null,
                Some(Duration::from_secs(30)),
            )
            .await
            .with_kind(ErrorKind::Action)
    }
}

impl Service {
    pub async fn get_action_input(
        &self,
        id: Guid,
        action_id: ActionId,
    ) -> Result<Option<ActionInput>, Error> {
        self.actor
            .send(id, GetActionInput { id: action_id })
            .await?
    }
}

pub(super) struct RunAction {
    id: ActionId,
    prev: Option<ActionInput>,
    input: Value,
}
impl Handler<RunAction> for ServiceActor {
    type Response = Result<Option<ActionResult>, Error>;
    fn conflicts_with(_: &RunAction) -> ConflictBuilder<Self> {
        ConflictBuilder::everything().except::<GetActionInput>()
    }
    async fn handle(
        &mut self,
        id: Guid,
        RunAction {
            id: action_id,
            prev,
            input,
        }: RunAction,
        _: &BackgroundJobQueue,
    ) -> Self::Response {
        let container = &self.0.persistent_container;
        container
            .execute::<Option<ActionResult>>(
                id,
                ProcedureName::RunAction(action_id),
                json!({
                    "prev": prev,
                    "input": input,
                }),
                Some(Duration::from_secs(30)),
            )
            .await
            .with_kind(ErrorKind::Action)
    }
}

impl Service {
    pub async fn run_action(
        &self,
        id: Guid,
        action_id: ActionId,
        prev: Option<ActionInput>,
        input: Value,
    ) -> Result<Option<ActionResult>, Error> {
        self.actor
            .send(
                id,
                RunAction {
                    id: action_id,
                    prev,
                    input,
                },
            )
            .await?
    }
}
