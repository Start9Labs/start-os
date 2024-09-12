use std::collections::BTreeMap;
use std::time::Duration;

use imbl_value::{json, InternedString};
use models::{ActionId, PackageId, ProcedureName};

use crate::action::{ActionInput, ActionResult};
use crate::db::model::package::{ActionRequestCondition, ActionRequestEntry, ActionRequestInput};
use crate::prelude::*;
use crate::rpc_continuations::Guid;
use crate::service::{Service, ServiceActor};
use crate::util::actor::background::BackgroundJobQueue;
use crate::util::actor::{ConflictBuilder, Handler};
use crate::util::serde::is_partial_of;

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
        if !self
            .seed
            .ctx
            .db
            .peek()
            .await
            .as_public()
            .as_package_data()
            .as_idx(&self.seed.id)
            .or_not_found(&self.seed.id)?
            .as_actions()
            .as_idx(&action_id)
            .or_not_found(&action_id)?
            .as_has_input()
            .de()?
        {
            return Ok(None);
        }
        self.actor
            .send(id, GetActionInput { id: action_id })
            .await?
    }
}

pub fn update_requested_actions(
    requested_actions: &mut BTreeMap<InternedString, ActionRequestEntry>,
    package_id: &PackageId,
    action_id: &ActionId,
    input: &Value,
) {
    requested_actions.retain(|_, v| {
        if &v.request.package_id != package_id || &v.request.action_id != action_id {
            return true;
        }
        if let Some(when) = &v.request.when {
            match &when.condition {
                ActionRequestCondition::InputNotMatches => match &v.request.input {
                    Some(ActionRequestInput::Partial { value }) => {
                        if is_partial_of(value, input) {
                            if when.once {
                                return false;
                            } else {
                                v.active = false;
                            }
                        } else {
                            v.active = true;
                        }
                    }
                    None => {
                        tracing::error!(
                            "action request exists in an invalid state {:?}",
                            v.request
                        );
                    }
                },
            }
        }
        true
    })
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
        let result = container
            .execute::<Option<ActionResult>>(
                id,
                ProcedureName::RunAction(action_id.clone()),
                json!({
                    "prev": prev,
                    "input": input,
                }),
                Some(Duration::from_secs(30)),
            )
            .await
            .with_kind(ErrorKind::Action)?;
        let package_id = &self.0.id;
        self.0
            .ctx
            .db
            .mutate(|db| {
                for (_, pde) in db.as_public_mut().as_package_data_mut().as_entries_mut()? {
                    pde.as_requested_actions_mut().mutate(|requested_actions| {
                        Ok(update_requested_actions(
                            requested_actions,
                            package_id,
                            &action_id,
                            &input,
                        ))
                    })?;
                }
                Ok(())
            })
            .await?;
        Ok(result)
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
