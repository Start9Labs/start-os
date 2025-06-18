use std::collections::BTreeMap;
use std::time::Duration;

use imbl_value::json;
use models::{ActionId, PackageId, ProcedureName, ReplayId};

use crate::action::{ActionInput, ActionResult};
use crate::db::model::package::{
    ActionVisibility, AllowedStatuses, TaskCondition, TaskEntry, TaskInput, TaskSeverity,
};
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

pub fn update_tasks(
    tasks: &mut BTreeMap<ReplayId, TaskEntry>,
    package_id: &PackageId,
    action_id: &ActionId,
    input: &Value,
    was_run: bool,
) -> bool {
    let mut critical_activated = false;
    tasks.retain(|_, v| {
        if &v.task.package_id != package_id || &v.task.action_id != action_id {
            return true;
        }
        if let Some(when) = &v.task.when {
            match &when.condition {
                TaskCondition::InputNotMatches => match &v.task.input {
                    Some(TaskInput::Partial { value }) => {
                        if is_partial_of(value, input) {
                            if when.once {
                                return !was_run;
                            } else {
                                v.active = false;
                            }
                        } else {
                            v.active = true;
                            if v.task.severity == TaskSeverity::Critical {
                                critical_activated = true;
                            }
                        }
                    }
                    None => {
                        tracing::error!("action request exists in an invalid state {:?}", v.task);
                    }
                },
            }
            true
        } else {
            !was_run
        }
    });
    critical_activated
}

pub(super) struct RunAction {
    id: ActionId,
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
            id: ref action_id,
            input,
        }: RunAction,
        jobs: &BackgroundJobQueue,
    ) -> Self::Response {
        let container = &self.0.persistent_container;
        let package_id = &self.0.id;
        let action = self
            .0
            .ctx
            .db
            .peek()
            .await
            .into_public()
            .into_package_data()
            .into_idx(package_id)
            .or_not_found(package_id)?
            .into_actions()
            .into_idx(&action_id)
            .or_not_found(lazy_format!("{package_id} action {action_id}"))?
            .de()?;
        if matches!(&action.visibility, ActionVisibility::Disabled(_)) {
            return Err(Error::new(
                eyre!("action {action_id} is disabled"),
                ErrorKind::Action,
            ));
        }
        let running = container.state.borrow().running_status.as_ref().is_some();
        if match action.allowed_statuses {
            AllowedStatuses::OnlyRunning => !running,
            AllowedStatuses::OnlyStopped => running,
            _ => false,
        } {
            return Err(Error::new(
                eyre!("service is not in allowed status for {action_id}"),
                ErrorKind::Action,
            ));
        }
        let result = container
            .execute::<Option<ActionResult>>(
                id.clone(),
                ProcedureName::RunAction(action_id.clone()),
                json!({
                    "input": input,
                }),
                Some(Duration::from_secs(30)),
            )
            .await
            .with_kind(ErrorKind::Action)?;
        if self
            .0
            .ctx
            .db
            .mutate(|db| {
                let mut critical_activated = false;
                for (_, pde) in db.as_public_mut().as_package_data_mut().as_entries_mut()? {
                    critical_activated |= pde.as_tasks_mut().mutate(|tasks| {
                        Ok(update_tasks(tasks, package_id, action_id, &input, true))
                    })?;
                }
                Ok(critical_activated)
            })
            .await
            .result?
        {
            <Self as Handler<super::control::Stop>>::handle(
                self,
                id,
                super::control::Stop { wait: false },
                jobs,
            )
            .await;
        }
        Ok(result)
    }
}

impl Service {
    pub async fn run_action(
        &self,
        id: Guid,
        action_id: ActionId,
        input: Value,
    ) -> Result<Option<ActionResult>, Error> {
        self.actor
            .send(
                id,
                RunAction {
                    id: action_id,
                    input,
                },
            )
            .await?
    }
}
