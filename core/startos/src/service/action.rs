use std::time::Duration;

use models::{ActionId, ProcedureName};

use crate::action::ActionResult;
use crate::prelude::*;
use crate::service::{Service, ServiceActor};
use crate::util::actor::{BackgroundJobs, Handler};

struct Action {
    id: ActionId,
    input: Value,
}
impl Handler<Action> for ServiceActor {
    type Response = Result<ActionResult, Error>;
    async fn handle(
        &mut self,
        Action { id, input }: Action,
        _: &mut BackgroundJobs,
    ) -> Self::Response {
        let container = &self.0.persistent_container;
        container
            .execute::<ActionResult>(
                ProcedureName::RunAction(id),
                input,
                Some(Duration::from_secs(30)),
            )
            .await
            .with_kind(ErrorKind::Action)
    }
}

impl Service {
    pub async fn action(&self, id: ActionId, input: Value) -> Result<ActionResult, Error> {
        self.actor.send(Action { id, input }).await?
    }
}
