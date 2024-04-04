use std::time::Duration;

use models::ProcedureName;

use crate::config::action::ConfigRes;
use crate::config::{action::SetResult, ConfigureContext};
use crate::prelude::*;
use crate::service::{Service, ServiceActor};
use crate::util::actor::{BackgroundJobs, Handler};
use crate::util::serde::NoOutput;

struct Configure(ConfigureContext);
impl Handler<Configure> for ServiceActor {
    type Response = Result<(), Error>;
    async fn handle(
        &mut self,
        Configure(ConfigureContext { timeout, config }): Configure,
        _: &mut BackgroundJobs,
    ) -> Self::Response {
        let container = &self.0.persistent_container;
        let package_id = &self.0.id;

        container
            .execute::<NoOutput>(ProcedureName::SetConfig, to_value(&config)?, timeout)
            .await
            .with_kind(ErrorKind::ConfigRulesViolation)?;
        self.0
            .ctx
            .db
            .mutate(move |db| {
                db.as_public_mut()
                    .as_package_data_mut()
                    .as_idx_mut(package_id)
                    .or_not_found(package_id)?
                    .as_status_mut()
                    .as_configured_mut()
                    .ser(&true)
            })
            .await?;
        Ok(())
    }
}

struct GetConfig;
impl Handler<GetConfig> for ServiceActor {
    type Response = Result<ConfigRes, Error>;
    async fn handle(&mut self, _: GetConfig, _: &mut BackgroundJobs) -> Self::Response {
        let container = &self.0.persistent_container;
        container
            .execute::<ConfigRes>(
                ProcedureName::GetConfig,
                Value::Null,
                Some(Duration::from_secs(30)), // TODO timeout
            )
            .await
            .with_kind(ErrorKind::ConfigRulesViolation)
    }
}

impl Service {
    pub async fn configure(&self, ctx: ConfigureContext) -> Result<(), Error> {
        self.actor.send(Configure(ctx)).await?
    }
    pub async fn get_config(&self) -> Result<ConfigRes, Error> {
        self.actor.send(GetConfig).await?
    }
}
