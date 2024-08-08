use std::time::Duration;

use models::ProcedureName;

use crate::config::action::ConfigRes;
use crate::config::ConfigureContext;
use crate::prelude::*;
use crate::rpc_continuations::Guid;
use crate::service::dependencies::DependencyConfig;
use crate::service::{Service, ServiceActor};
use crate::util::actor::background::BackgroundJobQueue;
use crate::util::actor::{ConflictBuilder, Handler};
use crate::util::serde::NoOutput;

pub(super) struct Configure(ConfigureContext);
impl Handler<Configure> for ServiceActor {
    type Response = Result<(), Error>;
    fn conflicts_with(_: &Configure) -> ConflictBuilder<Self> {
        ConflictBuilder::everything().except::<DependencyConfig>()
    }
    async fn handle(
        &mut self,
        id: Guid,
        Configure(ConfigureContext { timeout, config }): Configure,
        _: &BackgroundJobQueue,
    ) -> Self::Response {
        let container = &self.0.persistent_container;
        let package_id = &self.0.id;

        container
            .execute::<NoOutput>(id, ProcedureName::SetConfig, to_value(&config)?, timeout)
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

pub(super) struct GetConfig;
impl Handler<GetConfig> for ServiceActor {
    type Response = Result<ConfigRes, Error>;
    fn conflicts_with(_: &GetConfig) -> ConflictBuilder<Self> {
        ConflictBuilder::nothing().except::<Configure>()
    }
    async fn handle(&mut self, id: Guid, _: GetConfig, _: &BackgroundJobQueue) -> Self::Response {
        let container = &self.0.persistent_container;
        container
            .execute::<ConfigRes>(
                id,
                ProcedureName::GetConfig,
                Value::Null,
                Some(Duration::from_secs(30)), // TODO timeout
            )
            .await
            .with_kind(ErrorKind::ConfigRulesViolation)
    }
}

impl Service {
    pub async fn configure(&self, id: Guid, ctx: ConfigureContext) -> Result<(), Error> {
        self.actor.send(id, Configure(ctx)).await?
    }
    pub async fn get_config(&self, id: Guid) -> Result<ConfigRes, Error> {
        self.actor.send(id, GetConfig).await?
    }
}
