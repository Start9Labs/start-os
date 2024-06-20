use std::time::Duration;

use imbl_value::json;
use models::{PackageId, ProcedureName};

use crate::prelude::*;
use crate::rpc_continuations::Guid;
use crate::service::{Service, ServiceActor, ServiceActorSeed};
use crate::util::actor::background::BackgroundJobQueue;
use crate::util::actor::{ConflictBuilder, Handler};
use crate::Config;

impl ServiceActorSeed {
    async fn dependency_config(
        &self,
        id: Guid,
        dependency_id: PackageId,
        remote_config: Option<Config>,
    ) -> Result<Option<Config>, Error> {
        let container = &self.persistent_container;
        container
            .sanboxed::<Option<Config>>(
                id.clone(),
                ProcedureName::UpdateDependency(dependency_id.clone()),
                json!({
                    "queryResults": container
                        .execute::<Value>(
                            id,
                            ProcedureName::QueryDependency(dependency_id),
                            Value::Null,
                            Some(Duration::from_secs(30)),
                        )
                        .await
                        .with_kind(ErrorKind::Dependency)?,
                    "remoteConfig": remote_config,
                }),
                Some(Duration::from_secs(30)),
            )
            .await
            .with_kind(ErrorKind::Dependency)
            .map(|res| res.filter(|c| !c.is_empty()))
    }
}

pub(super) struct DependencyConfig {
    dependency_id: PackageId,
    remote_config: Option<Config>,
}
impl Handler<DependencyConfig> for ServiceActor {
    type Response = Result<Option<Config>, Error>;
    fn conflicts_with(_: &DependencyConfig) -> ConflictBuilder<Self> {
        ConflictBuilder::nothing()
    }
    async fn handle(
        &mut self,
        id: Guid,
        DependencyConfig {
            dependency_id,
            remote_config,
        }: DependencyConfig,
        _: &BackgroundJobQueue,
    ) -> Self::Response {
        self.0
            .dependency_config(id, dependency_id, remote_config)
            .await
    }
}

impl Service {
    pub async fn dependency_config(
        &self,
        id: Guid,
        dependency_id: PackageId,
        remote_config: Option<Config>,
    ) -> Result<Option<Config>, Error> {
        self.actor
            .send(
                id,
                DependencyConfig {
                    dependency_id,
                    remote_config,
                },
            )
            .await?
    }
}
