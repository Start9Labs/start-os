use std::time::Duration;

use imbl_value::json;
use models::{PackageId, ProcedureName};

use crate::prelude::*;
use crate::service::{Service, ServiceActor};
use crate::util::actor::{BackgroundJobs, Handler};
use crate::Config;

struct DependencyConfig {
    dependency_id: PackageId,
    remote_config: Option<Config>,
}
impl Handler<DependencyConfig> for ServiceActor {
    type Response = Result<Option<Config>, Error>;
    async fn handle(
        &mut self,
        DependencyConfig {
            dependency_id,
            remote_config,
        }: DependencyConfig,
        _: &mut BackgroundJobs,
    ) -> Self::Response {
        let container = &self.0.persistent_container;
        container
            .sanboxed::<Option<Config>>(
                ProcedureName::UpdateDependency(dependency_id.clone()),
                json!({
                    "queryResults": container
                        .execute::<Value>(
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

impl Service {
    pub async fn dependency_config(
        &self,
        dependency_id: PackageId,
        remote_config: Option<Config>,
    ) -> Result<Option<Config>, Error> {
        self.actor
            .send(DependencyConfig {
                dependency_id,
                remote_config,
            })
            .await?
    }
}
