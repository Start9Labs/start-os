use std::collections::{BTreeMap, BTreeSet};

use chrono::{DateTime, Utc};
pub use models::HealthCheckId;
use serde::{Deserialize, Serialize};
use tracing::instrument;

use crate::id::ImageId;
use crate::procedure::{NoOutput, PackageProcedure, ProcedureName};
use crate::s9pk::manifest::PackageId;
use crate::util::serde::Duration;
use crate::util::Version;
use crate::volume::Volumes;
use crate::{context::RpcContext, procedure::docker::DockerContainer};
use crate::{Error, ResultExt};

#[derive(Clone, Debug, Deserialize, Serialize)]
pub struct HealthChecks(pub BTreeMap<HealthCheckId, HealthCheck>);
impl HealthChecks {
    #[instrument]
    pub fn validate(
        &self,
        container: &Option<DockerContainer>,
        eos_version: &Version,
        volumes: &Volumes,
        image_ids: &BTreeSet<ImageId>,
    ) -> Result<(), Error> {
        for (_, check) in &self.0 {
            check
                .implementation
                .validate(container, eos_version, &volumes, image_ids, false)
                .with_ctx(|_| {
                    (
                        crate::ErrorKind::ValidateS9pk,
                        format!("Health Check {}", check.name),
                    )
                })?;
        }
        Ok(())
    }
    pub async fn check_all(
        &self,
        ctx: &RpcContext,
        container: &Option<DockerContainer>,
        started: DateTime<Utc>,
        pkg_id: &PackageId,
        pkg_version: &Version,
        volumes: &Volumes,
    ) -> Result<BTreeMap<HealthCheckId, HealthCheckResult>, Error> {
        let res = futures::future::try_join_all(self.0.iter().map(|(id, check)| async move {
            Ok::<_, Error>((
                id.clone(),
                check
                    .check(ctx, container, id, started, pkg_id, pkg_version, volumes)
                    .await?,
            ))
        }))
        .await?;
        Ok(res.into_iter().collect())
    }
}

#[derive(Clone, Debug, Deserialize, Serialize)]
#[serde(rename_all = "kebab-case")]
pub struct HealthCheck {
    pub name: String,
    pub success_message: Option<String>,
    #[serde(flatten)]
    implementation: PackageProcedure,
    pub timeout: Option<Duration>,
}
impl HealthCheck {
    #[instrument(skip(ctx))]
    pub async fn check(
        &self,
        ctx: &RpcContext,
        container: &Option<DockerContainer>,
        id: &HealthCheckId,
        started: DateTime<Utc>,
        pkg_id: &PackageId,
        pkg_version: &Version,
        volumes: &Volumes,
    ) -> Result<HealthCheckResult, Error> {
        let res = self
            .implementation
            .execute(
                ctx,
                container,
                pkg_id,
                pkg_version,
                ProcedureName::Health(id.clone()),
                volumes,
                Some(Utc::now().signed_duration_since(started).num_milliseconds()),
                Some(
                    self.timeout
                        .map_or(std::time::Duration::from_secs(30), |d| *d),
                ),
            )
            .await?;
        Ok(match res {
            Ok(NoOutput) => HealthCheckResult::Success,
            Err((59, _)) => HealthCheckResult::Disabled,
            Err((60, _)) => HealthCheckResult::Starting,
            Err((61, message)) => HealthCheckResult::Loading { message },
            Err((_, error)) => HealthCheckResult::Failure { error },
        })
    }
}

#[derive(Clone, Debug, Deserialize, Serialize)]
#[serde(rename_all = "kebab-case")]
#[serde(tag = "result")]
pub enum HealthCheckResult {
    Success,
    Disabled,
    Starting,
    Loading { message: String },
    Failure { error: String },
}
impl std::fmt::Display for HealthCheckResult {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            HealthCheckResult::Success => write!(f, "Succeeded"),
            HealthCheckResult::Disabled => write!(f, "Disabled"),
            HealthCheckResult::Starting => write!(f, "Starting"),
            HealthCheckResult::Loading { message } => write!(f, "Loading ({})", message),
            HealthCheckResult::Failure { error } => write!(f, "Failed ({})", error),
        }
    }
}
