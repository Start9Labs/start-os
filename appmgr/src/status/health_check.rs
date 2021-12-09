use std::collections::BTreeMap;
use std::path::Path;

use chrono::{DateTime, Utc};
use serde::{Deserialize, Deserializer, Serialize};
use tracing::instrument;

use crate::action::{ActionImplementation, NoOutput};
use crate::context::RpcContext;
use crate::id::Id;
use crate::s9pk::manifest::PackageId;
use crate::util::serde::Duration;
use crate::util::Version;
use crate::volume::Volumes;
use crate::Error;

#[derive(Clone, Copy, Debug, PartialEq, Eq, PartialOrd, Ord, Hash, Serialize)]
pub struct HealthCheckId<S: AsRef<str> = String>(Id<S>);
impl<S: AsRef<str>> std::fmt::Display for HealthCheckId<S> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", &self.0)
    }
}
impl<S: AsRef<str>> AsRef<str> for HealthCheckId<S> {
    fn as_ref(&self) -> &str {
        self.0.as_ref()
    }
}
impl<'de, S> Deserialize<'de> for HealthCheckId<S>
where
    S: AsRef<str>,
    Id<S>: Deserialize<'de>,
{
    fn deserialize<D>(deserializer: D) -> Result<Self, D::Error>
    where
        D: Deserializer<'de>,
    {
        Ok(HealthCheckId(Deserialize::deserialize(deserializer)?))
    }
}
impl<S: AsRef<str>> AsRef<Path> for HealthCheckId<S> {
    fn as_ref(&self) -> &Path {
        self.0.as_ref().as_ref()
    }
}

#[derive(Clone, Debug, Deserialize, Serialize)]
pub struct HealthChecks(pub BTreeMap<HealthCheckId, HealthCheck>);
impl HealthChecks {
    pub async fn check_all(
        &self,
        ctx: &RpcContext,
        started: DateTime<Utc>,
        pkg_id: &PackageId,
        pkg_version: &Version,
        volumes: &Volumes,
    ) -> Result<BTreeMap<HealthCheckId, HealthCheckResult>, Error> {
        let res = futures::future::try_join_all(self.0.iter().map(|(id, check)| async move {
            Ok::<_, Error>((
                id.clone(),
                check
                    .check(ctx, id, started, pkg_id, pkg_version, volumes)
                    .await?,
            ))
        }))
        .await?;
        Ok(res.into_iter().collect())
    }
}

#[derive(Clone, Debug, Deserialize, Serialize)]
pub struct HealthCheck {
    pub name: String,
    pub description: String,
    #[serde(flatten)]
    implementation: ActionImplementation,
    pub timeout: Option<Duration>,
}
impl HealthCheck {
    #[instrument(skip(ctx))]
    pub async fn check(
        &self,
        ctx: &RpcContext,
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
                pkg_id,
                pkg_version,
                Some(&format!("{}Health", id)),
                volumes,
                Some(Utc::now().signed_duration_since(started).num_milliseconds()),
                true,
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
