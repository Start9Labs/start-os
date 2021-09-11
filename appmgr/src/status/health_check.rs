use std::path::Path;

use chrono::{DateTime, Utc};
use indexmap::IndexMap;
use serde::{Deserialize, Deserializer, Serialize};

use crate::action::{ActionImplementation, NoOutput};
use crate::context::RpcContext;
use crate::id::Id;
use crate::s9pk::manifest::PackageId;
use crate::util::Version;
use crate::volume::Volumes;
use crate::Error;

#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash, Serialize)]
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
pub struct HealthChecks(pub IndexMap<HealthCheckId, HealthCheck>);
impl HealthChecks {
    pub async fn check_all(
        &self,
        ctx: &RpcContext,
        started: DateTime<Utc>,
        pkg_id: &PackageId,
        pkg_version: &Version,
        volumes: &Volumes,
    ) -> Result<IndexMap<HealthCheckId, HealthCheckResult>, Error> {
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
    #[serde(flatten)]
    implementation: ActionImplementation,
    pub critical: bool,
}
impl HealthCheck {
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
            )
            .await?;
        Ok(HealthCheckResult {
            time: Utc::now(),
            result: match res {
                Ok(NoOutput) => HealthCheckResultVariant::Success,
                Err((59, _)) => HealthCheckResultVariant::Disabled,
                Err((60, _)) => HealthCheckResultVariant::Starting,
                Err((61, message)) => HealthCheckResultVariant::Loading { message },
                Err((_, error)) => HealthCheckResultVariant::Failure { error },
            },
        })
    }
}

#[derive(Clone, Debug, Deserialize, Serialize)]
pub struct HealthCheckResult {
    pub time: DateTime<Utc>,
    #[serde(flatten)]
    pub result: HealthCheckResultVariant,
}
impl HealthCheckResult {
    pub fn not_available() -> Self {
        HealthCheckResult {
            time: Utc::now(),
            result: HealthCheckResultVariant::Failure {
                error: "Health Check Status Not Available".to_owned(),
            },
        }
    }
}

#[derive(Clone, Debug, Deserialize, Serialize)]
#[serde(rename_all = "kebab-case")]
#[serde(tag = "result")]
pub enum HealthCheckResultVariant {
    Success,
    Disabled,
    Starting,
    Loading { message: String },
    Failure { error: String },
}
impl std::fmt::Display for HealthCheckResultVariant {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            HealthCheckResultVariant::Success => write!(f, "Succeeded"),
            HealthCheckResultVariant::Disabled => write!(f, "Disabled"),
            HealthCheckResultVariant::Starting => write!(f, "Starting"),
            HealthCheckResultVariant::Loading { message } => write!(f, "Loading ({})", message),
            HealthCheckResultVariant::Failure { error } => write!(f, "Failed ({})", error),
        }
    }
}
