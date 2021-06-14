use std::collections::HashMap;

use anyhow::anyhow;
use emver::VersionRange;
use indexmap::{IndexMap, IndexSet};
use patch_db::{DbHandle, DiffPatch, HasModel, Map, MapModel};
use serde::{Deserialize, Serialize};

use crate::action::ActionImplementation;
use crate::config::{Config, ConfigSpec};
use crate::id::InterfaceId;
use crate::net::host::Hosts;
use crate::s9pk::manifest::PackageId;
use crate::status::health_check::{HealthCheckId, HealthCheckResult, HealthCheckResultVariant};
use crate::status::{DependencyErrors, MainStatus, Status};
use crate::util::Version;
use crate::{Error, ResultExt as _};

#[derive(Clone, Debug, thiserror::Error, Serialize, Deserialize)]
#[serde(rename_all = "kebab-case")]
#[serde(tag = "type")]
pub enum DependencyError {
    NotInstalled, // { "type": "not-installed" }
    IncorrectVersion {
        expected: VersionRange,
        received: Version,
    }, // { "type": "incorrect-version", "expected": "0.1.0", "received": "^0.2.0" }
    ConfigUnsatisfied {
        error: String,
    }, // { "type": "config-unsatisfied", "error": "Bitcoin Core must have pruning set to manual." }
    NotRunning,   // { "type": "not-running" }
    HealthChecksFailed {
        failures: IndexMap<HealthCheckId, HealthCheckResult>,
    }, // { "type": "health-checks-failed", "checks": { "rpc": { "time": "2021-05-11T18:21:29Z", "result": "warming-up" } } }
}
impl DependencyError {
    pub fn merge_with(self, other: DependencyError) -> DependencyError {
        use DependencyError::*;
        match (self, other) {
            (NotInstalled, _) => NotInstalled,
            (_, NotInstalled) => NotInstalled,
            (IncorrectVersion { expected, received }, _) => IncorrectVersion { expected, received },
            (_, IncorrectVersion { expected, received }) => IncorrectVersion { expected, received },
            (ConfigUnsatisfied { error: e0 }, ConfigUnsatisfied { error: e1 }) => {
                ConfigUnsatisfied {
                    error: e0 + "\n" + &e1,
                }
            }
            (ConfigUnsatisfied { error }, _) => ConfigUnsatisfied { error },
            (_, ConfigUnsatisfied { error }) => ConfigUnsatisfied { error },
            (NotRunning, _) => NotRunning,
            (_, NotRunning) => NotRunning,
            (HealthChecksFailed { failures: f0 }, HealthChecksFailed { failures: f1 }) => {
                HealthChecksFailed {
                    failures: f0.into_iter().chain(f1.into_iter()).collect(),
                }
            }
        }
    }
}
impl std::fmt::Display for DependencyError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        use DependencyError::*;
        match self {
            NotInstalled => write!(f, "Not Installed"),
            IncorrectVersion { expected, received } => write!(
                f,
                "Incorrect Version: Expected {}, Received {}",
                expected,
                received.as_str()
            ),
            ConfigUnsatisfied { error } => {
                write!(f, "Configuration Requirements Not Satisfied: {}", error)
            }
            NotRunning => write!(f, "Not Running"),
            HealthChecksFailed { failures } => {
                write!(f, "Failed Health Check(s): ")?;
                let mut comma = false;
                for (check, res) in failures {
                    if !comma {
                        comma = true;
                    } else {
                        write!(f, ", ");
                    }
                    write!(f, "{} @ {} {}", check, res.time, res.result)?;
                }
                Ok(())
            }
        }
    }
}

#[derive(Clone, Debug, Serialize, Deserialize)]
#[serde(rename_all = "kebab-case")]
pub struct TaggedDependencyError {
    pub dependency: PackageId,
    pub error: DependencyError,
}

#[derive(Clone, Debug, Serialize, Deserialize)]
#[serde(rename_all = "kebab-case")]
pub struct BreakageRes {
    pub patch: DiffPatch,
    pub breakages: IndexMap<PackageId, TaggedDependencyError>,
}

#[derive(Clone, Debug, Default, Deserialize, Serialize)]
pub struct Dependencies(pub IndexMap<PackageId, DepInfo>);
impl Map for Dependencies {
    type Key = PackageId;
    type Value = DepInfo;
    fn get(&self, key: &Self::Key) -> Option<&Self::Value> {
        self.0.get(key)
    }
}
impl HasModel for Dependencies {
    type Model = MapModel<Self>;
}

#[derive(Clone, Debug, Deserialize, Serialize, HasModel)]
#[serde(rename_all = "kebab-case")]
pub struct DepInfo {
    pub version: VersionRange,
    pub optional: Option<String>,
    pub description: Option<String>,
    pub critical: bool,
    #[serde(default)]
    #[model]
    pub config: Option<DependencyConfig>,
}
impl DepInfo {
    pub async fn satisfied<Db: DbHandle>(
        &self,
        db: &mut Db,
        dependency_id: &PackageId,
        dependency_config: Option<Config>, // fetch if none
        dependent_id: &PackageId,
        dependent_version: &Version,
    ) -> Result<Result<(), DependencyError>, Error> {
        let dependency = crate::db::DatabaseModel::new()
            .package_data()
            .idx_model(dependency_id)
            .and_then(|pde| pde.installed())
            .get(db)
            .await?;
        let info = if let Some(info) = &*dependency {
            info
        } else {
            return Ok(Err(DependencyError::NotInstalled));
        };
        if !&info.manifest.version.satisfies(&self.version) {
            return Ok(Err(DependencyError::IncorrectVersion {
                expected: self.version.clone(),
                received: info.manifest.version.clone(),
            }));
        }
        let hosts = crate::db::DatabaseModel::new()
            .network()
            .hosts()
            .get(db)
            .await?;
        let dependency_config = if let Some(cfg) = dependency_config {
            cfg
        } else if let Some(cfg_info) = &info.manifest.config {
            cfg_info
                .get(
                    dependency_id,
                    &info.manifest.version,
                    &info.manifest.volumes,
                    &hosts,
                )
                .await?
                .config
                .unwrap_or_default()
        } else {
            Config::default()
        };
        if let Some(cfg_req) = &self.config {
            if let Err(e) = cfg_req
                .check(dependent_id, dependent_version, &dependency_config)
                .await
            {
                if e.kind == crate::ErrorKind::ConfigRulesViolation {
                    return Ok(Err(DependencyError::ConfigUnsatisfied {
                        error: format!("{}", e),
                    }));
                } else {
                    return Err(e);
                }
            }
        }
        match &info.status.main {
            MainStatus::BackingUp {
                started: Some(_),
                health,
            }
            | MainStatus::Running { health, .. } => {
                let mut failures = IndexMap::with_capacity(health.len());
                for (check, res) in health {
                    if !matches!(res.result, HealthCheckResultVariant::Success) {
                        failures.insert(check.clone(), res.clone());
                    }
                }
                if !failures.is_empty() {
                    return Ok(Err(DependencyError::HealthChecksFailed { failures }));
                }
            }
            _ => return Ok(Err(DependencyError::NotRunning)),
        }
        Ok(Ok(()))
    }
}

#[derive(Clone, Debug, Deserialize, Serialize, HasModel)]
#[serde(rename_all = "kebab-case")]
pub struct DependencyConfig {
    check: ActionImplementation,
    auto_configure: ActionImplementation,
}
impl DependencyConfig {
    pub async fn check(
        &self,
        dependent_id: &PackageId,
        dependent_version: &Version,
        dependency_config: &Config,
    ) -> Result<Result<(), String>, Error> {
        Ok(self
            .check
            .sandboxed(dependent_id, dependent_version, Some(dependency_config))
            .await?
            .map_err(|(_, e)| e))
    }
    pub async fn auto_configure(
        &self,
        dependent_id: &PackageId,
        dependent_version: &Version,
        old: &Config,
    ) -> Result<Config, Error> {
        self.auto_configure
            .sandboxed(dependent_id, dependent_version, Some(old))
            .await?
            .map_err(|e| Error::new(anyhow!("{}", e.1), crate::ErrorKind::AutoConfigure))
    }
}
