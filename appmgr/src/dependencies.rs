use std::collections::BTreeMap;

use anyhow::anyhow;
use emver::VersionRange;
use futures::future::BoxFuture;
use futures::FutureExt;
use patch_db::{DbHandle, DiffPatch, HasModel, Map, MapModel};
use serde::{Deserialize, Serialize};

use crate::action::{ActionImplementation, NoOutput};
use crate::config::Config;
use crate::context::RpcContext;
use crate::db::model::CurrentDependencyInfo;
use crate::s9pk::manifest::{Manifest, PackageId};
use crate::status::health_check::{HealthCheckId, HealthCheckResult};
use crate::status::{MainStatus, Status};
use crate::util::Version;
use crate::volume::Volumes;
use crate::Error;

#[derive(Clone, Debug, thiserror::Error, Serialize, Deserialize)]
#[serde(rename_all = "kebab-case")]
#[serde(tag = "type")]
pub enum DependencyError {
    NotInstalled, // { "type": "not-installed" }
    #[serde(rename_all = "kebab-case")]
    IncorrectVersion {
        expected: VersionRange,
        received: Version,
    }, // { "type": "incorrect-version", "expected": "0.1.0", "received": "^0.2.0" }
    #[serde(rename_all = "kebab-case")]
    ConfigUnsatisfied {
        error: String,
    }, // { "type": "config-unsatisfied", "error": "Bitcoin Core must have pruning set to manual." }
    NotRunning,   // { "type": "not-running" }
    #[serde(rename_all = "kebab-case")]
    HealthChecksFailed {
        failures: BTreeMap<HealthCheckId, HealthCheckResult>,
    }, // { "type": "health-checks-failed", "checks": { "rpc": { "time": "2021-05-11T18:21:29Z", "result": "starting" } } }
    #[serde(rename_all = "kebab-case")]
    Transitive, // { "type": "transitive" }
}
impl DependencyError {
    pub fn merge_with(self, other: DependencyError) -> DependencyError {
        match (self, other) {
            (DependencyError::NotInstalled, _) | (_, DependencyError::NotInstalled) => {
                DependencyError::NotInstalled
            }
            (DependencyError::IncorrectVersion { expected, received }, _)
            | (_, DependencyError::IncorrectVersion { expected, received }) => {
                DependencyError::IncorrectVersion { expected, received }
            }
            (
                DependencyError::ConfigUnsatisfied { error: e0 },
                DependencyError::ConfigUnsatisfied { error: e1 },
            ) => DependencyError::ConfigUnsatisfied {
                error: e0 + "\n" + &e1,
            },
            (DependencyError::ConfigUnsatisfied { error }, _)
            | (_, DependencyError::ConfigUnsatisfied { error }) => {
                DependencyError::ConfigUnsatisfied { error }
            }
            (DependencyError::NotRunning, _) | (_, DependencyError::NotRunning) => {
                DependencyError::NotRunning
            }
            (
                DependencyError::HealthChecksFailed { failures: f0 },
                DependencyError::HealthChecksFailed { failures: f1 },
            ) => DependencyError::HealthChecksFailed {
                failures: f0.into_iter().chain(f1.into_iter()).collect(),
            },
            (DependencyError::HealthChecksFailed { failures }, _)
            | (_, DependencyError::HealthChecksFailed { failures }) => {
                DependencyError::HealthChecksFailed { failures }
            }
            (DependencyError::Transitive, _) | (_, DependencyError::Transitive) => {
                DependencyError::Transitive
            }
        }
    }
    pub fn try_heal<'a, Db: DbHandle>(
        self,
        ctx: &'a RpcContext,
        db: &'a mut Db,
        id: &'a PackageId,
        dependency: &'a PackageId,
        mut dependency_config: Option<Config>,
        info: &'a DepInfo,
    ) -> BoxFuture<'a, Result<Option<Self>, Error>> {
        async move {
            Ok(match self {
                DependencyError::NotInstalled => {
                    if crate::db::DatabaseModel::new()
                        .package_data()
                        .idx_model(dependency)
                        .and_then(|m| m.installed())
                        .exists(db, true)
                        .await?
                    {
                        DependencyError::IncorrectVersion {
                            expected: info.version.clone(),
                            received: Default::default(),
                        }
                        .try_heal(ctx, db, id, dependency, dependency_config, info)
                        .await?
                    } else {
                        Some(DependencyError::NotInstalled)
                    }
                }
                DependencyError::IncorrectVersion { expected, .. } => {
                    let version: Version = crate::db::DatabaseModel::new()
                        .package_data()
                        .idx_model(dependency)
                        .and_then(|m| m.installed())
                        .map(|m| m.manifest().version())
                        .get(db, true)
                        .await?
                        .into_owned()
                        .unwrap_or_default();
                    if version.satisfies(&expected) {
                        DependencyError::ConfigUnsatisfied {
                            error: String::new(),
                        }
                        .try_heal(ctx, db, id, dependency, dependency_config, info)
                        .await?
                    } else {
                        Some(DependencyError::IncorrectVersion {
                            expected,
                            received: version,
                        })
                    }
                }
                DependencyError::ConfigUnsatisfied { .. } => {
                    let dependent_manifest = crate::db::DatabaseModel::new()
                        .package_data()
                        .idx_model(id)
                        .and_then(|m| m.installed())
                        .map::<_, Manifest>(|m| m.manifest())
                        .expect(db)
                        .await?
                        .get(db, true)
                        .await?;
                    let dependency_manifest = crate::db::DatabaseModel::new()
                        .package_data()
                        .idx_model(dependency)
                        .and_then(|m| m.installed())
                        .map::<_, Manifest>(|m| m.manifest())
                        .expect(db)
                        .await?
                        .get(db, true)
                        .await?;
                    let dependency_config = if let Some(cfg) = dependency_config.take() {
                        cfg
                    } else if let Some(cfg_info) = &dependency_manifest.config {
                        cfg_info
                            .get(
                                ctx,
                                dependency,
                                &dependency_manifest.version,
                                &dependency_manifest.volumes,
                            )
                            .await?
                            .config
                            .unwrap_or_default()
                    } else {
                        Config::default()
                    };
                    if let Some(cfg_req) = &info.config {
                        if let Err(e) = cfg_req
                            .check(
                                ctx,
                                id,
                                &dependent_manifest.version,
                                &dependent_manifest.volumes,
                                &dependency_config,
                            )
                            .await
                        {
                            if e.kind == crate::ErrorKind::ConfigRulesViolation {
                                return Ok(Some(DependencyError::ConfigUnsatisfied {
                                    error: format!("{}", e),
                                }));
                            } else {
                                return Err(e);
                            }
                        }
                    }
                    DependencyError::NotRunning
                        .try_heal(ctx, db, id, dependency, Some(dependency_config), info)
                        .await?
                }
                DependencyError::NotRunning => {
                    let status = crate::db::DatabaseModel::new()
                        .package_data()
                        .idx_model(dependency)
                        .and_then(|m| m.installed())
                        .map::<_, Status>(|m| m.status())
                        .expect(db)
                        .await?
                        .get(db, true)
                        .await?;
                    if status.main.running() {
                        DependencyError::HealthChecksFailed {
                            failures: BTreeMap::new(),
                        }
                        .try_heal(ctx, db, id, dependency, dependency_config, info)
                        .await?
                    } else {
                        Some(DependencyError::NotRunning)
                    }
                }
                DependencyError::HealthChecksFailed { .. } => {
                    let status = crate::db::DatabaseModel::new()
                        .package_data()
                        .idx_model(dependency)
                        .and_then(|m| m.installed())
                        .map::<_, Status>(|m| m.status())
                        .expect(db)
                        .await?
                        .get(db, true)
                        .await?
                        .into_owned();
                    match status.main {
                        MainStatus::BackingUp {
                            started: Some(_),
                            health,
                        }
                        | MainStatus::Running { health, .. } => {
                            let mut failures = BTreeMap::new();
                            for (check, res) in health {
                                if !matches!(res, HealthCheckResult::Success)
                                    && crate::db::DatabaseModel::new()
                                        .package_data()
                                        .idx_model(id)
                                        .and_then(|m| m.installed())
                                        .and_then::<_, CurrentDependencyInfo>(|m| {
                                            m.current_dependencies().idx_model(dependency)
                                        })
                                        .get(db, true)
                                        .await?
                                        .into_owned()
                                        .map(|i| i.health_checks)
                                        .unwrap_or_default()
                                        .contains(&check)
                                {
                                    failures.insert(check.clone(), res.clone());
                                }
                            }
                            if !failures.is_empty() {
                                Some(DependencyError::HealthChecksFailed { failures })
                            } else {
                                DependencyError::Transitive
                                    .try_heal(ctx, db, id, dependency, dependency_config, info)
                                    .await?
                            }
                        }
                        _ => return Ok(Some(DependencyError::NotRunning)),
                    }
                }
                DependencyError::Transitive => {
                    if crate::db::DatabaseModel::new()
                        .package_data()
                        .idx_model(dependency)
                        .and_then(|m| m.installed())
                        .map::<_, DependencyErrors>(|m| m.status().dependency_errors())
                        .get(db, true)
                        .await?
                        .into_owned()
                        .unwrap_or_default()
                        .0
                        .is_empty()
                    {
                        None
                    } else {
                        Some(DependencyError::Transitive)
                    }
                }
            })
        }
        .boxed()
    }
}
impl std::fmt::Display for DependencyError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            DependencyError::NotInstalled => write!(f, "Not Installed"),
            DependencyError::IncorrectVersion { expected, received } => write!(
                f,
                "Incorrect Version: Expected {}, Received {}",
                expected,
                received.as_str()
            ),
            DependencyError::ConfigUnsatisfied { error } => {
                write!(f, "Configuration Requirements Not Satisfied: {}", error)
            }
            DependencyError::NotRunning => write!(f, "Not Running"),
            DependencyError::HealthChecksFailed { failures } => {
                write!(f, "Failed Health Check(s): ")?;
                let mut comma = false;
                for (check, res) in failures {
                    if !comma {
                        comma = true;
                    } else {
                        write!(f, ", ")?;
                    }
                    write!(f, "{}: {}", check, res)?;
                }
                Ok(())
            }
            DependencyError::Transitive => {
                write!(f, "Dependency Error(s)")
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
    pub breakages: BTreeMap<PackageId, TaggedDependencyError>,
}

#[derive(Clone, Debug, Default, Deserialize, Serialize)]
pub struct Dependencies(pub BTreeMap<PackageId, DepInfo>);
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

#[derive(Clone, Debug, Deserialize, Serialize)]
#[serde(rename_all = "kebab-case")]
#[serde(tag = "type")]
pub enum DependencyRequirement {
    OptIn { how: String },
    OptOut { how: String },
    Required,
}
impl DependencyRequirement {
    pub fn required(&self) -> bool {
        matches!(self, &DependencyRequirement::Required)
    }
}

#[derive(Clone, Debug, Deserialize, Serialize, HasModel)]
#[serde(rename_all = "kebab-case")]
pub struct DepInfo {
    pub version: VersionRange,
    pub requirement: DependencyRequirement,
    pub description: Option<String>,
    pub critical: bool,
    #[serde(default)]
    #[model]
    pub config: Option<DependencyConfig>,
}
impl DepInfo {
    pub async fn satisfied<Db: DbHandle>(
        &self,
        ctx: &RpcContext,
        db: &mut Db,
        dependency_id: &PackageId,
        dependency_config: Option<Config>, // fetch if none
        dependent_id: &PackageId,
    ) -> Result<Result<(), DependencyError>, Error> {
        Ok(
            if let Some(err) = DependencyError::NotInstalled
                .try_heal(
                    ctx,
                    db,
                    dependent_id,
                    dependency_id,
                    dependency_config,
                    self,
                )
                .await?
            {
                Err(err)
            } else {
                Ok(())
            },
        )
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
        ctx: &RpcContext,
        dependent_id: &PackageId,
        dependent_version: &Version,
        dependent_volumes: &Volumes,
        dependency_config: &Config,
    ) -> Result<Result<NoOutput, String>, Error> {
        Ok(self
            .check
            .sandboxed(
                ctx,
                dependent_id,
                dependent_version,
                dependent_volumes,
                Some(dependency_config),
            )
            .await?
            .map_err(|(_, e)| e))
    }
    pub async fn auto_configure(
        &self,
        ctx: &RpcContext,
        dependent_id: &PackageId,
        dependent_version: &Version,
        dependent_volumes: &Volumes,
        old: &Config,
    ) -> Result<Config, Error> {
        self.auto_configure
            .sandboxed(
                ctx,
                dependent_id,
                dependent_version,
                dependent_volumes,
                Some(old),
            )
            .await?
            .map_err(|e| Error::new(anyhow!("{}", e.1), crate::ErrorKind::AutoConfigure))
    }
}

pub async fn update_current_dependents<
    'a,
    Db: DbHandle,
    I: IntoIterator<Item = (&'a PackageId, &'a CurrentDependencyInfo)>,
>(
    db: &mut Db,
    dependent_id: &PackageId,
    current_dependencies: I,
) -> Result<(), Error> {
    for (dependency, dep_info) in current_dependencies
        .into_iter()
        .filter(|(dependency, _)| dependency != &dependent_id)
    {
        if let Some(dependency_model) = crate::db::DatabaseModel::new()
            .package_data()
            .idx_model(&dependency)
            .and_then(|pkg| pkg.installed())
            .check(db)
            .await?
        {
            dependency_model
                .current_dependents()
                .idx_model(dependent_id)
                .put(db, &dep_info)
                .await?;
        }
    }
    Ok(())
}

#[derive(Debug, Clone, Default, Deserialize, Serialize)]
pub struct DependencyErrors(pub BTreeMap<PackageId, DependencyError>);
impl Map for DependencyErrors {
    type Key = PackageId;
    type Value = DependencyError;
    fn get(&self, key: &Self::Key) -> Option<&Self::Value> {
        self.0.get(key)
    }
}
impl HasModel for DependencyErrors {
    type Model = MapModel<Self>;
}
impl DependencyErrors {
    pub async fn init<Db: DbHandle>(
        ctx: &RpcContext,
        db: &mut Db,
        manifest: &Manifest,
        current_dependencies: &BTreeMap<PackageId, CurrentDependencyInfo>,
    ) -> Result<DependencyErrors, Error> {
        let mut res = BTreeMap::new();
        for (dependency_id, info) in current_dependencies.keys().filter_map(|dependency_id| {
            manifest
                .dependencies
                .0
                .get(dependency_id)
                .map(|info| (dependency_id, info))
        }) {
            if let Err(e) = info
                .satisfied(ctx, db, dependency_id, None, &manifest.id)
                .await?
            {
                res.insert(dependency_id.clone(), e);
            }
        }
        Ok(DependencyErrors(res))
    }
}
impl std::fmt::Display for DependencyErrors {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{{ ")?;
        for (idx, (id, err)) in self.0.iter().enumerate() {
            write!(f, "{}: {}", id, err)?;
            if idx < self.0.len() - 1 {
                // not last
                write!(f, ", ")?;
            }
        }
        write!(f, " }}")
    }
}

pub async fn break_all_dependents_transitive<'a, Db: DbHandle>(
    db: &'a mut Db,
    id: &'a PackageId,
    error: DependencyError,
    breakages: &'a mut BTreeMap<PackageId, TaggedDependencyError>,
) -> Result<(), Error> {
    for dependent in crate::db::DatabaseModel::new()
        .package_data()
        .idx_model(id)
        .and_then(|m| m.installed())
        .expect(db)
        .await?
        .current_dependents()
        .keys(db, true)
        .await?
    {
        break_transitive(db, &dependent, id, error.clone(), breakages).await?;
    }
    Ok(())
}

pub fn break_transitive<'a, Db: DbHandle>(
    db: &'a mut Db,
    id: &'a PackageId,
    dependency: &'a PackageId,
    error: DependencyError,
    breakages: &'a mut BTreeMap<PackageId, TaggedDependencyError>,
) -> BoxFuture<'a, Result<(), Error>> {
    async move {
        let model = crate::db::DatabaseModel::new()
            .package_data()
            .idx_model(id)
            .and_then(|m| m.installed())
            .expect(db)
            .await?;
        let mut status = model.clone().status().get_mut(db).await?;

        let old = status.dependency_errors.0.remove(dependency);
        let newly_broken = old.is_none();
        status.dependency_errors.0.insert(
            dependency.clone(),
            if let Some(old) = old {
                old.merge_with(error.clone())
            } else {
                error.clone()
            },
        );
        if newly_broken {
            breakages.insert(
                id.clone(),
                TaggedDependencyError {
                    dependency: dependency.clone(),
                    error: error.clone(),
                },
            );
            if status.main.running() {
                let transitive_error = if model
                    .clone()
                    .manifest()
                    .dependencies()
                    .idx_model(dependency)
                    .get(db, true)
                    .await?
                    .into_owned()
                    .ok_or_else(|| {
                        Error::new(
                            anyhow!("{} not in listed dependencies", dependency),
                            crate::ErrorKind::Database,
                        )
                    })?
                    .critical
                {
                    status.main.stop();
                    DependencyError::NotRunning
                } else {
                    DependencyError::Transitive
                };
                break_all_dependents_transitive(db, id, transitive_error, breakages).await?;
            }
        }

        status.save(db).await?;

        Ok(())
    }
    .boxed()
}

pub async fn heal_all_dependents_transitive<'a, Db: DbHandle>(
    ctx: &'a RpcContext,
    db: &'a mut Db,
    id: &'a PackageId,
) -> Result<(), Error> {
    for dependent in crate::db::DatabaseModel::new()
        .package_data()
        .idx_model(id)
        .and_then(|m| m.installed())
        .expect(db)
        .await?
        .current_dependents()
        .keys(db, true)
        .await?
    {
        heal_transitive(ctx, db, &dependent, id).await?;
    }
    Ok(())
}

pub fn heal_transitive<'a, Db: DbHandle>(
    ctx: &'a RpcContext,
    db: &'a mut Db,
    id: &'a PackageId,
    dependency: &'a PackageId,
) -> BoxFuture<'a, Result<(), Error>> {
    async move {
        let model = crate::db::DatabaseModel::new()
            .package_data()
            .idx_model(id)
            .and_then(|m| m.installed())
            .expect(db)
            .await?;
        let mut status = model.clone().status().get_mut(db).await?;

        let old = status.dependency_errors.0.remove(dependency);

        if let Some(old) = old {
            let info = model
                .manifest()
                .dependencies()
                .idx_model(dependency)
                .expect(db)
                .await?
                .get(db, true)
                .await?;
            if let Some(new) = old.try_heal(ctx, db, id, dependency, None, &*info).await? {
                status.dependency_errors.0.insert(dependency.clone(), new);
            } else {
                heal_all_dependents_transitive(ctx, db, id).await?;
            }
        }

        status.save(db).await?;

        Ok(())
    }
    .boxed()
}
