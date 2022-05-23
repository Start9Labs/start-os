use std::cmp::Ordering;
use std::collections::BTreeMap;
use std::time::Duration;

use color_eyre::eyre::eyre;
use emver::VersionRange;
use futures::future::BoxFuture;
use futures::FutureExt;
use patch_db::{
    DbHandle, HasModel, LockReceipt, LockTargetId, LockType, Map, MapModel, PatchDbHandle, Verifier,
};
use rand::SeedableRng;
use rpc_toolkit::command;
use serde::{Deserialize, Serialize};
use tracing::instrument;

use crate::config::spec::PackagePointerSpec;
use crate::config::{not_found, Config, ConfigReceipts, ConfigSpec};
use crate::context::RpcContext;
use crate::db::model::{CurrentDependencyInfo, InstalledPackageDataEntry};
use crate::procedure::{NoOutput, PackageProcedure};
use crate::s9pk::manifest::{Manifest, PackageId};
use crate::status::health_check::{HealthCheckId, HealthCheckResult};
use crate::status::{MainStatus, Status};
use crate::util::serde::display_serializable;
use crate::util::{display_none, Version};
use crate::volume::Volumes;
use crate::Error;
use crate::{
    config::action::{ConfigActions, ConfigRes},
    procedure::ProcedureName,
};

#[command(subcommands(configure))]
pub fn dependency() -> Result<(), Error> {
    Ok(())
}

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

#[derive(Clone)]
pub struct TryHealReceipts {
    status: LockReceipt<Status, String>,
    manifest: LockReceipt<Manifest, String>,
    manifest_version: LockReceipt<Version, String>,
    current_dependencies: LockReceipt<BTreeMap<PackageId, CurrentDependencyInfo>, String>,
    dependency_errors: LockReceipt<DependencyErrors, String>,
}

impl TryHealReceipts {
    pub async fn new<'a>(db: &'a mut impl DbHandle) -> Result<Self, Error> {
        let mut locks = Vec::new();

        let setup = Self::setup(&mut locks);
        Ok(setup(&db.lock_all(locks).await?)?)
    }

    pub fn setup(locks: &mut Vec<LockTargetId>) -> impl FnOnce(&Verifier) -> Result<Self, Error> {
        let manifest_version = crate::db::DatabaseModel::new()
            .package_data()
            .star()
            .installed()
            .map(|x| x.manifest().version())
            .make_locker(LockType::Write)
            .add_to_keys(locks);
        let status = crate::db::DatabaseModel::new()
            .package_data()
            .star()
            .installed()
            .map(|x| x.status())
            .make_locker(LockType::Write)
            .add_to_keys(locks);
        let manifest = crate::db::DatabaseModel::new()
            .package_data()
            .star()
            .installed()
            .map(|x| x.manifest())
            .make_locker(LockType::Write)
            .add_to_keys(locks);

        let current_dependencies = crate::db::DatabaseModel::new()
            .package_data()
            .star()
            .installed()
            .map(|x| x.current_dependencies())
            .make_locker(LockType::Write)
            .add_to_keys(locks);
        let dependency_errors = crate::db::DatabaseModel::new()
            .package_data()
            .star()
            .installed()
            .map(|x| x.status().dependency_errors())
            .make_locker(LockType::Write)
            .add_to_keys(locks);
        move |skeleton_key| {
            Ok(Self {
                status: status.verify(skeleton_key)?,
                manifest_version: manifest_version.verify(skeleton_key)?,
                current_dependencies: current_dependencies.verify(skeleton_key)?,
                manifest: manifest.verify(skeleton_key)?,
                dependency_errors: dependency_errors.verify(skeleton_key)?,
            })
        }
    }
}

impl DependencyError {
    pub fn cmp_priority(&self, other: &DependencyError) -> std::cmp::Ordering {
        use std::cmp::Ordering::*;

        use DependencyError::*;
        match (self, other) {
            (NotInstalled, NotInstalled) => Equal,
            (NotInstalled, _) => Greater,
            (_, NotInstalled) => Less,
            (IncorrectVersion { .. }, IncorrectVersion { .. }) => Equal,
            (IncorrectVersion { .. }, _) => Greater,
            (_, IncorrectVersion { .. }) => Less,
            (ConfigUnsatisfied { .. }, ConfigUnsatisfied { .. }) => Equal,
            (ConfigUnsatisfied { .. }, _) => Greater,
            (_, ConfigUnsatisfied { .. }) => Less,
            (NotRunning, NotRunning) => Equal,
            (NotRunning, _) => Greater,
            (_, NotRunning) => Less,
            (HealthChecksFailed { .. }, HealthChecksFailed { .. }) => Equal,
            (HealthChecksFailed { .. }, _) => Greater,
            (_, HealthChecksFailed { .. }) => Less,
            (Transitive, Transitive) => Equal,
        }
    }
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
            (DependencyError::Transitive, _) => DependencyError::Transitive,
        }
    }
    #[instrument(skip(ctx, db, receipts))]
    pub fn try_heal<'a, Db: DbHandle>(
        self,
        ctx: &'a RpcContext,
        db: &'a mut Db,
        id: &'a PackageId,
        dependency: &'a PackageId,
        mut dependency_config: Option<Config>,
        info: &'a DepInfo,
        receipts: &'a TryHealReceipts,
    ) -> BoxFuture<'a, Result<Option<Self>, Error>> {
        async move {
            Ok(match self {
                DependencyError::NotInstalled => {
                    if receipts.status.get(db, dependency).await?.is_some() {
                        DependencyError::IncorrectVersion {
                            expected: info.version.clone(),
                            received: Default::default(),
                        }
                        .try_heal(ctx, db, id, dependency, dependency_config, info, receipts)
                        .await?
                    } else {
                        Some(DependencyError::NotInstalled)
                    }
                }
                DependencyError::IncorrectVersion { expected, .. } => {
                    let version: Version = receipts
                        .manifest_version
                        .get(db, dependency)
                        .await?
                        .unwrap_or_default();
                    if version.satisfies(&expected) {
                        DependencyError::ConfigUnsatisfied {
                            error: String::new(),
                        }
                        .try_heal(ctx, db, id, dependency, dependency_config, info, receipts)
                        .await?
                    } else {
                        Some(DependencyError::IncorrectVersion {
                            expected,
                            received: version,
                        })
                    }
                }
                DependencyError::ConfigUnsatisfied { .. } => {
                    let dependent_manifest =
                        receipts.manifest.get(db, id).await?.ok_or_else(not_found)?;
                    let dependency_manifest = receipts
                        .manifest
                        .get(db, dependency)
                        .await?
                        .ok_or_else(not_found)?;

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
                        if let Err(error) = cfg_req
                            .check(
                                ctx,
                                id,
                                &dependent_manifest.version,
                                &dependent_manifest.volumes,
                                dependency,
                                &dependency_config,
                            )
                            .await?
                        {
                            return Ok(Some(DependencyError::ConfigUnsatisfied { error }));
                        }
                    }
                    DependencyError::NotRunning
                        .try_heal(
                            ctx,
                            db,
                            id,
                            dependency,
                            Some(dependency_config),
                            info,
                            receipts,
                        )
                        .await?
                }
                DependencyError::NotRunning => {
                    let status = receipts
                        .status
                        .get(db, dependency)
                        .await?
                        .ok_or_else(not_found)?;
                    if status.main.running() {
                        DependencyError::HealthChecksFailed {
                            failures: BTreeMap::new(),
                        }
                        .try_heal(ctx, db, id, dependency, dependency_config, info, receipts)
                        .await?
                    } else {
                        Some(DependencyError::NotRunning)
                    }
                }
                DependencyError::HealthChecksFailed { .. } => {
                    let status = receipts
                        .status
                        .get(db, dependency)
                        .await?
                        .ok_or_else(not_found)?;
                    match status.main {
                        MainStatus::BackingUp {
                            started: Some(_),
                            health,
                        }
                        | MainStatus::Running { health, .. } => {
                            let mut failures = BTreeMap::new();
                            for (check, res) in health {
                                if !matches!(res, HealthCheckResult::Success)
                                    && receipts
                                        .current_dependencies
                                        .get(db, id)
                                        .await?
                                        .ok_or_else(not_found)?
                                        .get(dependency)
                                        .map(|x| x.health_checks.contains(&check))
                                        .unwrap_or(false)
                                {
                                    failures.insert(check.clone(), res.clone());
                                }
                            }
                            if !failures.is_empty() {
                                Some(DependencyError::HealthChecksFailed { failures })
                            } else {
                                DependencyError::Transitive
                                    .try_heal(
                                        ctx,
                                        db,
                                        id,
                                        dependency,
                                        dependency_config,
                                        info,
                                        receipts,
                                    )
                                    .await?
                            }
                        }
                        MainStatus::Starting => {
                            DependencyError::Transitive
                                .try_heal(
                                    ctx,
                                    db,
                                    id,
                                    dependency,
                                    dependency_config,
                                    info,
                                    receipts,
                                )
                                .await?
                        }
                        _ => return Ok(Some(DependencyError::NotRunning)),
                    }
                }
                DependencyError::Transitive => {
                    if receipts
                        .dependency_errors
                        .get(db, dependency)
                        .await?
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
pub struct BreakageRes(pub BTreeMap<PackageId, TaggedDependencyError>);

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
        receipts: &TryHealReceipts,
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
                    receipts,
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
    check: PackageProcedure,
    auto_configure: PackageProcedure,
}
impl DependencyConfig {
    pub async fn check(
        &self,
        ctx: &RpcContext,
        dependent_id: &PackageId,
        dependent_version: &Version,
        dependent_volumes: &Volumes,
        dependency_id: &PackageId,
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
                None,
                ProcedureName::Check(dependency_id.clone()),
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
                None,
                ProcedureName::AutoConfig(dependent_id.clone()),
            )
            .await?
            .map_err(|e| Error::new(eyre!("{}", e.1), crate::ErrorKind::AutoConfigure))
    }
}

pub struct DependencyConfigReceipts {
    dependencies: LockReceipt<Dependencies, ()>,
    dependency_volumes: LockReceipt<Volumes, ()>,
    dependency_version: LockReceipt<Version, ()>,
    dependency_config_action: LockReceipt<ConfigActions, ()>,
    package_volumes: LockReceipt<Volumes, ()>,
    package_version: LockReceipt<Version, ()>,
}

impl DependencyConfigReceipts {
    pub async fn new(
        db: &'_ mut impl DbHandle,
        package_id: &PackageId,
        dependency_id: &PackageId,
    ) -> Result<Self, Error> {
        let mut locks = Vec::new();

        let dependencies = crate::db::DatabaseModel::new()
            .package_data()
            .idx_model(package_id)
            .and_then(|x| x.installed())
            .map(|x| x.manifest().dependencies())
            .make_locker(LockType::Write)
            .add_to_keys(&mut locks);
        let dependency_volumes = crate::db::DatabaseModel::new()
            .package_data()
            .idx_model(dependency_id)
            .and_then(|x| x.installed())
            .map(|x| x.manifest().volumes())
            .make_locker(LockType::Write)
            .add_to_keys(&mut locks);
        let dependency_version = crate::db::DatabaseModel::new()
            .package_data()
            .idx_model(dependency_id)
            .and_then(|x| x.installed())
            .map(|x| x.manifest().version())
            .make_locker(LockType::Write)
            .add_to_keys(&mut locks);
        let dependency_config_action = crate::db::DatabaseModel::new()
            .package_data()
            .idx_model(dependency_id)
            .and_then(|x| x.installed())
            .and_then(|x| x.manifest().config())
            .make_locker(LockType::Write)
            .add_to_keys(&mut locks);
        let package_volumes = crate::db::DatabaseModel::new()
            .package_data()
            .idx_model(package_id)
            .and_then(|x| x.installed())
            .map(|x| x.manifest().volumes())
            .make_locker(LockType::Write)
            .add_to_keys(&mut locks);
        let package_version = crate::db::DatabaseModel::new()
            .package_data()
            .idx_model(package_id)
            .and_then(|x| x.installed())
            .map(|x| x.manifest().version())
            .make_locker(LockType::Write)
            .add_to_keys(&mut locks);

        let skeleton_key = db.lock_all(locks).await?;
        Ok(Self {
            dependencies: dependencies.verify(&skeleton_key)?,
            dependency_volumes: dependency_volumes.verify(&skeleton_key)?,
            dependency_version: dependency_version.verify(&skeleton_key)?,
            dependency_config_action: dependency_config_action.verify(&skeleton_key)?,
            package_volumes: package_volumes.verify(&skeleton_key)?,
            package_version: package_version.verify(&skeleton_key)?,
        })
    }
}

#[command(
    subcommands(self(configure_impl(async)), configure_dry),
    display(display_none)
)]
pub async fn configure(
    #[arg(rename = "dependent-id")] dependent_id: PackageId,
    #[arg(rename = "dependency-id")] dependency_id: PackageId,
) -> Result<(PackageId, PackageId), Error> {
    Ok((dependent_id, dependency_id))
}

pub async fn configure_impl(
    ctx: RpcContext,
    (pkg_id, dep_id): (PackageId, PackageId),
) -> Result<(), Error> {
    let mut db = ctx.db.handle();
    let receipts = DependencyConfigReceipts::new(&mut db, &pkg_id, &dep_id).await?;
    let ConfigDryRes {
        old_config: _,
        new_config,
        spec: _,
    } = configure_logic(ctx.clone(), &mut db, (pkg_id, dep_id.clone()), &receipts).await?;

    let locks = ConfigReceipts::new(&mut db).await?;
    Ok(crate::config::configure(
        &ctx,
        &mut db,
        &dep_id,
        Some(new_config),
        &Some(Duration::from_secs(3).into()),
        false,
        &mut BTreeMap::new(),
        &mut BTreeMap::new(),
        &locks,
    )
    .await?)
}

#[derive(Clone, Debug, Serialize, Deserialize)]
#[serde(rename_all = "kebab-case")]
pub struct ConfigDryRes {
    pub old_config: Config,
    pub new_config: Config,
    pub spec: ConfigSpec,
}

#[command(rename = "dry", display(display_serializable))]
#[instrument(skip(ctx))]
pub async fn configure_dry(
    #[context] ctx: RpcContext,
    #[parent_data] (pkg_id, dependency_id): (PackageId, PackageId),
) -> Result<ConfigDryRes, Error> {
    let mut db = ctx.db.handle();
    let receipts = DependencyConfigReceipts::new(&mut db, &pkg_id, &dependency_id).await?;
    configure_logic(ctx, &mut db, (pkg_id, dependency_id), &receipts).await
}

pub async fn configure_logic(
    ctx: RpcContext,
    db: &mut PatchDbHandle,
    (pkg_id, dependency_id): (PackageId, PackageId),
    receipts: &DependencyConfigReceipts,
) -> Result<ConfigDryRes, Error> {
    let pkg_version = receipts.package_version.get(db).await?;
    let pkg_volumes = receipts.package_volumes.get(db).await?;
    let dependency_config_action = receipts.dependency_config_action.get(db).await?;
    let dependency_version = receipts.dependency_version.get(db).await?;
    let dependency_volumes = receipts.dependency_volumes.get(db).await?;
    let dependencies = receipts.dependencies.get(db).await?;

    let dependency = dependencies
        .0
        .get(&dependency_id)
        .ok_or_else(|| {
            Error::new(
                eyre!(
                    "dependency for {} not found in the manifest for {}",
                    dependency_id,
                    pkg_id
                ),
                crate::ErrorKind::NotFound,
            )
        })?
        .config
        .as_ref()
        .ok_or_else(|| {
            Error::new(
                eyre!(
                    "dependency config for {} not found on {}",
                    dependency_id,
                    pkg_id
                ),
                crate::ErrorKind::NotFound,
            )
        })?;
    let ConfigRes {
        config: maybe_config,
        spec,
    } = dependency_config_action
        .get(
            &ctx,
            &dependency_id,
            &dependency_version,
            &dependency_volumes,
        )
        .await?;

    let old_config = if let Some(config) = maybe_config {
        config
    } else {
        spec.gen(
            &mut rand::rngs::StdRng::from_entropy(),
            &Some(Duration::new(10, 0)),
        )?
    };

    let new_config = dependency
        .auto_configure
        .sandboxed(
            &ctx,
            &pkg_id,
            &pkg_version,
            &pkg_volumes,
            Some(&old_config),
            None,
            ProcedureName::AutoConfig(dependency_id.clone()),
        )
        .await?
        .map_err(|e| Error::new(eyre!("{}", e.1), crate::ErrorKind::AutoConfigure))?;

    Ok(ConfigDryRes {
        old_config,
        new_config,
        spec,
    })
}
#[instrument(skip(db, current_dependencies, current_dependent_receipt))]
pub async fn add_dependent_to_current_dependents_lists<
    'a,
    Db: DbHandle,
    I: IntoIterator<Item = (&'a PackageId, &'a CurrentDependencyInfo)>,
>(
    db: &mut Db,
    dependent_id: &PackageId,
    current_dependencies: I,
    current_dependent_receipt: &LockReceipt<BTreeMap<PackageId, CurrentDependencyInfo>, String>,
) -> Result<(), Error> {
    for (dependency, dep_info) in current_dependencies {
        if let Some(mut dependency_dependents) =
            current_dependent_receipt.get(db, dependency).await?
        {
            dependency_dependents.insert(dependent_id.clone(), dep_info.clone());
            current_dependent_receipt
                .set(db, dependency_dependents, dependency)
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
        receipts: &TryHealReceipts,
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
                .satisfied(ctx, db, dependency_id, None, &manifest.id, receipts)
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
    receipts: &'a BreakTransitiveReceipts,
) -> Result<(), Error> {
    for dependent in receipts
        .current_dependents
        .get(db, id)
        .await?
        .iter()
        .flat_map(|x| x.keys())
        .filter(|dependent| id != *dependent)
    {
        break_transitive(db, dependent, id, error.clone(), breakages, receipts).await?;
    }
    Ok(())
}

#[derive(Clone)]
pub struct BreakTransitiveReceipts {
    pub dependency_receipt: DependencyReceipt,
    dependency_errors: LockReceipt<DependencyErrors, String>,
    current_dependents: LockReceipt<BTreeMap<PackageId, CurrentDependencyInfo>, String>,
}

impl BreakTransitiveReceipts {
    pub async fn new(db: &'_ mut impl DbHandle) -> Result<Self, Error> {
        let mut locks = Vec::new();

        let setup = Self::setup(&mut locks);
        Ok(setup(&db.lock_all(locks).await?)?)
    }

    pub fn setup(locks: &mut Vec<LockTargetId>) -> impl FnOnce(&Verifier) -> Result<Self, Error> {
        let dependency_receipt = DependencyReceipt::setup(locks);
        let dependency_errors = crate::db::DatabaseModel::new()
            .package_data()
            .star()
            .installed()
            .map(|x| x.status().dependency_errors())
            .make_locker(LockType::Write)
            .add_to_keys(locks);
        let current_dependents = crate::db::DatabaseModel::new()
            .package_data()
            .star()
            .installed()
            .map(|x| x.current_dependents())
            .make_locker(LockType::Exist)
            .add_to_keys(locks);
        move |skeleton_key| {
            Ok(Self {
                dependency_receipt: dependency_receipt(skeleton_key)?,
                dependency_errors: dependency_errors.verify(skeleton_key)?,
                current_dependents: current_dependents.verify(skeleton_key)?,
            })
        }
    }
}

#[instrument(skip(db, receipts))]
pub fn break_transitive<'a, Db: DbHandle>(
    db: &'a mut Db,
    id: &'a PackageId,
    dependency: &'a PackageId,
    error: DependencyError,
    breakages: &'a mut BTreeMap<PackageId, TaggedDependencyError>,
    receipts: &'a BreakTransitiveReceipts,
) -> BoxFuture<'a, Result<(), Error>> {
    async move {
        let mut tx = db.begin().await?;
        let mut dependency_errors = receipts
            .dependency_errors
            .get(&mut tx, id)
            .await?
            .ok_or_else(not_found)?;

        let old = dependency_errors.0.remove(dependency);
        let newly_broken = if let Some(e) = &old {
            error.cmp_priority(&e) == Ordering::Greater
        } else {
            true
        };
        dependency_errors.0.insert(
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
            receipts
                .dependency_errors
                .set(&mut tx, dependency_errors, id)
                .await?;

            tx.save().await?;
            break_all_dependents_transitive(
                db,
                id,
                DependencyError::Transitive,
                breakages,
                receipts,
            )
            .await?;
        } else {
            receipts
                .dependency_errors
                .set(&mut tx, dependency_errors, id)
                .await?;

            tx.save().await?;
        }

        Ok(())
    }
    .boxed()
}

#[instrument(skip(ctx, db, locks))]
pub async fn heal_all_dependents_transitive<'a, Db: DbHandle>(
    ctx: &'a RpcContext,
    db: &'a mut Db,
    id: &'a PackageId,
    locks: &'a DependencyReceipt,
) -> Result<(), Error> {
    let dependents = locks
        .current_dependents
        .get(db, id)
        .await?
        .ok_or_else(not_found)?;
    for dependent in dependents.keys().filter(|dependent| id != *dependent) {
        heal_transitive(ctx, db, dependent, id, locks).await?;
    }
    Ok(())
}

#[instrument(skip(ctx, db, receipts))]
pub fn heal_transitive<'a, Db: DbHandle>(
    ctx: &'a RpcContext,
    db: &'a mut Db,
    id: &'a PackageId,
    dependency: &'a PackageId,
    receipts: &'a DependencyReceipt,
) -> BoxFuture<'a, Result<(), Error>> {
    async move {
        let mut status = receipts.status.get(db, id).await?.ok_or_else(not_found)?;

        let old = status.dependency_errors.0.remove(dependency);

        if let Some(old) = old {
            let info = receipts
                .dependency
                .get(db, (id, dependency))
                .await?
                .ok_or_else(not_found)?;
            if let Some(new) = old
                .try_heal(ctx, db, id, dependency, None, &info, &receipts.try_heal)
                .await?
            {
                status.dependency_errors.0.insert(dependency.clone(), new);
                receipts.status.set(db, status, id).await?;
            } else {
                receipts.status.set(db, status, id).await?;
                heal_all_dependents_transitive(ctx, db, id, receipts).await?;
            }
        }

        Ok(())
    }
    .boxed()
}

pub async fn reconfigure_dependents_with_live_pointers(
    ctx: &RpcContext,
    mut tx: impl DbHandle,
    receipts: &ConfigReceipts,
    pde: &InstalledPackageDataEntry,
) -> Result<(), Error> {
    let dependents = &pde.current_dependents;
    let me = &pde.manifest.id;
    for (dependent_id, dependency_info) in dependents {
        if dependency_info.pointers.iter().any(|ptr| match ptr {
            // dependency id matches the package being uninstalled
            PackagePointerSpec::TorAddress(ptr) => &ptr.package_id == me && dependent_id != me,
            PackagePointerSpec::LanAddress(ptr) => &ptr.package_id == me && dependent_id != me,
            // we never need to retarget these
            PackagePointerSpec::TorKey(_) => false,
            PackagePointerSpec::Config(_) => false,
        }) {
            crate::config::configure(
                ctx,
                &mut tx,
                dependent_id,
                None,
                &None,
                false,
                &mut BTreeMap::new(),
                &mut BTreeMap::new(),
                receipts,
            )
            .await?;
        }
    }
    Ok(())
}

#[derive(Clone)]
pub struct DependencyReceipt {
    pub try_heal: TryHealReceipts,
    current_dependents: LockReceipt<BTreeMap<PackageId, CurrentDependencyInfo>, String>,
    status: LockReceipt<Status, String>,
    dependency: LockReceipt<DepInfo, (String, String)>,
}

impl DependencyReceipt {
    pub async fn new<'a>(db: &'a mut impl DbHandle) -> Result<Self, Error> {
        let mut locks = Vec::new();

        let setup = Self::setup(&mut locks);
        Ok(setup(&db.lock_all(locks).await?)?)
    }

    pub fn setup(locks: &mut Vec<LockTargetId>) -> impl FnOnce(&Verifier) -> Result<Self, Error> {
        let try_heal = TryHealReceipts::setup(locks);
        let dependency = crate::db::DatabaseModel::new()
            .package_data()
            .star()
            .installed()
            .map(|x| x.manifest().dependencies().star())
            .make_locker(LockType::Read)
            .add_to_keys(locks);
        let current_dependents = crate::db::DatabaseModel::new()
            .package_data()
            .star()
            .installed()
            .map(|x| x.current_dependents())
            .make_locker(LockType::Write)
            .add_to_keys(locks);
        let status = crate::db::DatabaseModel::new()
            .package_data()
            .star()
            .installed()
            .map(|x| x.status())
            .make_locker(LockType::Write)
            .add_to_keys(locks);
        move |skeleton_key| {
            Ok(Self {
                try_heal: try_heal(skeleton_key)?,
                current_dependents: current_dependents.verify(skeleton_key)?,
                status: status.verify(skeleton_key)?,
                dependency: dependency.verify(skeleton_key)?,
            })
        }
    }
}
