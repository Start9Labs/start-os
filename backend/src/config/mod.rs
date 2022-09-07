use std::collections::{BTreeMap, BTreeSet};
use std::path::PathBuf;
use std::time::Duration;

use color_eyre::eyre::eyre;
use futures::future::{BoxFuture, FutureExt};
use indexmap::IndexSet;
use itertools::Itertools;
use patch_db::{DbHandle, LockReceipt, LockTarget, LockTargetId, LockType, Verifier};
use rand::SeedableRng;
use regex::Regex;
use rpc_toolkit::command;
use serde_json::Value;
use tracing::instrument;

use crate::context::RpcContext;
use crate::db::model::{CurrentDependencies, CurrentDependencyInfo, CurrentDependents};
use crate::dependencies::{
    add_dependent_to_current_dependents_lists, break_transitive, heal_all_dependents_transitive,
    BreakTransitiveReceipts, BreakageRes, Dependencies, DependencyConfig, DependencyError,
    DependencyErrors, DependencyReceipt, TaggedDependencyError, TryHealReceipts,
};
use crate::install::cleanup::{remove_from_current_dependents_lists, UpdateDependencyReceipts};
use crate::s9pk::manifest::{Manifest, PackageId};
use crate::util::display_none;
use crate::util::serde::{display_serializable, parse_stdin_deserializable, IoFormat};
use crate::Error;

pub mod action;
pub mod spec;
pub mod util;

pub use spec::{ConfigSpec, Defaultable};
use util::NumRange;

use self::action::{ConfigActions, ConfigRes};
use self::spec::{ConfigPointerReceipts, PackagePointerSpec, ValueSpecPointer};

pub type Config = serde_json::Map<String, Value>;
pub trait TypeOf {
    fn type_of(&self) -> &'static str;
}
impl TypeOf for Value {
    fn type_of(&self) -> &'static str {
        match self {
            Value::Array(_) => "list",
            Value::Bool(_) => "boolean",
            Value::Null => "null",
            Value::Number(_) => "number",
            Value::Object(_) => "object",
            Value::String(_) => "string",
        }
    }
}

#[derive(Debug, thiserror::Error)]
pub enum ConfigurationError {
    #[error("Timeout Error")]
    TimeoutError(#[from] TimeoutError),
    #[error("No Match: {0}")]
    NoMatch(#[from] NoMatchWithPath),
    #[error("System Error: {0}")]
    SystemError(Error),
    #[error("Permission Denied: {0}")]
    PermissionDenied(ValueSpecPointer),
}
impl From<ConfigurationError> for Error {
    fn from(err: ConfigurationError) -> Self {
        let kind = match &err {
            ConfigurationError::SystemError(e) => e.kind,
            _ => crate::ErrorKind::ConfigGen,
        };
        crate::Error::new(err, kind)
    }
}

#[derive(Clone, Copy, Debug, thiserror::Error)]
#[error("Timeout Error")]
pub struct TimeoutError;

#[derive(Clone, Debug, thiserror::Error)]
pub struct NoMatchWithPath {
    pub path: Vec<String>,
    pub error: MatchError,
}
impl NoMatchWithPath {
    pub fn new(error: MatchError) -> Self {
        NoMatchWithPath {
            path: Vec::new(),
            error,
        }
    }
    pub fn prepend(mut self, seg: String) -> Self {
        self.path.push(seg);
        self
    }
}
impl std::fmt::Display for NoMatchWithPath {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}: {}", self.path.iter().rev().join("."), self.error)
    }
}
impl From<NoMatchWithPath> for Error {
    fn from(e: NoMatchWithPath) -> Self {
        ConfigurationError::from(e).into()
    }
}

#[derive(Clone, Debug, thiserror::Error)]
pub enum MatchError {
    #[error("String {0:?} Does Not Match Pattern {1}")]
    Pattern(String, Regex),
    #[error("String {0:?} Is Not In Enum {1:?}")]
    Enum(String, IndexSet<String>),
    #[error("Field Is Not Nullable")]
    NotNullable,
    #[error("Length Mismatch: expected {0}, actual: {1}")]
    LengthMismatch(NumRange<usize>, usize),
    #[error("Invalid Type: expected {0}, actual: {1}")]
    InvalidType(&'static str, &'static str),
    #[error("Number Out Of Range: expected {0}, actual: {1}")]
    OutOfRange(NumRange<f64>, f64),
    #[error("Number Is Not Integral: {0}")]
    NonIntegral(f64),
    #[error("Variant {0:?} Is Not In Union {1:?}")]
    Union(String, IndexSet<String>),
    #[error("Variant Is Missing Tag {0:?}")]
    MissingTag(String),
    #[error("Property {0:?} Of Variant {1:?} Conflicts With Union Tag")]
    PropertyMatchesUnionTag(String, String),
    #[error("Name of Property {0:?} Conflicts With Map Tag Name")]
    PropertyNameMatchesMapTag(String),
    #[error("Pointer Is Invalid: {0}")]
    InvalidPointer(spec::ValueSpecPointer),
    #[error("Object Key Is Invalid: {0}")]
    InvalidKey(String),
    #[error("Value In List Is Not Unique")]
    ListUniquenessViolation,
}

#[command(rename = "config-spec", cli_only, blocking, display(display_none))]
pub fn verify_spec(#[arg] path: PathBuf) -> Result<(), Error> {
    let mut file = std::fs::File::open(&path)?;
    let format = match path.extension().and_then(|s| s.to_str()) {
        Some("yaml") | Some("yml") => IoFormat::Yaml,
        Some("json") => IoFormat::Json,
        Some("toml") => IoFormat::Toml,
        Some("cbor") => IoFormat::Cbor,
        _ => {
            return Err(Error::new(
                eyre!("Unknown file format. Expected one of yaml, json, toml, cbor."),
                crate::ErrorKind::Deserialization,
            ));
        }
    };
    let _: ConfigSpec = format.from_reader(&mut file)?;

    Ok(())
}

#[command(subcommands(get, set))]
pub fn config(#[arg] id: PackageId) -> Result<PackageId, Error> {
    Ok(id)
}

pub struct ConfigGetReceipts {
    manifest_volumes: LockReceipt<crate::volume::Volumes, ()>,
    manifest_version: LockReceipt<crate::util::Version, ()>,
    manifest_config: LockReceipt<Option<ConfigActions>, ()>,
}

impl ConfigGetReceipts {
    pub async fn new<'a>(db: &'a mut impl DbHandle, id: &PackageId) -> Result<Self, Error> {
        let mut locks = Vec::new();

        let setup = Self::setup(&mut locks, id);
        Ok(setup(&db.lock_all(locks).await?)?)
    }

    pub fn setup(
        locks: &mut Vec<LockTargetId>,
        id: &PackageId,
    ) -> impl FnOnce(&Verifier) -> Result<Self, Error> {
        let manifest_version = crate::db::DatabaseModel::new()
            .package_data()
            .idx_model(id)
            .and_then(|x| x.installed())
            .map(|x| x.manifest().version())
            .make_locker(LockType::Write)
            .add_to_keys(locks);
        let manifest_volumes = crate::db::DatabaseModel::new()
            .package_data()
            .idx_model(id)
            .and_then(|x| x.installed())
            .map(|x| x.manifest().volumes())
            .make_locker(LockType::Write)
            .add_to_keys(locks);
        let manifest_config = crate::db::DatabaseModel::new()
            .package_data()
            .idx_model(id)
            .and_then(|x| x.installed())
            .map(|x| x.manifest().config())
            .make_locker(LockType::Write)
            .add_to_keys(locks);
        move |skeleton_key| {
            Ok(Self {
                manifest_volumes: manifest_volumes.verify(skeleton_key)?,
                manifest_version: manifest_version.verify(skeleton_key)?,
                manifest_config: manifest_config.verify(skeleton_key)?,
            })
        }
    }
}

#[command(display(display_serializable))]
#[instrument(skip(ctx))]
pub async fn get(
    #[context] ctx: RpcContext,
    #[parent_data] id: PackageId,
    #[allow(unused_variables)]
    #[arg(long = "format")]
    format: Option<IoFormat>,
) -> Result<ConfigRes, Error> {
    let mut db = ctx.db.handle();
    let receipts = ConfigGetReceipts::new(&mut db, &id).await?;
    let action = receipts
        .manifest_config
        .get(&mut db)
        .await?
        .ok_or_else(|| Error::new(eyre!("{} has no config", id), crate::ErrorKind::NotFound))?;

    let volumes = receipts.manifest_volumes.get(&mut db).await?;
    let version = receipts.manifest_version.get(&mut db).await?;
    action.get(&ctx, &id, &version, &volumes).await
}

#[command(
    subcommands(self(set_impl(async, context(RpcContext))), set_dry),
    display(display_none),
    metadata(sync_db = true)
)]
#[instrument]
pub fn set(
    #[parent_data] id: PackageId,
    #[allow(unused_variables)]
    #[arg(long = "format")]
    format: Option<IoFormat>,
    #[arg(long = "timeout")] timeout: Option<crate::util::serde::Duration>,
    #[arg(stdin, parse(parse_stdin_deserializable))] config: Option<Config>,
) -> Result<(PackageId, Option<Config>, Option<Duration>), Error> {
    Ok((id, config, timeout.map(|d| *d)))
}

/// So, the new locking finds all the possible locks and lifts them up into a bundle of locks.
/// Then this bundle will be passed down into the functions that will need to touch the db, and
/// instead of doing the locks down in the system, we have already done the locks and can
/// do the operation on the db.
/// An UnlockedLock has two types, the type of setting and getting from the db, and the second type
/// is the keys that we need to insert on getting/setting because we have included wild cards into the paths.
pub struct ConfigReceipts {
    pub dependency_receipt: DependencyReceipt,
    pub config_receipts: ConfigPointerReceipts,
    pub update_dependency_receipts: UpdateDependencyReceipts,
    pub try_heal_receipts: TryHealReceipts,
    pub break_transitive_receipts: BreakTransitiveReceipts,
    configured: LockReceipt<bool, String>,
    config_actions: LockReceipt<ConfigActions, String>,
    dependencies: LockReceipt<Dependencies, String>,
    volumes: LockReceipt<crate::volume::Volumes, String>,
    version: LockReceipt<crate::util::Version, String>,
    manifest: LockReceipt<Manifest, String>,
    system_pointers: LockReceipt<Vec<spec::SystemPointerSpec>, String>,
    pub current_dependents: LockReceipt<CurrentDependents, String>,
    pub current_dependencies: LockReceipt<CurrentDependencies, String>,
    dependency_errors: LockReceipt<DependencyErrors, String>,
    manifest_dependencies_config: LockReceipt<DependencyConfig, (String, String)>,
}

impl ConfigReceipts {
    pub async fn new<'a>(db: &'a mut impl DbHandle) -> Result<Self, Error> {
        let mut locks = Vec::new();

        let setup = Self::setup(&mut locks);
        Ok(setup(&db.lock_all(locks).await?)?)
    }

    pub fn setup(locks: &mut Vec<LockTargetId>) -> impl FnOnce(&Verifier) -> Result<Self, Error> {
        let dependency_receipt = DependencyReceipt::setup(locks);
        let config_receipts = ConfigPointerReceipts::setup(locks);
        let update_dependency_receipts = UpdateDependencyReceipts::setup(locks);
        let break_transitive_receipts = BreakTransitiveReceipts::setup(locks);
        let try_heal_receipts = TryHealReceipts::setup(locks);

        let configured: LockTarget<bool, String> = crate::db::DatabaseModel::new()
            .package_data()
            .star()
            .installed()
            .map(|x| x.status().configured())
            .make_locker(LockType::Write)
            .add_to_keys(locks);

        let config_actions = crate::db::DatabaseModel::new()
            .package_data()
            .star()
            .installed()
            .and_then(|x| x.manifest().config())
            .make_locker(LockType::Read)
            .add_to_keys(locks);

        let dependencies = crate::db::DatabaseModel::new()
            .package_data()
            .star()
            .installed()
            .map(|x| x.manifest().dependencies())
            .make_locker(LockType::Read)
            .add_to_keys(locks);

        let volumes = crate::db::DatabaseModel::new()
            .package_data()
            .star()
            .installed()
            .map(|x| x.manifest().volumes())
            .make_locker(LockType::Read)
            .add_to_keys(locks);

        let version = crate::db::DatabaseModel::new()
            .package_data()
            .star()
            .installed()
            .map(|x| x.manifest().version())
            .make_locker(LockType::Read)
            .add_to_keys(locks);

        let manifest = crate::db::DatabaseModel::new()
            .package_data()
            .star()
            .installed()
            .map(|x| x.manifest())
            .make_locker(LockType::Read)
            .add_to_keys(locks);

        let system_pointers = crate::db::DatabaseModel::new()
            .package_data()
            .star()
            .installed()
            .map(|x| x.system_pointers())
            .make_locker(LockType::Write)
            .add_to_keys(locks);

        let current_dependents = crate::db::DatabaseModel::new()
            .package_data()
            .star()
            .installed()
            .map(|x| x.current_dependents())
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

        let manifest_dependencies_config = crate::db::DatabaseModel::new()
            .package_data()
            .star()
            .installed()
            .and_then(|x| x.manifest().dependencies().star().config())
            .make_locker(LockType::Write)
            .add_to_keys(locks);

        move |skeleton_key| {
            Ok(Self {
                dependency_receipt: dependency_receipt(skeleton_key)?,
                config_receipts: config_receipts(skeleton_key)?,
                try_heal_receipts: try_heal_receipts(skeleton_key)?,
                break_transitive_receipts: break_transitive_receipts(skeleton_key)?,
                update_dependency_receipts: update_dependency_receipts(skeleton_key)?,
                configured: configured.verify(skeleton_key)?,
                config_actions: config_actions.verify(skeleton_key)?,
                dependencies: dependencies.verify(skeleton_key)?,
                volumes: volumes.verify(skeleton_key)?,
                version: version.verify(skeleton_key)?,
                manifest: manifest.verify(skeleton_key)?,
                system_pointers: system_pointers.verify(skeleton_key)?,
                current_dependents: current_dependents.verify(skeleton_key)?,
                current_dependencies: current_dependencies.verify(skeleton_key)?,
                dependency_errors: dependency_errors.verify(skeleton_key)?,
                manifest_dependencies_config: manifest_dependencies_config.verify(skeleton_key)?,
            })
        }
    }
}

#[command(rename = "dry", display(display_serializable))]
#[instrument(skip(ctx))]
pub async fn set_dry(
    #[context] ctx: RpcContext,
    #[parent_data] (id, config, timeout): (PackageId, Option<Config>, Option<Duration>),
) -> Result<BreakageRes, Error> {
    let mut db = ctx.db.handle();
    let mut tx = db.begin().await?;
    let mut breakages = BTreeMap::new();
    let locks = ConfigReceipts::new(&mut tx).await?;
    configure(
        &ctx,
        &mut tx,
        &id,
        config,
        &timeout,
        true,
        &mut BTreeMap::new(),
        &mut breakages,
        &locks,
    )
    .await?;

    locks.configured.set(&mut tx, true, &id).await?;
    tx.abort().await?;
    Ok(BreakageRes(breakages))
}

#[instrument(skip(ctx))]
pub async fn set_impl(
    ctx: RpcContext,
    (id, config, timeout): (PackageId, Option<Config>, Option<Duration>),
) -> Result<(), Error> {
    let mut db = ctx.db.handle();
    let mut tx = db.begin().await?;
    let mut breakages = BTreeMap::new();
    let locks = ConfigReceipts::new(&mut tx).await?;
    configure(
        &ctx,
        &mut tx,
        &id,
        config,
        &timeout,
        false,
        &mut BTreeMap::new(),
        &mut breakages,
        &locks,
    )
    .await?;
    tx.commit().await?;
    Ok(())
}

#[instrument(skip(ctx, db, receipts))]
pub async fn configure<'a, Db: DbHandle>(
    ctx: &RpcContext,
    db: &'a mut Db,
    id: &PackageId,
    config: Option<Config>,
    timeout: &Option<Duration>,
    dry_run: bool,
    overrides: &mut BTreeMap<PackageId, Config>,
    breakages: &mut BTreeMap<PackageId, TaggedDependencyError>,
    receipts: &ConfigReceipts,
) -> Result<(), Error> {
    configure_rec(
        ctx, db, id, config, timeout, dry_run, overrides, breakages, receipts,
    )
    .await?;
    receipts.configured.set(db, true, &id).await?;
    Ok(())
}

#[instrument(skip(ctx, db, receipts))]
pub fn configure_rec<'a, Db: DbHandle>(
    ctx: &'a RpcContext,
    db: &'a mut Db,
    id: &'a PackageId,
    config: Option<Config>,
    timeout: &'a Option<Duration>,
    dry_run: bool,
    overrides: &'a mut BTreeMap<PackageId, Config>,
    breakages: &'a mut BTreeMap<PackageId, TaggedDependencyError>,
    receipts: &'a ConfigReceipts,
) -> BoxFuture<'a, Result<(), Error>> {
    async move {
        // fetch data from db
        let action = receipts
            .config_actions
            .get(db, id)
            .await?
            .ok_or_else(not_found)?;
        let dependencies = receipts
            .dependencies
            .get(db, id)
            .await?
            .ok_or_else(not_found)?;
        let volumes = receipts.volumes.get(db, id).await?.ok_or_else(not_found)?;
        let is_needs_config = !receipts
            .configured
            .get(db, id)
            .await?
            .ok_or_else(not_found)?;
        let version = receipts.version.get(db, id).await?.ok_or_else(not_found)?;

        // get current config and current spec
        let ConfigRes {
            config: old_config,
            spec,
        } = action.get(ctx, id, &version, &volumes).await?;

        // determine new config to use
        let mut config = if let Some(config) = config.or_else(|| old_config.clone()) {
            config
        } else {
            spec.gen(&mut rand::rngs::StdRng::from_entropy(), timeout)?
        };

        let manifest = receipts.manifest.get(db, id).await?.ok_or_else(not_found)?;

        spec.validate(&manifest)?;
        spec.matches(&config)?; // check that new config matches spec
        spec.update(
            ctx,
            db,
            &manifest,
            &*overrides,
            &mut config,
            &receipts.config_receipts,
        )
        .await?; // dereference pointers in the new config

        // create backreferences to pointers
        let mut sys = receipts
            .system_pointers
            .get(db, &id)
            .await?
            .ok_or_else(not_found)?;
        sys.truncate(0);
        let mut current_dependencies: CurrentDependencies = CurrentDependencies(
            dependencies
                .0
                .iter()
                .filter_map(|(id, info)| {
                    if info.requirement.required() {
                        Some((id.clone(), CurrentDependencyInfo::default()))
                    } else {
                        None
                    }
                })
                .collect(),
        );
        for ptr in spec.pointers(&config)? {
            match ptr {
                ValueSpecPointer::Package(pkg_ptr) => {
                    if let Some(current_dependency) =
                        current_dependencies.0.get_mut(pkg_ptr.package_id())
                    {
                        current_dependency.pointers.push(pkg_ptr);
                    } else {
                        current_dependencies.0.insert(
                            pkg_ptr.package_id().to_owned(),
                            CurrentDependencyInfo {
                                pointers: vec![pkg_ptr],
                                health_checks: BTreeSet::new(),
                            },
                        );
                    }
                }
                ValueSpecPointer::System(s) => sys.push(s),
            }
        }
        receipts.system_pointers.set(db, sys, &id).await?;

        let signal = if !dry_run {
            // run config action
            let res = action
                .set(ctx, id, &version, &dependencies, &volumes, &config)
                .await?;

            // track dependencies with no pointers
            for (package_id, health_checks) in res.depends_on.into_iter() {
                if let Some(current_dependency) = current_dependencies.0.get_mut(&package_id) {
                    current_dependency.health_checks.extend(health_checks);
                } else {
                    current_dependencies.0.insert(
                        package_id,
                        CurrentDependencyInfo {
                            pointers: Vec::new(),
                            health_checks,
                        },
                    );
                }
            }

            // track dependency health checks
            current_dependencies = current_dependencies.map(|x| {
                x.into_iter()
                    .filter(|(dep_id, _)| {
                        if dep_id != id && !manifest.dependencies.0.contains_key(dep_id) {
                            tracing::warn!("Illegal dependency specified: {}", dep_id);
                            false
                        } else {
                            true
                        }
                    })
                    .collect()
            });
            res.signal
        } else {
            None
        };

        // update dependencies
        let prev_current_dependencies = receipts
            .current_dependencies
            .get(db, &id)
            .await?
            .unwrap_or_default();
        remove_from_current_dependents_lists(
            db,
            id,
            &prev_current_dependencies,
            &receipts.current_dependents,
        )
        .await?; // remove previous
        add_dependent_to_current_dependents_lists(
            db,
            id,
            &current_dependencies,
            &receipts.current_dependents,
        )
        .await?; // add new
        current_dependencies.0.remove(id);
        receipts
            .current_dependencies
            .set(db, current_dependencies.clone(), &id)
            .await?;

        let errs = receipts
            .dependency_errors
            .get(db, &id)
            .await?
            .ok_or_else(not_found)?;
        tracing::warn!("Dependency Errors: {:?}", errs);
        let errs = DependencyErrors::init(
            ctx,
            db,
            &manifest,
            &current_dependencies,
            &receipts.dependency_receipt.try_heal,
        )
        .await?;
        receipts.dependency_errors.set(db, errs, &id).await?;

        // cache current config for dependents
        overrides.insert(id.clone(), config.clone());

        // handle dependents
        let dependents = receipts
            .current_dependents
            .get(db, id)
            .await?
            .ok_or_else(not_found)?;
        let prev = if is_needs_config { None } else { old_config }
            .map(Value::Object)
            .unwrap_or_default();
        let next = Value::Object(config.clone());
        for (dependent, dep_info) in dependents.0.iter().filter(|(dep_id, _)| dep_id != &id) {
            // check if config passes dependent check
            if let Some(cfg) = receipts
                .manifest_dependencies_config
                .get(db, (&dependent, &id))
                .await?
            {
                let manifest = receipts
                    .manifest
                    .get(db, &dependent)
                    .await?
                    .ok_or_else(not_found)?;
                if let Err(error) = cfg
                    .check(
                        ctx,
                        dependent,
                        &manifest.version,
                        &manifest.volumes,
                        id,
                        &config,
                    )
                    .await?
                {
                    let dep_err = DependencyError::ConfigUnsatisfied { error };
                    break_transitive(
                        db,
                        dependent,
                        id,
                        dep_err,
                        breakages,
                        &receipts.break_transitive_receipts,
                    )
                    .await?;
                }

                // handle backreferences
                for ptr in &dep_info.pointers {
                    if let PackagePointerSpec::Config(cfg_ptr) = ptr {
                        if cfg_ptr.select(&next) != cfg_ptr.select(&prev) {
                            if let Err(e) = configure_rec(
                                ctx, db, dependent, None, timeout, dry_run, overrides, breakages,
                                receipts,
                            )
                            .await
                            {
                                if e.kind == crate::ErrorKind::ConfigRulesViolation {
                                    break_transitive(
                                        db,
                                        dependent,
                                        id,
                                        DependencyError::ConfigUnsatisfied {
                                            error: format!("{}", e),
                                        },
                                        breakages,
                                        &receipts.break_transitive_receipts,
                                    )
                                    .await?;
                                } else {
                                    return Err(e);
                                }
                            }
                        }
                    }
                }
                heal_all_dependents_transitive(ctx, db, id, &receipts.dependency_receipt).await?;
            }
        }

        if let Some(signal) = signal {
            match ctx.managers.get(&(id.clone(), version.clone())).await {
                None => {
                    // in theory this should never happen, which indicates this function should be moved behind the
                    // Manager interface
                    return Err(Error::new(
                        eyre!("Manager Not Found for package being configured"),
                        crate::ErrorKind::Incoherent,
                    ));
                }
                Some(m) => {
                    m.signal(&signal).await?;
                }
            }
        }

        Ok(())
    }
    .boxed()
}
#[instrument]
pub fn not_found() -> Error {
    Error::new(eyre!("Could not find"), crate::ErrorKind::Incoherent)
}

/// We want to have a double check that the paths are what we expect them to be.
/// Found that earlier the paths where not what we expected them to be.
#[tokio::test]
async fn ensure_creation_of_config_paths_makes_sense() {
    let mut fake = patch_db::test_utils::NoOpDb();
    let config_locks = ConfigReceipts::new(&mut fake).await.unwrap();
    assert_eq!(
        &format!("{}", config_locks.configured.lock.glob),
        "/package-data/*/installed/status/configured"
    );
    assert_eq!(
        &format!("{}", config_locks.config_actions.lock.glob),
        "/package-data/*/installed/manifest/config"
    );
    assert_eq!(
        &format!("{}", config_locks.dependencies.lock.glob),
        "/package-data/*/installed/manifest/dependencies"
    );
    assert_eq!(
        &format!("{}", config_locks.volumes.lock.glob),
        "/package-data/*/installed/manifest/volumes"
    );
    assert_eq!(
        &format!("{}", config_locks.version.lock.glob),
        "/package-data/*/installed/manifest/version"
    );
    assert_eq!(
        &format!("{}", config_locks.volumes.lock.glob),
        "/package-data/*/installed/manifest/volumes"
    );
    assert_eq!(
        &format!("{}", config_locks.manifest.lock.glob),
        "/package-data/*/installed/manifest"
    );
    assert_eq!(
        &format!("{}", config_locks.manifest.lock.glob),
        "/package-data/*/installed/manifest"
    );
    assert_eq!(
        &format!("{}", config_locks.system_pointers.lock.glob),
        "/package-data/*/installed/system-pointers"
    );
    assert_eq!(
        &format!("{}", config_locks.current_dependents.lock.glob),
        "/package-data/*/installed/current-dependents"
    );
    assert_eq!(
        &format!("{}", config_locks.dependency_errors.lock.glob),
        "/package-data/*/installed/status/dependency-errors"
    );
    assert_eq!(
        &format!("{}", config_locks.manifest_dependencies_config.lock.glob),
        "/package-data/*/installed/manifest/dependencies/*/config"
    );
    assert_eq!(
        &format!("{}", config_locks.system_pointers.lock.glob),
        "/package-data/*/installed/system-pointers"
    );
}
