use std::collections::BTreeMap;
use std::path::PathBuf;
use std::time::Duration;

use color_eyre::eyre::eyre;
use indexmap::IndexSet;
use itertools::Itertools;
use models::ErrorKind;
use patch_db::{DbHandle, LockReceipt, LockTarget, LockTargetId, LockType, Verifier};
use regex::Regex;
use rpc_toolkit::command;
use serde_json::Value;
use tracing::instrument;

use crate::context::RpcContext;
use crate::db::model::{CurrentDependencies, CurrentDependents};
use crate::dependencies::{
    BreakTransitiveReceipts, BreakageRes, Dependencies, DependencyConfig, DependencyErrors,
    DependencyReceipt, TaggedDependencyError, TryHealReceipts,
};
use crate::install::cleanup::UpdateDependencyReceipts;
use crate::procedure::docker::DockerContainers;
use crate::s9pk::manifest::{Manifest, PackageId};
use crate::util::display_none;
use crate::util::serde::{display_serializable, parse_stdin_deserializable, IoFormat};
use crate::Error;

pub mod action;
pub mod hook;
pub mod spec;
pub mod util;

pub use spec::{ConfigSpec, Defaultable};
use util::NumRange;

use self::action::{ConfigActions, ConfigRes};
use self::spec::{ConfigPointerReceipts, ValueSpecPointer};

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
    pub configured: LockReceipt<bool, String>,
    pub config_actions: LockReceipt<ConfigActions, String>,
    pub dependencies: LockReceipt<Dependencies, String>,
    pub volumes: LockReceipt<crate::volume::Volumes, String>,
    pub version: LockReceipt<crate::util::Version, String>,
    pub manifest: LockReceipt<Manifest, String>,
    pub system_pointers: LockReceipt<Vec<spec::SystemPointerSpec>, String>,
    pub current_dependents: LockReceipt<CurrentDependents, String>,
    pub current_dependencies: LockReceipt<CurrentDependencies, String>,
    pub dependency_errors: LockReceipt<DependencyErrors, String>,
    pub manifest_dependencies_config: LockReceipt<DependencyConfig, (String, String)>,
    pub docker_containers: LockReceipt<DockerContainers, String>,
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
        let docker_containers = crate::db::DatabaseModel::new()
            .package_data()
            .star()
            .installed()
            .and_then(|x| x.manifest().containers())
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
                docker_containers: docker_containers.verify(skeleton_key)?,
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
    let breakages = BTreeMap::new();
    let overrides = Default::default();

    let configure_context = ConfigureContext {
        breakages,
        timeout,
        config,
        dry_run: true,
        overrides,
    };
    let breakages = configure(&ctx, &id, configure_context).await?;

    Ok(BreakageRes(breakages))
}

pub struct ConfigureContext {
    pub breakages: BTreeMap<PackageId, TaggedDependencyError>,
    pub timeout: Option<Duration>,
    pub config: Option<Config>,
    pub overrides: BTreeMap<PackageId, Config>,
    pub dry_run: bool,
}

#[instrument(skip(ctx))]
pub async fn set_impl(
    ctx: RpcContext,
    (id, config, timeout): (PackageId, Option<Config>, Option<Duration>),
) -> Result<(), Error> {
    let breakages = BTreeMap::new();
    let overrides = Default::default();

    let configure_context = ConfigureContext {
        breakages,
        timeout,
        config,
        dry_run: false,
        overrides,
    };
    configure(&ctx, &id, configure_context).await?;
    Ok(())
}

#[instrument(skip(ctx, configure_context))]
pub async fn configure(
    ctx: &RpcContext,
    id: &PackageId,
    configure_context: ConfigureContext,
) -> Result<BTreeMap<PackageId, TaggedDependencyError>, Error> {
    let mut db = ctx.db.handle();
    let version = crate::db::DatabaseModel::new()
        .package_data()
        .idx_model(id)
        .expect(&mut db)
        .await?
        .installed()
        .expect(&mut db)
        .await?
        .manifest()
        .version()
        .get(&mut ctx.db.handle())
        .await?;
    ctx.managers
        .get(&(id.clone(), version.clone()))
        .await
        .ok_or_else(|| {
            Error::new(
                eyre!("There is no manager running for {id:?} and {version:?}"),
                ErrorKind::Unknown,
            )
        })?
        .configure(configure_context)
        .await
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
