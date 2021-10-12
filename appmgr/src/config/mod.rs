use std::collections::{BTreeMap, BTreeSet};
use std::time::Duration;

use bollard::container::KillContainerOptions;
use color_eyre::eyre::eyre;
use futures::future::{BoxFuture, FutureExt};
use indexmap::IndexSet;
use itertools::Itertools;
use patch_db::DbHandle;
use rand::SeedableRng;
use regex::Regex;
use rpc_toolkit::command;
use serde_json::Value;
use tracing::instrument;

use crate::action::docker::DockerAction;
use crate::context::RpcContext;
use crate::db::model::CurrentDependencyInfo;
use crate::db::util::WithRevision;
use crate::dependencies::{
    break_transitive, update_current_dependents, BreakageRes, DependencyError, DependencyErrors,
    TaggedDependencyError,
};
use crate::install::cleanup::remove_current_dependents;
use crate::s9pk::manifest::{Manifest, PackageId};
use crate::util::{
    display_none, display_serializable, parse_duration, parse_stdin_deserializable, IoFormat,
};
use crate::{Error, ResultExt as _};

pub mod action;
pub mod spec;
pub mod util;

pub use spec::{ConfigSpec, Defaultable};
use util::NumRange;

use self::action::ConfigRes;
use self::spec::{PackagePointerSpec, ValueSpecPointer};

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

#[command(subcommands(get, set))]
pub fn config(#[arg] id: PackageId) -> Result<PackageId, Error> {
    Ok(id)
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
    let pkg_model = crate::db::DatabaseModel::new()
        .package_data()
        .idx_model(&id)
        .and_then(|m| m.installed())
        .expect(&mut db)
        .await
        .with_kind(crate::ErrorKind::NotFound)?;
    let action = pkg_model
        .clone()
        .manifest()
        .config()
        .get(&mut db, true)
        .await?
        .to_owned()
        .ok_or_else(|| Error::new(eyre!("{} has no config", id), crate::ErrorKind::NotFound))?;
    let version = pkg_model
        .clone()
        .manifest()
        .version()
        .get(&mut db, true)
        .await?;
    let volumes = pkg_model.manifest().volumes().get(&mut db, true).await?;
    action.get(&ctx, &id, &*version, &*volumes).await
}

#[command(
    subcommands(self(set_impl(async, context(RpcContext))), set_dry),
    display(display_none)
)]
#[instrument]
pub fn set(
    #[parent_data] id: PackageId,
    #[allow(unused_variables)]
    #[arg(long = "format")]
    format: Option<IoFormat>,
    #[arg(long = "timeout", parse(parse_duration))] timeout: Option<Duration>,
    #[arg(stdin, parse(parse_stdin_deserializable))] config: Option<Config>,
    #[arg(rename = "expire-id", long = "expire-id")] expire_id: Option<String>,
) -> Result<(PackageId, Option<Config>, Option<Duration>, Option<String>), Error> {
    Ok((id, config, timeout, expire_id))
}

#[command(rename = "dry", display(display_serializable))]
#[instrument(skip(ctx))]
pub async fn set_dry(
    #[context] ctx: RpcContext,
    #[parent_data] (id, config, timeout, _): (
        PackageId,
        Option<Config>,
        Option<Duration>,
        Option<String>,
    ),
) -> Result<BreakageRes, Error> {
    let mut db = ctx.db.handle();
    let mut tx = db.begin().await?;
    let mut breakages = BTreeMap::new();
    configure(
        &ctx,
        &mut tx,
        &id,
        config,
        &timeout,
        true,
        &mut BTreeMap::new(),
        &mut breakages,
    )
    .await?;
    crate::db::DatabaseModel::new()
        .package_data()
        .idx_model(&id)
        .expect(&mut tx)
        .await?
        .installed()
        .expect(&mut tx)
        .await?
        .status()
        .configured()
        .put(&mut tx, &true)
        .await?;
    Ok(BreakageRes(breakages))
}

#[instrument(skip(ctx))]
pub async fn set_impl(
    ctx: RpcContext,
    (id, config, timeout, expire_id): (PackageId, Option<Config>, Option<Duration>, Option<String>),
) -> Result<WithRevision<()>, Error> {
    let mut db = ctx.db.handle();
    let mut tx = db.begin().await?;
    let mut breakages = BTreeMap::new();
    configure(
        &ctx,
        &mut tx,
        &id,
        config,
        &timeout,
        false,
        &mut BTreeMap::new(),
        &mut breakages,
    )
    .await?;
    crate::db::DatabaseModel::new()
        .package_data()
        .idx_model(&id)
        .expect(&mut tx)
        .await?
        .installed()
        .expect(&mut tx)
        .await?
        .status()
        .configured()
        .put(&mut tx, &true)
        .await?;
    Ok(WithRevision {
        response: (),
        revision: tx.commit(expire_id).await?,
    })
}

#[instrument(skip(ctx, db))]
pub fn configure<'a, Db: DbHandle>(
    ctx: &'a RpcContext,
    db: &'a mut Db,
    id: &'a PackageId,
    config: Option<Config>,
    timeout: &'a Option<Duration>,
    dry_run: bool,
    overrides: &'a mut BTreeMap<PackageId, Config>,
    breakages: &'a mut BTreeMap<PackageId, TaggedDependencyError>,
) -> BoxFuture<'a, Result<(), Error>> {
    async move {
        crate::db::DatabaseModel::new()
            .package_data()
            .lock(db, true)
            .await;
        // fetch data from db
        let pkg_model = crate::db::DatabaseModel::new()
            .package_data()
            .idx_model(id)
            .and_then(|m| m.installed())
            .expect(db)
            .await
            .with_kind(crate::ErrorKind::NotFound)?;
        let action = pkg_model
            .clone()
            .manifest()
            .config()
            .get(db, true)
            .await?
            .to_owned()
            .ok_or_else(|| Error::new(eyre!("{} has no config", id), crate::ErrorKind::NotFound))?;
        let version = pkg_model.clone().manifest().version().get(db, true).await?;
        let dependencies = pkg_model
            .clone()
            .manifest()
            .dependencies()
            .get(db, true)
            .await?;
        let volumes = pkg_model.clone().manifest().volumes().get(db, true).await?;

        // get current config and current spec
        let ConfigRes {
            config: old_config,
            spec,
        } = action.get(ctx, id, &*version, &*volumes).await?;

        // determine new config to use
        let mut config = if let Some(config) = config.or_else(|| old_config.clone()) {
            config
        } else {
            spec.gen(&mut rand::rngs::StdRng::from_entropy(), timeout)?
        };

        let manifest = crate::db::DatabaseModel::new()
            .package_data()
            .idx_model(id)
            .and_then(|m| m.installed())
            .map::<_, Manifest>(|i| i.manifest())
            .expect(db)
            .await?
            .get(db, true)
            .await
            .with_kind(crate::ErrorKind::NotFound)?;

        spec.validate(&*manifest)?;
        spec.matches(&config)?; // check that new config matches spec
        spec.update(ctx, db, &*manifest, &*overrides, &mut config)
            .await?; // dereference pointers in the new config

        // create backreferences to pointers
        let mut sys = pkg_model.clone().system_pointers().get_mut(db).await?;
        sys.truncate(0);
        let mut current_dependencies: BTreeMap<PackageId, CurrentDependencyInfo> = dependencies
            .0
            .iter()
            .filter_map(|(id, info)| {
                if info.requirement.required() {
                    Some((id.clone(), CurrentDependencyInfo::default()))
                } else {
                    None
                }
            })
            .collect();
        for ptr in spec.pointers(&config)? {
            match ptr {
                ValueSpecPointer::Package(pkg_ptr) => {
                    if let Some(current_dependency) =
                        current_dependencies.get_mut(pkg_ptr.package_id())
                    {
                        current_dependency.pointers.push(pkg_ptr);
                    } else {
                        current_dependencies.insert(
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
        sys.save(db).await?;

        let signal = if !dry_run {
            // run config action
            let res = action
                .set(ctx, id, &*version, &*dependencies, &*volumes, &config)
                .await?;

            // track dependencies with no pointers
            for (package_id, health_checks) in res.depends_on.into_iter() {
                if let Some(current_dependency) = current_dependencies.get_mut(&package_id) {
                    current_dependency.health_checks.extend(health_checks);
                } else {
                    current_dependencies.insert(
                        package_id,
                        CurrentDependencyInfo {
                            pointers: Vec::new(),
                            health_checks,
                        },
                    );
                }
            }

            // track dependency health checks
            current_dependencies = current_dependencies
                .into_iter()
                .filter(|(dep_id, _)| {
                    if dep_id != id && !manifest.dependencies.0.contains_key(dep_id) {
                        tracing::warn!("Illegal dependency specified: {}", dep_id);
                        false
                    } else {
                        true
                    }
                })
                .collect();
            res.signal
        } else {
            None
        };

        // update dependencies
        let mut deps = pkg_model.clone().current_dependencies().get_mut(db).await?;
        remove_current_dependents(db, id, deps.keys()).await?;
        *deps = current_dependencies.clone();
        deps.save(db).await?;
        update_current_dependents(db, id, &current_dependencies).await?;
        let mut errs = pkg_model
            .clone()
            .status()
            .dependency_errors()
            .get_mut(db)
            .await?;
        *errs = DependencyErrors::init(ctx, db, &*manifest, &current_dependencies).await?;
        errs.save(db).await?;

        // cache current config for dependents
        overrides.insert(id.clone(), config.clone());

        // handle dependents
        let dependents = pkg_model.clone().current_dependents().get(db, true).await?;
        let prev = old_config.map(Value::Object).unwrap_or_default();
        let next = Value::Object(config.clone());
        for (dependent, dep_info) in dependents.iter().filter(|(dep_id, _)| dep_id != &id) {
            // check if config passes dependent check
            let dependent_model = crate::db::DatabaseModel::new()
                .package_data()
                .idx_model(dependent)
                .and_then(|pkg| pkg.installed())
                .expect(db)
                .await?;
            if let Some(cfg) = &*dependent_model
                .clone()
                .manifest()
                .dependencies()
                .idx_model(id)
                .expect(db)
                .await?
                .config()
                .get(db, true)
                .await?
            {
                let manifest = dependent_model.clone().manifest().get(db, true).await?;
                if let Err(error) = cfg
                    .check(
                        ctx,
                        dependent,
                        &manifest.version,
                        &manifest.volumes,
                        &config,
                    )
                    .await?
                {
                    let dep_err = DependencyError::ConfigUnsatisfied { error };
                    break_transitive(db, dependent, id, dep_err, breakages).await?;
                }

                // handle backreferences
                for ptr in &dep_info.pointers {
                    if let PackagePointerSpec::Config(cfg_ptr) = ptr {
                        if cfg_ptr.select(&next) != cfg_ptr.select(&prev) {
                            if let Err(e) = configure(
                                ctx, db, dependent, None, timeout, dry_run, overrides, breakages,
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
                                    )
                                    .await?;
                                } else {
                                    return Err(e);
                                }
                            }
                        }
                    }
                }
            }
        }

        if let Some(signal) = signal {
            ctx.docker
                .kill_container(
                    &DockerAction::container_name(id, None),
                    Some(KillContainerOptions {
                        signal: signal.to_string(),
                    }),
                )
                .await
                // ignore container is not running https://docs.docker.com/engine/api/v1.41/#operation/ContainerKill
                .or_else(|e| {
                    if matches!(
                        e,
                        bollard::errors::Error::DockerResponseConflictError { .. }
                            | bollard::errors::Error::DockerResponseNotFoundError { .. }
                    ) {
                        Ok(())
                    } else {
                        Err(e)
                    }
                })?;
        }

        Ok(())
    }
    .boxed()
}
