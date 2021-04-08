use std::time::Duration;

use anyhow::anyhow;
use bollard::container::KillContainerOptions;
use bollard::Docker;
use futures::future::{BoxFuture, FutureExt};
use indexmap::{IndexMap, IndexSet};
use itertools::Itertools;
use patch_db::DbHandle;
use rand::SeedableRng;
use regex::Regex;
use rpc_toolkit::command;
use serde_json::Value;

use crate::action::docker::DockerAction;
use crate::config::spec::PackagePointerSpecVariant;
use crate::context::{EitherContext, ExtendedContext};
use crate::db::model::{CurrentDependencyInfo, InstalledPackageDataEntryModel};
use crate::db::util::WithRevision;
use crate::dependencies::{BreakageRes, DependencyError, TaggedDependencyError};
use crate::net::host::Hosts;
use crate::s9pk::manifest::PackageId;
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
pub fn config(
    #[context] ctx: EitherContext,
    #[arg] id: PackageId,
) -> Result<ExtendedContext<EitherContext, PackageId>, Error> {
    Ok(ExtendedContext::from(ctx).map(|_| id))
}

#[command(display(display_serializable))]
pub async fn get(
    #[context] ctx: ExtendedContext<EitherContext, PackageId>,
    #[allow(unused_variables)]
    #[arg(long = "format")]
    format: Option<IoFormat>,
) -> Result<ConfigRes, Error> {
    let mut db = ctx.base().as_rpc().unwrap().db.handle();
    let pkg_model = crate::db::DatabaseModel::new()
        .package_data()
        .idx_model(ctx.extension())
        .and_then(|m| m.installed())
        .expect(&mut db)
        .await
        .with_kind(crate::ErrorKind::NotFound)?;
    let action = pkg_model
        .clone()
        .manifest()
        .config()
        .get(&mut db)
        .await?
        .to_owned()
        .ok_or_else(|| {
            Error::new(
                anyhow!("{} has no config", ctx.extension()),
                crate::ErrorKind::NotFound,
            )
        })?;
    let version = pkg_model.clone().manifest().version().get(&mut db).await?;
    let volumes = pkg_model.manifest().volumes().get(&mut db).await?;
    let hosts = crate::db::DatabaseModel::new()
        .network()
        .hosts()
        .get(&mut db)
        .await?;
    action
        .get(ctx.extension(), &*version, &*volumes, &*hosts)
        .await
}

#[command(subcommands(self(set_impl(async)), set_dry), display(display_none))]
pub fn set(
    #[context] ctx: ExtendedContext<EitherContext, PackageId>,
    #[allow(unused_variables)]
    #[arg(long = "format")]
    format: Option<IoFormat>,
    #[arg(long = "timeout", parse(parse_duration))] timeout: Option<Duration>,
    #[arg(stdin, parse(parse_stdin_deserializable))] config: Option<Config>,
    #[arg(rename = "expire-id", long = "expire-id")] expire_id: Option<String>,
) -> Result<
    ExtendedContext<EitherContext, (PackageId, Option<Config>, Option<Duration>, Option<String>)>,
    Error,
> {
    Ok(ctx.map(|id| (id, config, timeout, expire_id)))
}

#[command(display(display_serializable))]
pub async fn set_dry(
    #[context] ctx: ExtendedContext<
        EitherContext,
        (PackageId, Option<Config>, Option<Duration>, Option<String>),
    >,
) -> Result<BreakageRes, Error> {
    let (ctx, (id, config, timeout, _)) = ctx.split();
    let rpc_ctx = ctx.as_rpc().unwrap();
    let mut db = rpc_ctx.db.handle();
    let hosts = crate::db::DatabaseModel::new()
        .network()
        .hosts()
        .get(&mut db)
        .await?;
    let mut tx = db.begin().await?;
    let mut breakages = IndexMap::new();
    configure(
        &mut tx,
        &rpc_ctx.docker,
        &*hosts,
        &id,
        config,
        &timeout,
        true,
        &mut IndexMap::new(),
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
    Ok(BreakageRes {
        patch: tx.abort().await?,
        breakages,
    })
}

pub async fn set_impl(
    ctx: ExtendedContext<
        EitherContext,
        (PackageId, Option<Config>, Option<Duration>, Option<String>),
    >,
) -> Result<WithRevision<()>, Error> {
    let (ctx, (id, config, timeout, expire_id)) = ctx.split();
    let rpc_ctx = ctx.as_rpc().unwrap();
    let mut db = rpc_ctx.db.handle();
    let hosts = crate::db::DatabaseModel::new()
        .network()
        .hosts()
        .get(&mut db)
        .await?;
    let mut tx = db.begin().await?;
    let mut breakages = IndexMap::new();
    configure(
        &mut tx,
        &rpc_ctx.docker,
        &*hosts,
        &id,
        config,
        &timeout,
        false,
        &mut IndexMap::new(),
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

pub fn configure<'a, Db: DbHandle>(
    db: &'a mut Db,
    docker: &'a Docker,
    hosts: &'a Hosts,
    id: &'a PackageId,
    config: Option<Config>,
    timeout: &'a Option<Duration>,
    dry_run: bool,
    overrides: &'a mut IndexMap<PackageId, Config>,
    breakages: &'a mut IndexMap<PackageId, TaggedDependencyError>,
) -> BoxFuture<'a, Result<(), Error>> {
    async move {
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
            .get(db)
            .await?
            .to_owned()
            .ok_or_else(|| {
                Error::new(anyhow!("{} has no config", id), crate::ErrorKind::NotFound)
            })?;
        let version = pkg_model.clone().manifest().version().get(db).await?;
        let dependencies = pkg_model.clone().manifest().dependencies().get(db).await?;
        let volumes = pkg_model.clone().manifest().volumes().get(db).await?;

        // get current config and current spec
        let ConfigRes {
            config: old_config,
            spec,
        } = action.get(id, &*version, &*volumes, &*hosts).await?;

        // determine new config to use
        let mut config = if let Some(config) = config.or_else(|| old_config.clone()) {
            config
        } else {
            spec.gen(&mut rand::rngs::StdRng::from_entropy(), timeout)?
        };

        spec.matches(&config)?; // check that new config matches spec
        spec.update(db, &*overrides, &mut config).await?; // dereference pointers in the new config

        // create backreferences to pointers
        let mut sys = pkg_model.clone().system_pointers().get_mut(db).await?;
        sys.truncate(0);
        let mut current_dependencies: IndexMap<PackageId, CurrentDependencyInfo> = dependencies
            .0
            .iter()
            .filter_map(|(id, info)| {
                if info.optional.is_none() {
                    Some((id.clone(), CurrentDependencyInfo::default()))
                } else {
                    None
                }
            })
            .collect();
        for ptr in spec.pointers(&config)? {
            match ptr {
                ValueSpecPointer::Package(PackagePointerSpec { package_id, target }) => {
                    if let Some(current_dependency) = current_dependencies.get_mut(&package_id) {
                        current_dependency.pointers.push(target);
                    } else {
                        current_dependencies.insert(
                            package_id,
                            CurrentDependencyInfo {
                                pointers: vec![target],
                                health_checks: IndexSet::new(),
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
                .set(id, &*version, &*dependencies, &*volumes, hosts, &config)
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
            let mut deps = pkg_model.clone().current_dependencies().get_mut(db).await?;
            *deps = current_dependencies.clone();
            deps.save(db).await?;
            res.signal
        } else {
            None
        };

        // update dependencies
        for (dependency, dep_info) in current_dependencies {
            if let Some(dependency_model) = crate::db::DatabaseModel::new()
                .package_data()
                .idx_model(&dependency)
                .and_then(|pkg| pkg.installed())
                .check(db)
                .await?
            {
                dependency_model
                    .current_dependents()
                    .idx_model(id)
                    .put(db, &dep_info)
                    .await?;
            }
        }

        // cache current config for dependents
        overrides.insert(id.clone(), config.clone());

        // handle dependents
        let dependents = pkg_model.clone().current_dependents().get(db).await?;
        let prev = old_config.map(Value::Object).unwrap_or_default();
        let next = Value::Object(config.clone());
        for (dependent, dep_info) in &*dependents {
            fn handle_broken_dependents<'a, Db: DbHandle>(
                db: &'a mut Db,
                id: &'a PackageId,
                dependency: &'a PackageId,
                model: InstalledPackageDataEntryModel,
                error: DependencyError,
                breakages: &'a mut IndexMap<PackageId, TaggedDependencyError>,
            ) -> BoxFuture<'a, Result<(), Error>> {
                async move {
                    let mut status = model.clone().status().get_mut(db).await?;

                    let old = status.dependency_errors.0.remove(id);
                    let newly_broken = old.is_none();
                    status.dependency_errors.0.insert(
                        id.clone(),
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
                            if model
                                .clone()
                                .manifest()
                                .dependencies()
                                .idx_model(dependency)
                                .expect(db)
                                .await?
                                .get(db)
                                .await?
                                .critical
                            {
                                status.main.stop();
                                let dependents = model.current_dependents().get(db).await?;
                                for (dependent, _) in &*dependents {
                                    let dependent_model = crate::db::DatabaseModel::new()
                                        .package_data()
                                        .idx_model(dependent)
                                        .and_then(|pkg| pkg.installed())
                                        .expect(db)
                                        .await?;
                                    handle_broken_dependents(
                                        db,
                                        dependent,
                                        id,
                                        dependent_model,
                                        DependencyError::NotRunning,
                                        breakages,
                                    )
                                    .await?;
                                }
                            }
                        }
                    }

                    status.save(db).await?;

                    Ok(())
                }
                .boxed()
            }

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
                .get(db)
                .await?
            {
                let version = dependent_model.clone().manifest().version().get(db).await?;
                if let Err(error) = cfg.check(dependent, &*version, &config).await? {
                    let dep_err = DependencyError::ConfigUnsatisfied { error };
                    handle_broken_dependents(
                        db,
                        dependent,
                        id,
                        dependent_model,
                        dep_err,
                        breakages,
                    )
                    .await?;
                }

                // handle backreferences
                for ptr in &dep_info.pointers {
                    if let PackagePointerSpecVariant::Config { selector, multi } = ptr {
                        if selector.select(*multi, &next) != selector.select(*multi, &prev) {
                            if let Err(e) = configure(
                                db, docker, hosts, dependent, None, timeout, dry_run, overrides,
                                breakages,
                            )
                            .await
                            {
                                if e.kind == crate::ErrorKind::ConfigRulesViolation {
                                    let dependent_model = crate::db::DatabaseModel::new()
                                        .package_data()
                                        .idx_model(dependent)
                                        .and_then(|pkg| pkg.installed())
                                        .expect(db)
                                        .await?;
                                    handle_broken_dependents(
                                        db,
                                        dependent,
                                        id,
                                        dependent_model,
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
            docker
                .kill_container(
                    &DockerAction::container_name(id, &*version),
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
