use std::collections::BTreeMap;
use std::path::PathBuf;
use std::time::Duration;

use color_eyre::eyre::eyre;
use indexmap::IndexSet;
use itertools::Itertools;
use models::{ErrorKind, OptionExt};
use regex::Regex;
use rpc_toolkit::command;
use serde_json::Value;
use tracing::instrument;

use crate::context::RpcContext;
use crate::prelude::*;
use crate::s9pk::manifest::PackageId;
use crate::util::display_none;
use crate::util::serde::{display_serializable, parse_stdin_deserializable, IoFormat};
use crate::Error;

pub mod action;
pub mod spec;
pub mod util;

pub use spec::{ConfigSpec, Defaultable};
use util::NumRange;

use self::action::ConfigRes;
use self::spec::ValueSpecPointer;

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

#[command(display(display_serializable))]
#[instrument(skip_all)]
pub async fn get(
    #[context] ctx: RpcContext,
    #[parent_data] id: PackageId,
    #[allow(unused_variables)]
    #[arg(long = "format")]
    format: Option<IoFormat>,
) -> Result<ConfigRes, Error> {
    let db = ctx.db.peek().await?;
    let manifest = db
        .as_package_data()
        .as_idx(&id)
        .or_not_found(&id)?
        .as_installed()
        .or_not_found(&id)?
        .as_manifest();
    let action = manifest
        .as_config()
        .de()?
        .ok_or_else(|| Error::new(eyre!("{} has no config", id), crate::ErrorKind::NotFound))?;

    let volumes = manifest.as_volumes().de()?;
    let version = manifest.as_version().de()?;
    action.get(&ctx, &id, &version, &volumes).await
}

#[command(
    subcommands(self(set_impl(async, context(RpcContext))), set_dry),
    display(display_none),
    metadata(sync_db = true)
)]
#[instrument(skip_all)]
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

#[command(rename = "dry", display(display_serializable))]
#[instrument(skip_all)]
pub async fn set_dry(
    #[context] ctx: RpcContext,
    #[parent_data] (id, config, timeout): (PackageId, Option<Config>, Option<Duration>),
) -> Result<BTreeMap<PackageId, String>, Error> {
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

    Ok(breakages)
}

pub struct ConfigureContext {
    pub breakages: BTreeMap<PackageId, String>,
    pub timeout: Option<Duration>,
    pub config: Option<Config>,
    pub overrides: BTreeMap<PackageId, Config>,
    pub dry_run: bool,
}

#[instrument(skip_all)]
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

#[instrument(skip_all)]
pub async fn configure(
    ctx: &RpcContext,
    id: &PackageId,
    configure_context: ConfigureContext,
) -> Result<BTreeMap<PackageId, String>, Error> {
    let db = ctx.db.peek().await?;
    let package = db
        .as_package_data()
        .as_idx(id)
        .or_not_found(&id)?
        .as_installed()
        .or_not_found(&id)?;
    let version = package.as_manifest().as_version().de()?;
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

macro_rules! not_found {
    ($x:expr) => {
        crate::Error::new(
            color_eyre::eyre::eyre!("Could not find {} at {}:{}", $x, module_path!(), line!()),
            crate::ErrorKind::Incoherent,
        )
    };
}
pub(crate) use not_found;

/// We want to have a double check that the paths are what we expect them to be.
/// Found that earlier the paths where not what we expected them to be.
#[tokio::test]
async fn ensure_creation_of_config_paths_makes_sense() {
    // TODO @dr-bonez
    // let mut fake = patch_db::test_utils::NoOpDb();
    // let config_locks = ConfigReceipts::new(&mut fake).await.unwrap();
    // assert_eq!(
    //     &format!("{}", config_locks.configured.lock.glob),
    //     "/package-data/*/installed/status/configured"
    // );
    // assert_eq!(
    //     &format!("{}", config_locks.config_actions.lock.glob),
    //     "/package-data/*/installed/manifest/config"
    // );
    // assert_eq!(
    //     &format!("{}", config_locks.dependencies.lock.glob),
    //     "/package-data/*/installed/manifest/dependencies"
    // );
    // assert_eq!(
    //     &format!("{}", config_locks.volumes.lock.glob),
    //     "/package-data/*/installed/manifest/volumes"
    // );
    // assert_eq!(
    //     &format!("{}", config_locks.version.lock.glob),
    //     "/package-data/*/installed/manifest/version"
    // );
    // assert_eq!(
    //     &format!("{}", config_locks.volumes.lock.glob),
    //     "/package-data/*/installed/manifest/volumes"
    // );
    // assert_eq!(
    //     &format!("{}", config_locks.manifest.lock.glob),
    //     "/package-data/*/installed/manifest"
    // );
    // assert_eq!(
    //     &format!("{}", config_locks.manifest.lock.glob),
    //     "/package-data/*/installed/manifest"
    // );
    // assert_eq!(
    //     &format!("{}", config_locks.system_pointers.lock.glob),
    //     "/package-data/*/installed/system-pointers"
    // );
    // assert_eq!(
    //     &format!("{}", config_locks.current_dependents.lock.glob),
    //     "/package-data/*/installed/current-dependents"
    // );
    // assert_eq!(
    //     &format!("{}", config_locks.dependency_errors.lock.glob),
    //     "/package-data/*/installed/status/dependency-errors"
    // );
    // assert_eq!(
    //     &format!("{}", config_locks.manifest_dependencies_config.lock.glob),
    //     "/package-data/*/installed/manifest/dependencies/*/config"
    // );
    // assert_eq!(
    //     &format!("{}", config_locks.system_pointers.lock.glob),
    //     "/package-data/*/installed/system-pointers"
    // );
}
