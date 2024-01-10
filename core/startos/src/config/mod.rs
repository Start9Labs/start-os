use std::collections::BTreeMap;
use std::path::PathBuf;
use std::sync::Arc;
use std::time::Duration;

use clap::{CommandFactory, FromArgMatches, Parser};
use color_eyre::eyre::eyre;
use indexmap::IndexSet;
use itertools::Itertools;
use models::{ErrorKind, OptionExt, PackageId, ProcedureName};
use patch_db::value::InternedString;
use patch_db::Value;
use regex::Regex;
use rpc_toolkit::{from_fn_async, Empty, HandlerExt, ParentHandler};
use serde::{Deserialize, Serialize};
use tracing::instrument;

use crate::context::{CliContext, RpcContext};
use crate::prelude::*;
use crate::util::display_none;
use crate::util::serde::{display_serializable, HandlerExtSerde, StdinDeserializable};
use crate::Error;

pub mod action;
pub mod spec;
pub mod util;

pub use spec::{ConfigSpec, Defaultable};
use util::NumRange;

use self::action::ConfigRes;
use self::spec::ValueSpecPointer;

pub type Config = patch_db::value::InOMap<InternedString, Value>;
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
    pub path: Vec<InternedString>,
    pub error: MatchError,
}
impl NoMatchWithPath {
    pub fn new(error: MatchError) -> Self {
        NoMatchWithPath {
            path: Vec::new(),
            error,
        }
    }
    pub fn prepend(mut self, seg: InternedString) -> Self {
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
    Pattern(Arc<String>, Regex),
    #[error("String {0:?} Is Not In Enum {1:?}")]
    Enum(Arc<String>, IndexSet<String>),
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
    Union(Arc<String>, IndexSet<String>),
    #[error("Variant Is Missing Tag {0:?}")]
    MissingTag(InternedString),
    #[error("Property {0:?} Of Variant {1:?} Conflicts With Union Tag")]
    PropertyMatchesUnionTag(InternedString, String),
    #[error("Name of Property {0:?} Conflicts With Map Tag Name")]
    PropertyNameMatchesMapTag(String),
    #[error("Pointer Is Invalid: {0}")]
    InvalidPointer(spec::ValueSpecPointer),
    #[error("Object Key Is Invalid: {0}")]
    InvalidKey(String),
    #[error("Value In List Is Not Unique")]
    ListUniquenessViolation,
}

#[derive(Deserialize, Serialize, Parser)]
#[serde(rename_all = "kebab-case")]
#[command(rename_all = "kebab-case")]
pub struct ConfigParams {
    pub id: PackageId,
}

// #[command(subcommands(get, set))]
pub fn config() -> ParentHandler<ConfigParams> {
    ParentHandler::new()
        .subcommand(
            "get",
            from_fn_async(get)
                .with_inherited(|ConfigParams { id }, _| id)
                .with_display_serializable()
                .with_remote_cli::<CliContext>(),
        )
        .subcommand("set", set().with_inherited(|ConfigParams { id }, _| id))
}

#[instrument(skip_all)]
pub async fn get(ctx: RpcContext, _: Empty, id: PackageId) -> Result<ConfigRes, Error> {
    let db = ctx.db.peek().await;
    let manifest = db
        .as_package_data()
        .as_idx(&id)
        .or_not_found(&id)?
        .as_installed()
        .or_not_found(&id)?
        .as_manifest();
    let version = manifest.as_version().de()?;

    ctx.services
        .get(&id)
        .await
        .or_not_found(lazy_format!("Manager for {id}"))?
        .execute(
            ProcedureName::GetConfig,
            Value::Null,
            Some(Duration::from_secs(30)),
        )
        .await?
        .map_err(|e| Error::new(eyre!("{}", e.1), ErrorKind::ConfigGen))
}

#[derive(Deserialize, Serialize, Parser)]
#[serde(rename_all = "kebab-case")]
pub struct SetParams {
    #[arg(long = "timeout")]
    pub timeout: Option<crate::util::serde::Duration>,
    #[command(flatten)]
    pub config: StdinDeserializable<Option<Config>>,
}

// TODO Dr Why isn't this used?
// #[command(
//     subcommands(self(set_impl(async, context(RpcContext))), set_dry),
//     display(display_none),
//     metadata(sync_db = true)
// )]
#[instrument(skip_all)]
pub fn set() -> ParentHandler<SetParams, PackageId> {
    ParentHandler::new()
        .root_handler(
            from_fn_async(set_impl)
                .with_metadata("sync_db", Value::Bool(true))
                .with_inherited(|set_params, id| (id, set_params))
                .no_display()
                .with_remote_cli::<CliContext>(),
        )
        .subcommand(
            "dry",
            from_fn_async(set_dry)
                .with_inherited(|set_params, id| (id, set_params))
                .with_display_serializable()
                .with_remote_cli::<CliContext>(),
        )
}

pub async fn set_dry(
    ctx: RpcContext,
    _: Empty,
    (
        id,
        SetParams {
            timeout,
            config: StdinDeserializable(config),
        },
    ): (PackageId, SetParams),
) -> Result<BTreeMap<PackageId, String>, Error> {
    let breakages = BTreeMap::new();
    let overrides = Default::default();

    let configure_context = ConfigureContext {
        breakages,
        timeout: timeout.map(|t| *t),
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
    _: Empty,
    (
        id,
        SetParams {
            timeout,
            config: StdinDeserializable(config),
        },
    ): (PackageId, SetParams),
) -> Result<(), Error> {
    let breakages = BTreeMap::new();
    let overrides = Default::default();

    let configure_context = ConfigureContext {
        breakages,
        timeout: timeout.map(|t| *t),
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
    let db = ctx.db.peek().await;
    let package = db
        .as_package_data()
        .as_idx(id)
        .or_not_found(&id)?
        .as_installed()
        .or_not_found(&id)?;
    let version = package.as_manifest().as_version().de()?;
    ctx.services
        .get(&id)
        .await
        .ok_or_else(|| {
            Error::new(
                eyre!("There is no manager running for {id}"),
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
