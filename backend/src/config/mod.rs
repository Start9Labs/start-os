use std::collections::BTreeMap;
use std::time::Duration;

use color_eyre::eyre::eyre;
use models::ErrorKind;
use patch_db::value::InOMap;
use rpc_toolkit::command;
use serde_json::Value;
use tracing::instrument;

use crate::context::RpcContext;
use crate::dependencies::{BreakageRes, TaggedDependencyError};
use crate::prelude::*;
use crate::s9pk::manifest::PackageId;
use crate::util::display_none;
use crate::util::serde::{display_serializable, parse_stdin_deserializable, IoFormat};

pub mod action;
pub mod hook;
pub mod util;

use self::action::ConfigRes;

pub type Input = InOMap<String, Value>;
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
    let manifest = ctx
        .db
        .peek()
        .await?
        .into_package_data()
        .into_idx(&id)
        .or_not_found(&id)?
        .expect_into_installed()?
        .into_manifest();

    manifest
        .as_config()
        .transpose_ref()
        .or_not_found("no config")?
        .clone()
        .de()?
        .get(
            &ctx,
            &id,
            &manifest.as_version().clone().de()?,
            &manifest.as_volumes().clone().de()?,
        )
        .await
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
    #[arg(stdin, parse(parse_stdin_deserializable))] config: Option<Value>,
) -> Result<(PackageId, Option<Value>, Option<Duration>), Error> {
    Ok((id, config, timeout.map(|d| *d)))
}

#[command(rename = "dry", display(display_serializable))]
#[instrument(skip_all)]
pub async fn set_dry(
    #[context] ctx: RpcContext,
    #[parent_data] (id, config, timeout): (PackageId, Option<Value>, Option<Duration>),
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
    pub config: Option<Value>,
    pub overrides: BTreeMap<PackageId, Value>,
    pub dry_run: bool,
}

#[instrument(skip_all)]
pub async fn set_impl(
    ctx: RpcContext,
    (id, config, timeout): (PackageId, Option<Value>, Option<Duration>),
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
) -> Result<BTreeMap<PackageId, TaggedDependencyError>, Error> {
    ctx.managers
        .get(&id)
        .await
        .ok_or_else(|| {
            Error::new(
                eyre!("There is no manager running for {id:?}"),
                ErrorKind::Unknown,
            )
        })?
        .configure(configure_context)
        .await
}

/// We want to have a double check that the paths are what we expect them to be.
/// Found that earlier the paths where not what we expected them to be.
#[tokio::test]
async fn ensure_creation_of_config_paths_makes_sense() {
    todo!();
    // let mut fake = patch_db::test_utils::NoOpDb();
    let config_locks = todo!("BLUJ"); //ConfigReceipts::new(&mut fake).await.unwrap();
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
