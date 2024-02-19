use std::collections::BTreeMap;
use std::time::Duration;

use clap::Parser;
use emver::VersionRange;
use models::{OptionExt, PackageId};
use rpc_toolkit::{command, from_fn_async, Empty, HandlerExt, ParentHandler};
use serde::{Deserialize, Serialize};
use tracing::instrument;

use crate::config::spec::PackagePointerSpec;
use crate::config::{Config, ConfigSpec, ConfigureContext};
use crate::context::{CliContext, RpcContext};
use crate::db::model::{CurrentDependencies, Database};
use crate::prelude::*;
use crate::s9pk::manifest::Manifest;
use crate::status::DependencyConfigErrors;
use crate::util::serde::HandlerExtSerde;
use crate::util::Version;
use crate::Error;

pub fn dependency() -> ParentHandler {
    ParentHandler::new().subcommand("configure", configure())
}

#[derive(Clone, Debug, Default, Deserialize, Serialize, HasModel)]
#[model = "Model<Self>"]
pub struct Dependencies(pub BTreeMap<PackageId, DepInfo>);
impl Map for Dependencies {
    type Key = PackageId;
    type Value = DepInfo;
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
#[model = "Model<Self>"]
pub struct DepInfo {
    pub version: VersionRange,
    pub requirement: DependencyRequirement,
    pub description: Option<String>,
    #[serde(default)]
    pub config: Option<Value>, // TODO: remove
}

#[derive(Deserialize, Serialize, Parser)]
#[serde(rename_all = "kebab-case")]
#[command(rename_all = "kebab-case")]
pub struct ConfigureParams {
    #[arg(name = "dependent-id")]
    dependent_id: PackageId,
    #[arg(name = "dependency-id")]
    dependency_id: PackageId,
}
pub fn configure() -> ParentHandler<ConfigureParams> {
    ParentHandler::new()
        .root_handler(
            from_fn_async(configure_impl)
                .with_inherited(|params, _| params)
                .no_cli(),
        )
        .subcommand(
            "dry",
            from_fn_async(configure_dry)
                .with_inherited(|params, _| params)
                .with_display_serializable()
                .with_remote_cli::<CliContext>(),
        )
}

pub async fn configure_impl(
    ctx: RpcContext,
    _: Empty,
    ConfigureParams {
        dependent_id,
        dependency_id,
    }: ConfigureParams,
) -> Result<(), Error> {
    let breakages = BTreeMap::new();
    let overrides = Default::default();
    let ConfigDryRes {
        old_config: _,
        new_config,
        spec: _,
    } = configure_logic(ctx.clone(), (dependent_id, dependency_id.clone())).await?;

    let configure_context = ConfigureContext {
        breakages,
        timeout: Some(Duration::from_secs(3).into()),
        config: Some(new_config),
        dry_run: false,
        overrides,
    };
    ctx.services
        .get(&dependency_id)
        .await
        .as_ref()
        .ok_or_else(|| {
            Error::new(
                eyre!("There is no manager running for {dependency_id}"),
                ErrorKind::Unknown,
            )
        })?
        .configure(configure_context)
        .await?;
    Ok(())
}

#[derive(Clone, Debug, Serialize, Deserialize)]
#[serde(rename_all = "kebab-case")]
pub struct ConfigDryRes {
    pub old_config: Config,
    pub new_config: Config,
    pub spec: ConfigSpec,
}

// #[command(rename = "dry", display(display_serializable))]
#[instrument(skip_all)]
pub async fn configure_dry(
    ctx: RpcContext,
    _: Empty,
    ConfigureParams {
        dependent_id,
        dependency_id,
    }: ConfigureParams,
) -> Result<ConfigDryRes, Error> {
    configure_logic(ctx, (dependent_id, dependency_id)).await
}

pub async fn configure_logic(
    ctx: RpcContext,
    (dependent_id, dependency_id): (PackageId, PackageId),
) -> Result<ConfigDryRes, Error> {
    // let db = ctx.db.peek().await;
    // let pkg = db
    //     .as_package_data()
    //     .as_idx(&pkg_id)
    //     .or_not_found(&pkg_id)?
    //     .as_installed()
    //     .or_not_found(&pkg_id)?;
    // let pkg_version = pkg.as_manifest().as_version().de()?;
    // let pkg_volumes = pkg.as_manifest().as_volumes().de()?;
    // let dependency = db
    //     .as_package_data()
    //     .as_idx(&dependency_id)
    //     .or_not_found(&dependency_id)?
    //     .as_installed()
    //     .or_not_found(&dependency_id)?;
    // let dependency_config_action = dependency
    //     .as_manifest()
    //     .as_config()
    //     .de()?
    //     .ok_or_else(|| not_found!("Manifest Config"))?;
    // let dependency_version = dependency.as_manifest().as_version().de()?;
    // let dependency_volumes = dependency.as_manifest().as_volumes().de()?;
    // let dependency = pkg
    //     .as_manifest()
    //     .as_dependencies()
    //     .as_idx(&dependency_id)
    //     .or_not_found(&dependency_id)?;

    // let ConfigRes {
    //     config: maybe_config,
    //     spec,
    // } = dependency_config_action
    //     .get(
    //         &ctx,
    //         &dependency_id,
    //         &dependency_version,
    //         &dependency_volumes,
    //     )
    //     .await?;

    // let old_config = if let Some(config) = maybe_config {
    //     config
    // } else {
    //     spec.gen(
    //         &mut rand::rngs::StdRng::from_entropy(),
    //         &Some(Duration::new(10, 0)),
    //     )?
    // };

    // let new_config = dependency
    //     .as_config()
    //     .de()?
    //     .ok_or_else(|| not_found!("Config"))?
    //     .auto_configure
    //     .sandboxed(
    //         &ctx,
    //         &pkg_id,
    //         &pkg_version,
    //         &pkg_volumes,
    //         Some(&old_config),
    //         None,
    //         ProcedureName::AutoConfig(dependency_id.clone()),
    //     )
    //     .await?
    //     .map_err(|e| Error::new(eyre!("{}", e.1), crate::ErrorKind::AutoConfigure))?;

    // Ok(ConfigDryRes {
    //     old_config,
    //     new_config,
    //     spec,
    // })
    todo!()
}

#[instrument(skip_all)]
pub fn add_dependent_to_current_dependents_lists(
    db: &mut Model<Database>,
    dependent_id: &PackageId,
    current_dependencies: &CurrentDependencies,
) -> Result<(), Error> {
    for (dependency, dep_info) in &current_dependencies.0 {
        if let Some(dependency_dependents) = db
            .as_public_mut()
            .as_package_data_mut()
            .as_idx_mut(dependency)
            .and_then(|pde| pde.as_installed_mut())
            .map(|i| i.as_current_dependents_mut())
        {
            dependency_dependents.insert(dependent_id, dep_info)?;
        }
    }
    Ok(())
}

pub fn set_dependents_with_live_pointers_to_needs_config(
    db: &mut Peeked,
    id: &PackageId,
) -> Result<Vec<(PackageId, Version)>, Error> {
    let mut res = Vec::new();
    for (dep, info) in db
        .as_public_mut()
        .as_package_data()
        .as_idx(id)
        .or_not_found(id)?
        .as_installed()
        .or_not_found(id)?
        .as_current_dependents()
        .de()?
        .0
    {
        if info.pointers.iter().any(|ptr| match ptr {
            // dependency id matches the package being uninstalled
            PackagePointerSpec::TorAddress(ptr) => &ptr.package_id == id && &dep != id,
            PackagePointerSpec::LanAddress(ptr) => &ptr.package_id == id && &dep != id,
            // we never need to retarget these
            PackagePointerSpec::TorKey(_) => false,
            PackagePointerSpec::Config(_) => false,
        }) {
            let installed = db
                .as_public_mut()
                .as_package_data_mut()
                .as_idx_mut(&dep)
                .or_not_found(&dep)?
                .as_installed_mut()
                .or_not_found(&dep)?;
            let version = installed.as_manifest().as_version().de()?;
            let configured = installed.as_status_mut().as_configured_mut();
            if configured.de()? {
                configured.ser(&false)?;
                res.push((dep, version));
            }
        }
    }
    Ok(res)
}

#[instrument(skip_all)]
pub async fn compute_dependency_config_errs(
    ctx: &RpcContext,
    db: &Peeked,
    manifest: &Manifest,
    current_dependencies: &CurrentDependencies,
    dependency_config: &BTreeMap<PackageId, Config>,
) -> Result<DependencyConfigErrors, Error> {
    let mut dependency_config_errs = BTreeMap::new();
    for (dependency, _dep_info) in current_dependencies
        .0
        .iter()
        .filter(|(dep_id, _)| dep_id != &&manifest.id)
    {
        // check if config passes dependency check
        if let Some(cfg) = &manifest
            .dependencies
            .0
            .get(dependency)
            .or_not_found(dependency)?
            .config
        {
            let error = todo!();
            {
                dependency_config_errs.insert(dependency.clone(), error);
            }
        }
    }
    Ok(DependencyConfigErrors(dependency_config_errs))
}
