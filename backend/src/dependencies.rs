use std::collections::BTreeMap;
use std::time::Duration;

use color_eyre::eyre::eyre;
use emver::VersionRange;
use futures::FutureExt;
use models::OptionExt;
use rand::SeedableRng;
use rpc_toolkit::command;
use serde::{Deserialize, Serialize};
use tracing::instrument;

use crate::config::action::ConfigRes;
use crate::config::{Config, ConfigSpec, ConfigureContext};
use crate::context::RpcContext;
use crate::db::model::{CurrentDependencies, Database, InstalledPackageInfo};
use crate::prelude::*;
use crate::procedure::docker::DockerContainers;
use crate::procedure::{NoOutput, PackageProcedure, ProcedureName};
use crate::s9pk::manifest::PackageId;
use crate::util::serde::display_serializable;
use crate::util::{display_none, Version};
use crate::volume::Volumes;
use crate::Error;

#[command(subcommands(configure))]
pub fn dependency() -> Result<(), Error> {
    Ok(())
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
    pub config: Option<DependencyConfig>,
}

#[derive(Clone, Debug, Deserialize, Serialize, HasModel)]
#[serde(rename_all = "kebab-case")]
#[model = "Model<Self>"]
pub struct DependencyConfig {
    check: PackageProcedure,
    auto_configure: PackageProcedure,
}
impl DependencyConfig {
    pub async fn check(
        &self,
        ctx: &RpcContext,
        container: &Option<DockerContainers>,
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
        container: &Option<DockerContainers>,
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
    let breakages = BTreeMap::new();
    let overrides = Default::default();
    let ConfigDryRes {
        old_config: _,
        new_config,
        spec: _,
    } = configure_logic(ctx.clone(), (pkg_id, dep_id.clone())).await?;

    let configure_context = ConfigureContext {
        breakages,
        timeout: Some(Duration::from_secs(3).into()),
        config: Some(new_config),
        dry_run: false,
        overrides,
    };
    crate::config::configure(&ctx, &dep_id, configure_context).await?;
    Ok(())
}

#[derive(Clone, Debug, Serialize, Deserialize)]
#[serde(rename_all = "kebab-case")]
pub struct ConfigDryRes {
    pub old_config: Config,
    pub new_config: Config,
    pub spec: ConfigSpec,
}

#[command(rename = "dry", display(display_serializable))]
#[instrument(skip_all)]
pub async fn configure_dry(
    #[context] ctx: RpcContext,
    #[parent_data] (pkg_id, dependency_id): (PackageId, PackageId),
) -> Result<ConfigDryRes, Error> {
    configure_logic(ctx, (pkg_id, dependency_id)).await
}

pub async fn configure_logic(
    ctx: RpcContext,
    (pkg_id, dependency_id): (PackageId, PackageId),
) -> Result<ConfigDryRes, Error> {
    let db = ctx.db.peek().await?;
    let pkg = db
        .as_package_data()
        .as_idx(&pkg_id)
        .or_not_found(&pkg_id)?
        .as_installed()
        .or_not_found(&pkg_id)?;
    let pkg_version = pkg.as_manifest().as_version().de()?;
    let pkg_volumes = pkg.as_manifest().as_volumes().de()?;
    let dependency = db
        .as_package_data()
        .as_idx(&pkg_id)
        .or_not_found(&pkg_id)?
        .as_installed()
        .or_not_found(&pkg_id)?;
    let dependency_config_action = dependency.as_manifest().as_config().de()?;
    let dependency_version = dependency.as_manifest().as_version().de()?;
    let dependency_volumes = dependency.as_manifest().as_volumes().de()?;
    let dependency = pkg
        .as_manifest()
        .as_dependencies()
        .as_idx(&dependency_id)
        .or_not_found(&dependency_id)?;
    let pkg_docker_container = pkg.as_manifest().as_containers().de()?;

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
        .as_config()
        .de()?
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

#[instrument(skip_all)]
pub fn add_dependent_to_current_dependents_lists(
    db: &mut Model<Database>,
    dependent_id: &PackageId,
    current_dependencies: &CurrentDependencies,
) -> Result<(), Error> {
    for (dependency, dep_info) in &current_dependencies.0 {
        if let Some(mut dependency_dependents) = db
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

pub async fn reconfigure_dependents_with_live_pointers(
    ctx: &RpcContext,
    i: &InstalledPackageInfo,
) -> Result<(), Error> {
    todo!();
    // let dependents = &pde.current_dependents;
    // let me = &pde.manifest.id;
    // for (dependent_id, dependency_info) in &dependents.0 {
    //     if dependency_info.pointers.iter().any(|ptr| match ptr {
    //         // dependency id matches the package being uninstalled
    //         PackagePointerSpec::TorAddress(ptr) => &ptr.package_id == me && dependent_id != me,
    //         PackagePointerSpec::LanAddress(ptr) => &ptr.package_id == me && dependent_id != me,
    //         // we never need to retarget these
    //         PackagePointerSpec::TorKey(_) => false,
    //         PackagePointerSpec::Config(_) => false,
    //     }) {
    //         let breakages = BTreeMap::new();
    //         let overrides = Default::default();

    //         let configure_context = ConfigureContext {
    //             breakages,
    //             timeout: None,
    //             config: None,
    //             dry_run: false,
    //             overrides,
    //         };
    //         crate::config::configure(&ctx, dependent_id, configure_context).await?;
    //     }
    // }
    Ok(())
}
