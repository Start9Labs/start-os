use std::collections::BTreeMap;
use std::time::Duration;

use clap::Parser;
use models::PackageId;
use patch_db::json_patch::merge;
use rpc_toolkit::{from_fn_async, Empty, HandlerExt, ParentHandler};
use serde::{Deserialize, Serialize};
use tracing::instrument;
use ts_rs::TS;

use crate::config::{Config, ConfigSpec, ConfigureContext};
use crate::context::RpcContext;
use crate::db::model::package::CurrentDependencies;
use crate::prelude::*;
use crate::Error;

pub fn dependency() -> ParentHandler {
    ParentHandler::new().subcommand("configure", configure())
}

#[derive(Clone, Debug, Default, Deserialize, Serialize, HasModel, TS)]
#[model = "Model<Self>"]
#[ts(export)]
pub struct Dependencies(pub BTreeMap<PackageId, DepInfo>);
impl Map for Dependencies {
    type Key = PackageId;
    type Value = DepInfo;
    fn key_str(key: &Self::Key) -> Result<impl AsRef<str>, Error> {
        Ok(key)
    }
    fn key_string(key: &Self::Key) -> Result<imbl_value::InternedString, Error> {
        Ok(key.clone().into())
    }
}

#[derive(Clone, Debug, Deserialize, Serialize, HasModel, TS)]
#[serde(rename_all = "camelCase")]
#[model = "Model<Self>"]
#[ts(export)]
pub struct DepInfo {
    pub description: Option<String>,
    pub optional: bool,
}

#[derive(Deserialize, Serialize, Parser, TS)]
#[serde(rename_all = "camelCase")]
#[command(rename_all = "kebab-case")]
pub struct ConfigureParams {
    dependent_id: PackageId,
    dependency_id: PackageId,
}
pub fn configure() -> ParentHandler<ConfigureParams> {
    ParentHandler::new().root_handler(
        from_fn_async(configure_impl)
            .with_inherited(|params, _| params)
            .no_cli(),
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
    let ConfigDryRes {
        old_config: _,
        new_config,
        spec: _,
    } = configure_logic(ctx.clone(), (dependent_id, dependency_id.clone())).await?;

    let configure_context = ConfigureContext {
        timeout: Some(Duration::from_secs(3).into()),
        config: Some(new_config),
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
#[serde(rename_all = "camelCase")]
pub struct ConfigDryRes {
    pub old_config: Config,
    pub new_config: Config,
    pub spec: ConfigSpec,
}

pub async fn configure_logic(
    ctx: RpcContext,
    (dependent_id, dependency_id): (PackageId, PackageId),
) -> Result<ConfigDryRes, Error> {
    let dependency_guard = ctx.services.get(&dependency_id).await;
    let dependency = dependency_guard.as_ref().or_not_found(&dependency_id)?;
    let dependent_guard = ctx.services.get(&dependent_id).await;
    let dependent = dependent_guard.as_ref().or_not_found(&dependent_id)?;
    let config_res = dependency.get_config().await?;
    let diff = Value::Object(
        dependent
            .dependency_config(dependency_id, config_res.config.clone())
            .await?
            .unwrap_or_default(),
    );
    let mut new_config = Value::Object(config_res.config.clone().unwrap_or_default());
    merge(&mut new_config, &diff);
    Ok(ConfigDryRes {
        old_config: config_res.config.unwrap_or_default(),
        new_config: new_config.as_object().cloned().unwrap_or_default(),
        spec: config_res.spec,
    })
}

#[instrument(skip_all)]
pub async fn compute_dependency_config_errs(
    ctx: &RpcContext,
    id: &PackageId,
    current_dependencies: &mut CurrentDependencies,
) -> Result<(), Error> {
    let service_guard = ctx.services.get(id).await;
    let service = service_guard.as_ref().or_not_found(id)?;
    for (dep_id, dep_info) in current_dependencies.0.iter_mut() {
        // check if config passes dependency check
        let Some(dependency) = &*ctx.services.get(dep_id).await else {
            continue;
        };

        let dep_config = dependency.get_config().await?.config;

        dep_info.config_satisfied = service
            .dependency_config(dep_id.clone(), dep_config)
            .await?
            .is_none();
    }
    Ok(())
}
