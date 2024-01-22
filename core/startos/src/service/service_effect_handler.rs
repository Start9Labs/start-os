use imbl_value::json;
use models::{ActionId, HealthCheckId, PackageId};
use rpc_toolkit::{from_fn_async, Context, Empty, HandlerExt, ParentHandler};

use crate::context::RpcContext;
use crate::prelude::*;
use crate::status::health_check::HealthCheckResult;
use crate::status::MainStatus;

#[derive(Clone)]
pub struct EffectContext {
    ctx: RpcContext,
    package_id: PackageId,
}

impl EffectContext {
    pub fn new(ctx: RpcContext, package_id: PackageId) -> Self {
        Self { ctx, package_id }
    }
}

impl Context for EffectContext {}
#[derive(Debug, Clone, serde::Serialize, serde::Deserialize)]
struct RpcData {
    id: i64,
    method: String,
    params: Value,
}
pub fn service_effect_handler() -> ParentHandler {
    ParentHandler::new()
        .subcommand("exists", from_fn_async(exists).no_cli())
        .subcommand("executeAction", from_fn_async(execute_action).no_cli())
        .subcommand("getConfigured", from_fn_async(get_configured).no_cli())
        .subcommand("stopped", from_fn_async(stopped).no_cli())
        .subcommand("running", from_fn_async(running).no_cli())
        .subcommand("restart", from_fn_async(restart).no_cli())
        .subcommand("shutdown", from_fn_async(shutdown).no_cli())
        .subcommand("setConfigured", from_fn_async(set_configured).no_cli())
        .subcommand("setHealth", from_fn_async(set_health).no_cli())
    // .subcommand("getStore",from_fn(get_store))
    // .subcommand("setStore",from_fn(set_store))?
    // .subcommand("exposeForDependents",from_fn(expose_for_dependents))
    // .subcommand("exposeUi",from_fn(expose_ui))
    // TODO @DrBonez when we get the new api for 4.0
    // .subcommand("setDependencies",from_fn(set_dependencies))
    // .subcommand("embassyGetInterface",from_fn(embassy_get_interface))
    // .subcommand("mount",from_fn(mount))
    // .subcommand("getSslCertificate",from_fn(get_ssl_certificate))
    // .subcommand("getSslKey",from_fn(get_ssl_key))
    // .subcommand("removeAction",from_fn(remove_action))
    // .subcommand("removeAddress",from_fn(remove_address))
    // .subcommand("exportAction",from_fn(export_action))
    // .subcommand("bind",from_fn(bind))
    // .subcommand("clearNetworkInterfaces",from_fn(clear_network_interfaces))
    // .subcommand("exportNetworkInterface",from_fn(export_network_interface))
    // .subcommand("clearBindings",from_fn(clear_bindings))
    // .subcommand("getHostnames",from_fn(get_hostnames))
    // .subcommand("getInterface",from_fn(get_interface))
    // .subcommand("listInterface",from_fn(list_interface))
    // .subcommand("getIPHostname",from_fn(get_ip_hostname))
    // .subcommand("getContainerIp",from_fn(get_container_ip))
    // .subcommand("getLocalHostname",from_fn(get_local_hostname))
    // .subcommand("getPrimaryUrl",from_fn(get_primary_url))
    // .subcommand("getServicePortForward",from_fn(get_service_port_forward))
    // .subcommand("getServiceTorHostname",from_fn(get_service_tor_hostname))
    // .subcommand("getSystemSmtp",from_fn(get_system_smtp))
    // .subcommand("reverseProxy",from_fn(reverse_pro)xy)
}

#[derive(Debug, Clone, serde::Serialize, serde::Deserialize)]
struct ParamsPackageId {
    package: PackageId,
}
#[derive(Debug, Clone, serde::Serialize, serde::Deserialize)]
#[serde(rename_all = "camelCase")]
struct ParamsMaybePackageId {
    package_id: Option<PackageId>,
}

async fn exists(context: EffectContext, params: ParamsPackageId) -> Result<Value, Error> {
    let peeked = context.ctx.db.peek().await;
    let package = peeked.as_package_data().as_idx(&params.package).is_some();
    Ok(json!(package))
}

#[derive(Debug, Clone, serde::Serialize, serde::Deserialize)]
#[serde(rename_all = "camelCase")]
struct ExecuteAction {
    service_id: Option<PackageId>,
    action_id: ActionId,
    input: Value,
}
async fn execute_action(
    EffectContext { ctx, package_id }: EffectContext,
    ExecuteAction {
        action_id,
        input,
        service_id,
    }: ExecuteAction,
) -> Result<Value, Error> {
    let package_id = service_id.clone().unwrap_or_else(|| package_id.clone());
    let service = ctx.services.get(&package_id).await;
    let service = service.as_ref().ok_or_else(|| {
        Error::new(
            eyre!("Could not find package {package_id}"),
            ErrorKind::Unknown,
        )
    })?;

    Ok(json!(service.action(action_id, input).await?))
}
#[derive(Debug, Clone, serde::Serialize, serde::Deserialize)]
#[serde(rename_all = "camelCase")]
struct FromService {}
async fn get_configured(
    EffectContext { ctx, package_id }: EffectContext,
    _: Empty,
) -> Result<Value, Error> {
    let peeked = ctx.db.peek().await;
    let package = peeked
        .as_package_data()
        .as_idx(&package_id)
        .or_not_found(&package_id)?
        .as_installed()
        .or_not_found(&package_id)?
        .as_status()
        .as_configured()
        .de()?;
    Ok(json!(package))
}

async fn stopped(
    EffectContext { ctx, package_id }: EffectContext,
    params: ParamsMaybePackageId,
) -> Result<Value, Error> {
    let peeked = ctx.db.peek().await;
    let package_id = params.package_id.unwrap_or(package_id);
    let package = peeked
        .as_package_data()
        .as_idx(&package_id)
        .or_not_found(&package_id)?
        .as_installed()
        .or_not_found(&package_id)?
        .as_status()
        .as_main()
        .de()?;
    Ok(json!(matches!(package, MainStatus::Stopped)))
}
async fn running(
    EffectContext { ctx, package_id }: EffectContext,
    params: ParamsMaybePackageId,
) -> Result<Value, Error> {
    let peeked = ctx.db.peek().await;
    let package_id = params.package_id.unwrap_or(package_id);
    let package = peeked
        .as_package_data()
        .as_idx(&package_id)
        .or_not_found(&package_id)?
        .as_installed()
        .or_not_found(&package_id)?
        .as_status()
        .as_main()
        .de()?;
    Ok(json!(matches!(package, MainStatus::Running { .. })))
}

async fn restart(
    EffectContext { ctx, package_id }: EffectContext,
    _: Empty,
) -> Result<Value, Error> {
    let service = ctx.services.get(&package_id).await;
    let service = service.as_ref().ok_or_else(|| {
        Error::new(
            eyre!("Could not find package {package_id}"),
            ErrorKind::Unknown,
        )
    })?;
    service.restart().await;
    Ok(json!(()))
}

async fn shutdown(
    EffectContext { ctx, package_id }: EffectContext,
    _: Empty,
) -> Result<Value, Error> {
    let service = ctx.services.get(&package_id).await;
    let service = service.as_ref().ok_or_else(|| {
        Error::new(
            eyre!("Could not find package {package_id}"),
            ErrorKind::Unknown,
        )
    })?;
    service.stop().await;
    Ok(json!(()))
}
#[derive(Debug, Clone, serde::Serialize, serde::Deserialize)]
#[serde(rename_all = "camelCase")]
struct SetConfigured {
    configured: bool,
}
async fn set_configured(
    EffectContext { ctx, package_id }: EffectContext,
    params: SetConfigured,
) -> Result<Value, Error> {
    let package_id = &package_id;
    ctx.db
        .mutate(|db| {
            db.as_package_data_mut()
                .as_idx_mut(package_id)
                .or_not_found(package_id)?
                .as_installed_mut()
                .or_not_found(package_id)?
                .as_status_mut()
                .as_configured_mut()
                .ser(&params.configured)
        })
        .await?;
    Ok(json!(()))
}
#[derive(Debug, Clone, serde::Serialize, serde::Deserialize)]
#[serde(rename_all = "camelCase")]
struct SetHealth {
    name: HealthCheckId,
    health_result: Option<HealthCheckResult>,
}

async fn set_health(
    EffectContext { ctx, package_id }: EffectContext,
    params: SetHealth,
) -> Result<Value, Error> {
    // TODO DrBonez + BLU-J Need to change the type from
    // ```rs
    // #[serde(tag = "result")]
    // pub enum HealthCheckResult {
    //     Success,
    //     Disabled,
    //     Starting,
    //     Loading { message: String },
    //     Failure { error: String },
    // }
    // ```
    // to
    // ```ts
    // setHealth(o: {
    //     name: string
    //     status: HealthStatus
    //     message?: string
    //   }): Promise<void>
    // ```
    ctx.db
        .mutate(move |db| {
            let package_id = &package_id;
            let mut main = db
                .as_package_data()
                .as_idx(package_id)
                .or_not_found(package_id)?
                .as_installed()
                .or_not_found(package_id)?
                .as_status()
                .as_main()
                .de()?;
            match &mut main {
                &mut MainStatus::Running { ref mut health, .. }
                | &mut MainStatus::BackingUp { ref mut health, .. } => {
                    health.remove(&params.name);
                    if let SetHealth {
                        name,
                        health_result: Some(health_result),
                    } = params
                    {
                        health.insert(name, health_result);
                    }
                }
                _ => return Ok(()),
            };
            db.as_package_data_mut()
                .as_idx_mut(package_id)
                .or_not_found(package_id)?
                .as_installed_mut()
                .or_not_found(package_id)?
                .as_status_mut()
                .as_main_mut()
                .ser(&main)
        })
        .await?;
    Ok(json!(()))
}
