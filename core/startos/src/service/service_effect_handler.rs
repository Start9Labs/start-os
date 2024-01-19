use imbl_value::json;
use models::{ActionId, HealthCheckId, PackageId};
use rpc_toolkit::{from_fn_async, HandlerExt, ParentHandler};

use crate::prelude::*;
use crate::status::MainStatus;
use crate::{context::RpcContext, status::health_check::HealthCheckResult};

#[derive(Debug, Clone, serde::Serialize, serde::Deserialize)]
struct RpcData {
    id: i64,
    method: String,
    params: Value,
}
pub fn service_effect_handler(package_id: PackageId) -> ParentHandler {
    // TODO @dr-bonez @Blu-J Need to convert to have the use of the package_id in the routes, instead of from_service
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
    // TODO @DrBonez when we get the new api for 4.0?
    // .subcommand("setDependencies",from_fn(set_dependencies))
    // .subcommand("getStore",from_fn(get_store))
    // .subcommand("setStore",from_fn(set_store))
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
    // .subcommand("exposeForDependents",from_fn(expose_for_dependents))
    // .subcommand("exposeUi",from_fn(expose_ui))
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
    from_service: PackageId,
    package_id: Option<PackageId>,
}

async fn exists(context: RpcContext, params: ParamsPackageId) -> Result<Value, Error> {
    let peeked = context.db.peek().await;
    let package = peeked.as_package_data().as_idx(&params.package).is_some();
    Ok(json!(package))
}

#[derive(Debug, Clone, serde::Serialize, serde::Deserialize)]
#[serde(rename_all = "camelCase")]
struct ExecuteAction {
    from_service: PackageId,
    service_id: Option<PackageId>,
    action_id: ActionId,
    input: Value,
}
async fn execute_action(_: RpcContext, _: ExecuteAction) -> Result<Value, Error> {
    // let package_id = params
    //     .service_id
    //     .clone()
    //     .unwrap_or_else(|| params.from_service.clone());
    // let action_id = params.action_id.clone();
    // let input = from_value(params.input).ok();
    // let action_result = action(
    //     context,
    //     ActionParams {
    //         package_id,
    //         action_id,
    //         input: crate::util::serde::StdinDeserializable(input),
    //     },
    // )
    // .await?;
    todo!("@BluJ")
    // imbl_value::to_value(&action_result).with_kind(ErrorKind::Serialization)
}
#[derive(Debug, Clone, serde::Serialize, serde::Deserialize)]
#[serde(rename_all = "camelCase")]
struct FromService {
    from_service: PackageId,
}
async fn get_configured(
    context: RpcContext,
    FromService {
        from_service: package_id,
    }: FromService,
) -> Result<Value, Error> {
    let peeked = context.db.peek().await;
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

async fn stopped(context: RpcContext, params: ParamsMaybePackageId) -> Result<Value, Error> {
    let peeked = context.db.peek().await;
    let package_id = params.package_id.unwrap_or(params.from_service);
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
async fn running(context: RpcContext, params: ParamsMaybePackageId) -> Result<Value, Error> {
    let peeked = context.db.peek().await;
    let package_id = params.package_id.unwrap_or(params.from_service);
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
    context: RpcContext,
    FromService {
        from_service: package_id,
    }: FromService,
) -> Result<Value, Error> {
    let manager = context.services.get(&package_id).await.ok_or_else(|| {
        Error::new(
            eyre!("Could not find package {package_id}"),
            ErrorKind::Unknown,
        )
    })?;
    manager.restart().await;
    Ok(json!(()))
}

async fn shutdown(
    context: RpcContext,
    FromService {
        from_service: package_id,
    }: FromService,
) -> Result<Value, Error> {
    let manager = context.services.get(&package_id).await.ok_or_else(|| {
        Error::new(
            eyre!("Could not find package {package_id}"),
            ErrorKind::Unknown,
        )
    })?;
    manager.stop().await;
    Ok(json!(()))
}
#[derive(Debug, Clone, serde::Serialize, serde::Deserialize)]
#[serde(rename_all = "camelCase")]
struct SetConfigured {
    configured: bool,
    from_service: PackageId,
}
async fn set_configured(context: RpcContext, params: SetConfigured) -> Result<Value, Error> {
    let package_id = &params.from_service;
    context
        .db
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
    from_service: PackageId,
}

async fn set_health(context: RpcContext, params: SetHealth) -> Result<Value, Error> {
    // TODO DrBonez + BLUJ Need to change the type from
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
    context
        .db
        .mutate(move |db| {
            let package_id = &params.from_service;
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
                        from_service: _,
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
