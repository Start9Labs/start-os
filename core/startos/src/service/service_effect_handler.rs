use std::collections::BTreeSet;
use std::ffi::OsString;
use std::net::Ipv4Addr;
use std::os::unix::process::CommandExt;
use std::path::{Path, PathBuf};
use std::str::FromStr;
use std::sync::{Arc, Weak};

use clap::builder::ValueParserFactory;
use clap::Parser;
use emver::VersionRange;
use imbl::OrdMap;
use imbl_value::{json, InternedString};
use models::{ActionId, HealthCheckId, HostId, ImageId, PackageId, VolumeId};
use patch_db::json_ptr::JsonPointer;
use rpc_toolkit::{from_fn, from_fn_async, AnyContext, Context, Empty, HandlerExt, ParentHandler};
use serde::{Deserialize, Serialize};
use tokio::process::Command;
use ts_rs::TS;
use url::Url;

use crate::db::model::package::{
    ActionMetadata, CurrentDependencies, CurrentDependencyInfo, CurrentDependencyKind,
};
use crate::disk::mount::filesystem::idmapped::IdMapped;
use crate::disk::mount::filesystem::loop_dev::LoopDev;
use crate::disk::mount::filesystem::overlayfs::OverlayGuard;
use crate::net::host::binding::BindOptions;
use crate::net::host::HostKind;
use crate::prelude::*;
use crate::s9pk::rpc::SKIP_ENV;
use crate::service::cli::ContainerCliContext;
use crate::service::ServiceActorSeed;
use crate::status::health_check::HealthCheckResult;
use crate::status::MainStatus;
use crate::util::clap::FromStrParser;
use crate::util::{new_guid, Invoke};
use crate::{echo, ARCH};

#[derive(Clone)]
pub(super) struct EffectContext(Weak<ServiceActorSeed>);
impl EffectContext {
    pub fn new(seed: Weak<ServiceActorSeed>) -> Self {
        Self(seed)
    }
}
impl Context for EffectContext {}
impl EffectContext {
    fn deref(&self) -> Result<Arc<ServiceActorSeed>, Error> {
        if let Some(seed) = Weak::upgrade(&self.0) {
            Ok(seed)
        } else {
            Err(Error::new(
                eyre!("Service has already been destroyed"),
                ErrorKind::InvalidRequest,
            ))
        }
    }
}

#[derive(Debug, Clone, serde::Serialize, serde::Deserialize)]
struct RpcData {
    id: i64,
    method: String,
    params: Value,
}
pub fn service_effect_handler() -> ParentHandler {
    ParentHandler::new()
        .subcommand("gitInfo", from_fn(crate::version::git_info))
        .subcommand(
            "echo",
            from_fn(echo).with_remote_cli::<ContainerCliContext>(),
        )
        .subcommand("chroot", from_fn(chroot).no_display())
        .subcommand("exists", from_fn_async(exists).no_cli())
        .subcommand("executeAction", from_fn_async(execute_action).no_cli())
        .subcommand("getConfigured", from_fn_async(get_configured).no_cli())
        .subcommand(
            "stopped",
            from_fn_async(stopped)
                .no_display()
                .with_remote_cli::<ContainerCliContext>(),
        )
        .subcommand(
            "running",
            from_fn_async(running)
                .no_display()
                .with_remote_cli::<ContainerCliContext>(),
        )
        .subcommand(
            "restart",
            from_fn_async(restart)
                .no_display()
                .with_remote_cli::<ContainerCliContext>(),
        )
        .subcommand(
            "shutdown",
            from_fn_async(shutdown)
                .no_display()
                .with_remote_cli::<ContainerCliContext>(),
        )
        .subcommand(
            "setConfigured",
            from_fn_async(set_configured)
                .no_display()
                .with_remote_cli::<ContainerCliContext>(),
        )
        .subcommand(
            "setMainStatus",
            from_fn_async(set_main_status).with_remote_cli::<ContainerCliContext>(),
        )
        .subcommand("setHealth", from_fn_async(set_health).no_cli())
        .subcommand("getStore", from_fn_async(get_store).no_cli())
        .subcommand("setStore", from_fn_async(set_store).no_cli())
        .subcommand(
            "exposeForDependents",
            from_fn_async(expose_for_dependents).no_cli(),
        )
        .subcommand(
            "createOverlayedImage",
            from_fn_async(create_overlayed_image)
                .with_custom_display_fn::<AnyContext, _>(|_, (path, _)| {
                    Ok(println!("{}", path.display()))
                })
                .with_remote_cli::<ContainerCliContext>(),
        )
        .subcommand(
            "destroyOverlayedImage",
            from_fn_async(destroy_overlayed_image).no_cli(),
        )
        .subcommand(
            "getSslCertificate",
            from_fn_async(get_ssl_certificate).no_cli(),
        )
        .subcommand("getSslKey", from_fn_async(get_ssl_key).no_cli())
        .subcommand(
            "getServiceInterface",
            from_fn_async(get_service_interface).no_cli(),
        )
        .subcommand("clearBindings", from_fn_async(clear_bindings).no_cli())
        .subcommand("bind", from_fn_async(bind).no_cli())
        .subcommand("getHostInfo", from_fn_async(get_host_info).no_cli())
        .subcommand(
            "setDependencies",
            from_fn_async(set_dependencies)
                .no_display()
                .with_remote_cli::<ContainerCliContext>(),
        )
        .subcommand("getSystemSmtp", from_fn_async(get_system_smtp).no_cli())
        .subcommand("getContainerIp", from_fn_async(get_container_ip).no_cli())
        .subcommand(
            "getServicePortForward",
            from_fn_async(get_service_port_forward).no_cli(),
        )
        .subcommand(
            "clearServiceInterfaces",
            from_fn_async(clear_network_interfaces).no_cli(),
        )
        .subcommand(
            "exportServiceInterface",
            from_fn_async(export_service_interface).no_cli(),
        )
        .subcommand("getPrimaryUrl", from_fn_async(get_primary_url).no_cli())
        .subcommand(
            "listServiceInterfaces",
            from_fn_async(list_service_interfaces).no_cli(),
        )
        .subcommand("removeAddress", from_fn_async(remove_address).no_cli())
        .subcommand("exportAction", from_fn_async(export_action).no_cli())
        .subcommand("removeAction", from_fn_async(remove_action).no_cli())
        .subcommand("reverseProxy", from_fn_async(reverse_proxy).no_cli())
        .subcommand("mount", from_fn_async(mount).no_cli())
    // TODO Callbacks
}

#[derive(Debug, Clone, serde::Serialize, serde::Deserialize, TS)]
#[ts(export)]
#[serde(rename_all = "camelCase")]
struct GetSystemSmtpParams {
    callback: Callback,
}
#[derive(Debug, Clone, serde::Serialize, serde::Deserialize, TS)]
#[ts(export)]
#[serde(rename_all = "camelCase")]
struct GetServicePortForwardParams {
    #[ts(type = "string | null")]
    package_id: Option<PackageId>,
    internal_port: u32,
}

#[derive(Debug, Clone, serde::Serialize, serde::Deserialize, TS)]
#[ts(export)]
#[serde(rename_all = "camelCase")]
struct AddressInfo {
    username: Option<String>,
    host_id: String,
    bind_options: BindOptions,
    suffix: String,
}

#[derive(Debug, Clone, serde::Serialize, serde::Deserialize, TS)]
#[ts(export)]
#[serde(rename_all = "camelCase")]
enum ServiceInterfaceType {
    Ui,
    P2p,
    Api,
}
#[derive(Debug, Clone, serde::Serialize, serde::Deserialize, TS)]
#[ts(export)]
#[serde(rename_all = "camelCase")]
struct ExportServiceInterfaceParams {
    id: String,
    name: String,
    description: String,
    has_primary: bool,
    disabled: bool,
    masked: bool,
    address_info: AddressInfo,
    r#type: ServiceInterfaceType,
}
#[derive(Debug, Clone, serde::Serialize, serde::Deserialize, TS)]
#[ts(export)]
#[serde(rename_all = "camelCase")]
struct GetPrimaryUrlParams {
    #[ts(type = "string | null")]
    package_id: Option<PackageId>,
    service_interface_id: String,
    callback: Callback,
}
#[derive(Debug, Clone, serde::Serialize, serde::Deserialize, TS)]
#[ts(export)]
#[serde(rename_all = "camelCase")]
struct ListServiceInterfacesParams {
    #[ts(type = "string | null")]
    package_id: Option<PackageId>,
    callback: Callback,
}
#[derive(Debug, Clone, serde::Serialize, serde::Deserialize, TS)]
#[ts(export)]
#[serde(rename_all = "camelCase")]
struct RemoveAddressParams {
    id: String,
}

#[derive(Debug, Clone, serde::Serialize, serde::Deserialize, TS)]
#[ts(export)]
#[serde(rename_all = "camelCase")]
struct ExportActionParams {
    #[ts(type = "string")]
    id: ActionId,
    metadata: ActionMetadata,
}
#[derive(Debug, Clone, serde::Serialize, serde::Deserialize, TS)]
#[ts(export)]
#[serde(rename_all = "camelCase")]
struct RemoveActionParams {
    #[ts(type = "string")]
    id: ActionId,
}
#[derive(Debug, Clone, serde::Serialize, serde::Deserialize, TS)]
#[ts(export)]
#[serde(rename_all = "camelCase")]
struct ReverseProxyBind {
    ip: Option<String>,
    port: u32,
    ssl: bool,
}
#[derive(Debug, Clone, serde::Serialize, serde::Deserialize, TS)]
#[ts(export)]
#[serde(rename_all = "camelCase")]
struct ReverseProxyDestination {
    ip: Option<String>,
    port: u32,
    ssl: bool,
}
#[derive(Debug, Clone, serde::Serialize, serde::Deserialize, TS)]
#[ts(export)]
#[serde(rename_all = "camelCase")]
struct ReverseProxyHttp {
    #[ts(type = "null | {[key: string]: string}")]
    headers: Option<OrdMap<String, String>>,
}
#[derive(Debug, Clone, serde::Serialize, serde::Deserialize, TS)]
#[ts(export)]
#[serde(rename_all = "camelCase")]
struct ReverseProxyParams {
    bind: ReverseProxyBind,
    dst: ReverseProxyDestination,
    http: ReverseProxyHttp,
}
#[derive(Debug, Clone, serde::Serialize, serde::Deserialize, TS)]
#[ts(export)]
#[serde(rename_all = "camelCase")]
struct MountTarget {
    #[ts(type = "string")]
    package_id: PackageId,
    #[ts(type = "string")]
    volume_id: VolumeId,
    subpath: Option<PathBuf>,
    readonly: bool,
}
#[derive(Debug, Clone, serde::Serialize, serde::Deserialize, TS)]
#[ts(export)]
#[serde(rename_all = "camelCase")]
struct MountParams {
    location: String,
    target: MountTarget,
}
async fn get_system_smtp(
    context: EffectContext,
    data: GetSystemSmtpParams,
) -> Result<Value, Error> {
    todo!()
}
async fn get_container_ip(context: EffectContext, _: Empty) -> Result<Ipv4Addr, Error> {
    match context.0.upgrade() {
        Some(c) => {
            let net_service = c.persistent_container.net_service.lock().await;
            Ok(net_service.get_ip())
        }
        None => Err(Error::new(
            eyre!("Upgrade on Weak<ServiceActorSeed> resulted in a None variant"),
            crate::ErrorKind::NotFound,
        )),
    }
}
async fn get_service_port_forward(
    context: EffectContext,
    data: GetServicePortForwardParams,
) -> Result<Value, Error> {
    todo!()
}
async fn clear_network_interfaces(context: EffectContext, _: Empty) -> Result<Value, Error> {
    todo!()
}
async fn export_service_interface(
    context: EffectContext,
    data: ExportServiceInterfaceParams,
) -> Result<Value, Error> {
    todo!()
}
async fn get_primary_url(
    context: EffectContext,
    data: GetPrimaryUrlParams,
) -> Result<Value, Error> {
    todo!()
}
async fn list_service_interfaces(
    context: EffectContext,
    data: ListServiceInterfacesParams,
) -> Result<Value, Error> {
    todo!()
}
async fn remove_address(context: EffectContext, data: RemoveAddressParams) -> Result<Value, Error> {
    todo!()
}
async fn export_action(context: EffectContext, data: ExportActionParams) -> Result<(), Error> {
    let context = context.deref()?;
    let package_id = context.id.clone();
    context
        .ctx
        .db
        .mutate(|db| {
            let model = db
                .as_public_mut()
                .as_package_data_mut()
                .as_idx_mut(&package_id)
                .or_not_found(&package_id)?
                .as_actions_mut();
            let mut value = model.de()?;
            value
                .insert(data.id, data.metadata)
                .map(|_| ())
                .unwrap_or_default();
            model.ser(&value)
        })
        .await?;
    Ok(())
}
async fn remove_action(context: EffectContext, data: RemoveActionParams) -> Result<(), Error> {
    let context = context.deref()?;
    let package_id = context.id.clone();
    context
        .ctx
        .db
        .mutate(|db| {
            let model = db
                .as_public_mut()
                .as_package_data_mut()
                .as_idx_mut(&package_id)
                .or_not_found(&package_id)?
                .as_actions_mut();
            let mut value = model.de()?;
            value.remove(&data.id).map(|_| ()).unwrap_or_default();
            model.ser(&value)
        })
        .await?;
    Ok(())
}
async fn reverse_proxy(context: EffectContext, data: ReverseProxyParams) -> Result<Value, Error> {
    todo!()
}
async fn mount(context: EffectContext, data: MountParams) -> Result<Value, Error> {
    todo!()
}

#[derive(Debug, Clone, serde::Serialize, serde::Deserialize, TS)]
#[ts(export)]
struct Callback(#[ts(type = "() => void")] i64);

#[derive(Debug, Clone, serde::Serialize, serde::Deserialize, TS)]
#[serde(rename_all = "camelCase")]
#[ts(export)]
enum GetHostInfoParamsKind {
    Multi,
}
#[derive(Debug, Clone, serde::Serialize, serde::Deserialize, TS)]
#[serde(rename_all = "camelCase")]
#[ts(export)]
struct GetHostInfoParams {
    kind: Option<GetHostInfoParamsKind>,
    service_interface_id: String,
    #[ts(type = "string | null")]
    package_id: Option<PackageId>,
    callback: Callback,
}
async fn get_host_info(
    _: AnyContext,
    GetHostInfoParams { .. }: GetHostInfoParams,
) -> Result<Value, Error> {
    todo!()
}

async fn clear_bindings(context: EffectContext, _: Empty) -> Result<Value, Error> {
    todo!()
}

#[derive(Debug, Clone, serde::Serialize, serde::Deserialize, TS)]
#[serde(rename_all = "camelCase")]
#[ts(export)]
struct BindParams {
    kind: HostKind,
    id: HostId,
    internal_port: u16,
    #[serde(flatten)]
    options: BindOptions,
}
async fn bind(_: AnyContext, BindParams { .. }: BindParams) -> Result<Value, Error> {
    todo!()
}

#[derive(Debug, Clone, serde::Serialize, serde::Deserialize, TS)]
#[serde(rename_all = "camelCase")]
#[ts(export)]
struct GetServiceInterfaceParams {
    #[ts(type = "string | null")]
    package_id: Option<PackageId>,
    service_interface_id: String,
    callback: Callback,
}
async fn get_service_interface(
    _: AnyContext,
    GetServiceInterfaceParams {
        callback,
        package_id,
        service_interface_id,
    }: GetServiceInterfaceParams,
) -> Result<Value, Error> {
    // TODO @Dr_Bonez
    Ok(json!({
        "id": service_interface_id,
        "name": service_interface_id,
        "description": "This is a fake",
        "hasPrimary": true,
        "disabled": false,
        "masked": false,
        "addressInfo": json!({
            "username": Value::Null,
            "hostId": "HostId?",
            "options": json!({
                "scheme": Value::Null,
                "preferredExternalPort": 80,
                "addSsl":Value::Null,
                "secure": false,
                "ssl": false
            }),
            "suffix": "http"
        }),
        "type": "api"
    }))
}

#[derive(Debug, Clone, serde::Serialize, serde::Deserialize, Parser, TS)]
#[serde(rename_all = "camelCase")]
#[ts(export)]
struct ChrootParams {
    #[arg(short = 'e', long = "env")]
    env: Option<PathBuf>,
    #[arg(short = 'w', long = "workdir")]
    workdir: Option<PathBuf>,
    #[arg(short = 'u', long = "user")]
    user: Option<String>,
    path: PathBuf,
    #[ts(type = "string")]
    command: OsString,
    #[ts(type = "string[]")]
    args: Vec<OsString>,
}
fn chroot(
    _: AnyContext,
    ChrootParams {
        env,
        workdir,
        user,
        path,
        command,
        args,
    }: ChrootParams,
) -> Result<(), Error> {
    let mut cmd = std::process::Command::new(command);
    if let Some(env) = env {
        for (k, v) in std::fs::read_to_string(env)?
            .lines()
            .map(|l| l.trim())
            .filter_map(|l| l.split_once("="))
            .filter(|(k, _)| !SKIP_ENV.contains(&k))
        {
            cmd.env(k, v);
        }
    }
    std::os::unix::fs::chroot(path)?;
    if let Some(uid) = user.as_deref().and_then(|u| u.parse::<u32>().ok()) {
        cmd.uid(uid);
    } else if let Some(user) = user {
        let (uid, gid) = std::fs::read_to_string("/etc/passwd")?
            .lines()
            .find_map(|l| {
                let mut split = l.trim().split(":");
                if user != split.next()? {
                    return None;
                }
                split.next(); // throw away x
                Some((split.next()?.parse().ok()?, split.next()?.parse().ok()?))
                // uid gid
            })
            .or_not_found(lazy_format!("{user} in /etc/passwd"))?;
        cmd.uid(uid);
        cmd.gid(gid);
    };
    if let Some(workdir) = workdir {
        cmd.current_dir(workdir);
    }
    cmd.args(args);
    Err(cmd.exec().into())
}

#[derive(Debug, Clone, serde::Serialize, serde::Deserialize, TS)]
#[serde(rename_all = "camelCase")]
#[ts(export)]
enum Algorithm {
    Ecdsa,
    Ed25519,
}

#[derive(Debug, Clone, serde::Serialize, serde::Deserialize, TS)]
#[serde(rename_all = "camelCase")]
#[ts(export)]
struct GetSslCertificateParams {
    package_id: Option<String>,
    host_id: String,
    algorithm: Option<Algorithm>, //"ecdsa" | "ed25519"
}

async fn get_ssl_certificate(
    context: EffectContext,
    GetSslCertificateParams {
        package_id,
        algorithm,
        host_id,
    }: GetSslCertificateParams,
) -> Result<Value, Error> {
    let fake = include_str!("./fake.cert.pem");
    Ok(json!([fake, fake, fake]))
}

#[derive(Debug, Clone, serde::Serialize, serde::Deserialize, TS)]
#[serde(rename_all = "camelCase")]
#[ts(export)]
struct GetSslKeyParams {
    package_id: Option<String>,
    host_id: String,
    algorithm: Option<Algorithm>,
}

async fn get_ssl_key(
    context: EffectContext,
    GetSslKeyParams {
        package_id,
        host_id,
        algorithm,
    }: GetSslKeyParams,
) -> Result<Value, Error> {
    let fake = include_str!("./fake.cert.key");
    Ok(json!(fake))
}
#[derive(Debug, Clone, serde::Serialize, serde::Deserialize, TS)]
#[serde(rename_all = "camelCase")]
#[ts(export)]
struct GetStoreParams {
    #[ts(type = "string | null")]
    package_id: Option<PackageId>,
    #[ts(type = "string")]
    path: JsonPointer,
}

async fn get_store(
    context: EffectContext,
    GetStoreParams { package_id, path }: GetStoreParams,
) -> Result<Value, Error> {
    let context = context.deref()?;
    let peeked = context.ctx.db.peek().await;
    let package_id = package_id.unwrap_or(context.id.clone());
    let value = peeked
        .as_private()
        .as_package_stores()
        .as_idx(&package_id)
        .or_not_found(&package_id)?
        .de()?;

    Ok(path
        .get(&value)
        .ok_or_else(|| Error::new(eyre!("Did not find value at path"), ErrorKind::NotFound))?
        .clone())
}
#[derive(Debug, Clone, serde::Serialize, serde::Deserialize, TS)]
#[serde(rename_all = "camelCase")]
#[ts(export)]
struct SetStoreParams {
    #[ts(type = "any")]
    value: Value,
    #[ts(type = "string")]
    path: JsonPointer,
}

async fn set_store(
    context: EffectContext,
    SetStoreParams { value, path }: SetStoreParams,
) -> Result<(), Error> {
    let context = context.deref()?;
    let package_id = context.id.clone();
    context
        .ctx
        .db
        .mutate(|db| {
            let model = db
                .as_private_mut()
                .as_package_stores_mut()
                .upsert(&package_id, || Box::new(json!({})))?;
            let mut model_value = model.de()?;
            if model_value.is_null() {
                model_value = json!({});
            }
            path.set(&mut model_value, value, true)
                .with_kind(ErrorKind::ParseDbField)?;
            model.ser(&model_value)
        })
        .await?;
    Ok(())
}

#[derive(Debug, Clone, serde::Serialize, serde::Deserialize, TS)]
#[serde(rename_all = "camelCase")]
#[ts(export)]
struct ExposeForDependentsParams {
    #[ts(type = "string[]")]
    paths: Vec<JsonPointer>,
}

async fn expose_for_dependents(
    context: EffectContext,
    ExposeForDependentsParams { paths }: ExposeForDependentsParams,
) -> Result<(), Error> {
    Ok(())
}

#[derive(Debug, Clone, serde::Serialize, serde::Deserialize, Parser, TS)]
#[ts(export)]
#[serde(rename_all = "camelCase")]
struct ParamsPackageId {
    #[ts(type = "string")]
    package_id: PackageId,
}
#[derive(Debug, Clone, serde::Serialize, serde::Deserialize, Parser, TS)]
#[serde(rename_all = "camelCase")]
#[command(rename_all = "camelCase")]
#[ts(export)]
struct ParamsMaybePackageId {
    #[ts(type = "string | null")]
    package_id: Option<PackageId>,
}

async fn exists(context: EffectContext, params: ParamsPackageId) -> Result<Value, Error> {
    let context = context.deref()?;
    let peeked = context.ctx.db.peek().await;
    let package = peeked
        .as_public()
        .as_package_data()
        .as_idx(&params.package_id)
        .is_some();
    Ok(json!(package))
}

#[derive(Debug, Clone, serde::Serialize, serde::Deserialize, TS)]
#[serde(rename_all = "camelCase")]
#[ts(export)]
struct ExecuteAction {
    #[ts(type = "string | null")]
    service_id: Option<PackageId>,
    #[ts(type = "string")]
    action_id: ActionId,
    #[ts(type = "any")]
    input: Value,
}
async fn execute_action(
    context: EffectContext,
    ExecuteAction {
        action_id,
        input,
        service_id,
    }: ExecuteAction,
) -> Result<Value, Error> {
    let context = context.deref()?;
    let package_id = service_id.clone().unwrap_or_else(|| context.id.clone());
    let service = context.ctx.services.get(&package_id).await;
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
async fn get_configured(context: EffectContext, _: Empty) -> Result<Value, Error> {
    let context = context.deref()?;
    let peeked = context.ctx.db.peek().await;
    let package_id = &context.id;
    let package = peeked
        .as_public()
        .as_package_data()
        .as_idx(package_id)
        .or_not_found(package_id)?
        .as_status()
        .as_configured()
        .de()?;
    Ok(json!(package))
}

async fn stopped(context: EffectContext, params: ParamsMaybePackageId) -> Result<Value, Error> {
    let context = context.deref()?;
    let peeked = context.ctx.db.peek().await;
    let package_id = params.package_id.unwrap_or_else(|| context.id.clone());
    let package = peeked
        .as_public()
        .as_package_data()
        .as_idx(&package_id)
        .or_not_found(&package_id)?
        .as_status()
        .as_main()
        .de()?;
    Ok(json!(matches!(package, MainStatus::Stopped)))
}
async fn running(context: EffectContext, params: ParamsPackageId) -> Result<Value, Error> {
    dbg!("Starting the running {params:?}");
    let context = context.deref()?;
    let peeked = context.ctx.db.peek().await;
    let package_id = params.package_id;
    let package = peeked
        .as_public()
        .as_package_data()
        .as_idx(&package_id)
        .or_not_found(&package_id)?
        .as_status()
        .as_main()
        .de()?;
    Ok(json!(matches!(package, MainStatus::Running { .. })))
}

async fn restart(context: EffectContext, _: Empty) -> Result<Value, Error> {
    let context = context.deref()?;
    let service = context.ctx.services.get(&context.id).await;
    let service = service.as_ref().ok_or_else(|| {
        Error::new(
            eyre!("Could not find package {}", context.id),
            ErrorKind::Unknown,
        )
    })?;
    service.restart().await?;
    Ok(json!(()))
}

async fn shutdown(context: EffectContext, _: Empty) -> Result<Value, Error> {
    let context = context.deref()?;
    let service = context.ctx.services.get(&context.id).await;
    let service = service.as_ref().ok_or_else(|| {
        Error::new(
            eyre!("Could not find package {}", context.id),
            ErrorKind::Unknown,
        )
    })?;
    service.stop().await?;
    Ok(json!(()))
}

#[derive(Debug, Clone, serde::Serialize, serde::Deserialize, Parser, TS)]
#[serde(rename_all = "camelCase")]
#[command(rename_all = "camelCase")]
#[ts(export)]
struct SetConfigured {
    configured: bool,
}
async fn set_configured(context: EffectContext, params: SetConfigured) -> Result<Value, Error> {
    let context = context.deref()?;
    let package_id = &context.id;
    context
        .ctx
        .db
        .mutate(|db| {
            db.as_public_mut()
                .as_package_data_mut()
                .as_idx_mut(package_id)
                .or_not_found(package_id)?
                .as_status_mut()
                .as_configured_mut()
                .ser(&params.configured)
        })
        .await?;
    Ok(json!(()))
}

#[derive(Debug, Clone, serde::Serialize, serde::Deserialize, TS)]
#[serde(rename_all = "camelCase")]
#[ts(export)]
enum Status {
    Running,
    Stopped,
}
impl FromStr for Status {
    type Err = color_eyre::eyre::Report;
    fn from_str(s: &str) -> Result<Self, Self::Err> {
        match s {
            "running" => Ok(Self::Running),
            "stopped" => Ok(Self::Stopped),
            _ => Err(eyre!("unknown status {s}")),
        }
    }
}
impl ValueParserFactory for Status {
    type Parser = FromStrParser<Self>;
    fn value_parser() -> Self::Parser {
        FromStrParser::new()
    }
}

#[derive(Debug, Clone, serde::Serialize, serde::Deserialize, Parser, TS)]
#[serde(rename_all = "camelCase")]
#[command(rename_all = "camelCase")]
#[ts(export)]
struct SetMainStatus {
    status: Status,
}
async fn set_main_status(context: EffectContext, params: SetMainStatus) -> Result<Value, Error> {
    dbg!(format!("Status for main will be is {params:?}"));
    let context = context.deref()?;
    match params.status {
        Status::Running => context.started(),
        Status::Stopped => context.stopped(),
    }
    Ok(Value::Null)
}

#[derive(Debug, Clone, serde::Serialize, serde::Deserialize, TS)]
#[serde(rename_all = "camelCase")]
#[ts(export)]
struct SetHealth {
    id: HealthCheckId,
    #[serde(flatten)]
    result: HealthCheckResult,
}

async fn set_health(
    context: EffectContext,
    SetHealth { id, result }: SetHealth,
) -> Result<Value, Error> {
    let context = context.deref()?;

    let package_id = &context.id;
    context
        .ctx
        .db
        .mutate(move |db| {
            db.as_public_mut()
                .as_package_data_mut()
                .as_idx_mut(package_id)
                .or_not_found(package_id)?
                .as_status_mut()
                .as_main_mut()
                .mutate(|main| {
                    match main {
                        &mut MainStatus::Running { ref mut health, .. }
                        | &mut MainStatus::BackingUp { ref mut health, .. } => {
                            health.insert(id, result);
                        }
                        _ => (),
                    }
                    Ok(())
                })
        })
        .await?;
    Ok(json!(()))
}
#[derive(serde::Deserialize, serde::Serialize, Parser, TS)]
#[serde(rename_all = "camelCase")]
#[command(rename_all = "camelCase")]
#[ts(export)]
pub struct DestroyOverlayedImageParams {
    #[ts(type = "string")]
    guid: InternedString,
}

#[instrument(skip_all)]
pub async fn destroy_overlayed_image(
    ctx: EffectContext,
    DestroyOverlayedImageParams { guid }: DestroyOverlayedImageParams,
) -> Result<(), Error> {
    let ctx = ctx.deref()?;
    if ctx
        .persistent_container
        .overlays
        .lock()
        .await
        .remove(&guid)
        .is_none()
    {
        tracing::warn!("Could not find a guard to remove on the destroy overlayed image; assumming that it already is removed and will be skipping");
    }
    Ok(())
}
#[derive(serde::Deserialize, serde::Serialize, Parser, TS)]
#[serde(rename_all = "camelCase")]
#[command(rename_all = "camelCase")]
#[ts(export)]
pub struct CreateOverlayedImageParams {
    #[ts(type = "string")]
    image_id: ImageId,
}

#[instrument(skip_all)]
pub async fn create_overlayed_image(
    ctx: EffectContext,
    CreateOverlayedImageParams { image_id }: CreateOverlayedImageParams,
) -> Result<(PathBuf, InternedString), Error> {
    let ctx = ctx.deref()?;
    let path = Path::new("images")
        .join(*ARCH)
        .join(&image_id)
        .with_extension("squashfs");
    if let Some(image) = ctx
        .persistent_container
        .s9pk
        .as_archive()
        .contents()
        .get_path(dbg!(&path))
        .and_then(|e| e.as_file())
    {
        let guid = new_guid();
        let rootfs_dir = ctx
            .persistent_container
            .lxc_container
            .get()
            .ok_or_else(|| {
                Error::new(
                    eyre!("PersistentContainer has been destroyed"),
                    ErrorKind::Incoherent,
                )
            })?
            .rootfs_dir();
        let mountpoint = rootfs_dir.join("media/startos/overlays").join(&*guid);
        tokio::fs::create_dir_all(&mountpoint).await?;
        Command::new("chown")
            .arg("100000:100000")
            .arg(&mountpoint)
            .invoke(ErrorKind::Filesystem)
            .await?;
        let container_mountpoint = Path::new("/").join(
            mountpoint
                .strip_prefix(rootfs_dir)
                .with_kind(ErrorKind::Incoherent)?,
        );
        tracing::info!("Mounting overlay {guid} for {image_id}");
        let guard = OverlayGuard::mount(
            &IdMapped::new(LoopDev::from(&**image), 0, 100000, 65536),
            mountpoint,
        )
        .await?;
        tracing::info!("Mounted overlay {guid} for {image_id}");
        ctx.persistent_container
            .overlays
            .lock()
            .await
            .insert(guid.clone(), guard);
        Ok((container_mountpoint, guid))
    } else {
        Err(Error::new(
            eyre!("image {image_id} not found in s9pk"),
            ErrorKind::NotFound,
        ))
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Deserialize, Serialize, TS)]
#[serde(rename_all = "camelCase")]
#[ts(export)]
enum DependencyKind {
    Exists,
    Running,
}

#[derive(Debug, Clone, Deserialize, Serialize, TS)]
#[serde(rename_all = "camelCase", tag = "kind")]
#[ts(export)]
enum DependencyRequirement {
    #[serde(rename_all = "camelCase")]
    Running {
        #[ts(type = "string")]
        id: PackageId,
        #[ts(type = "string[]")]
        health_checks: BTreeSet<HealthCheckId>,
        #[ts(type = "string")]
        version_spec: VersionRange,
        #[ts(type = "string")]
        registry_url: Url,
    },
    #[serde(rename_all = "camelCase")]
    Exists {
        #[ts(type = "string")]
        id: PackageId,
        #[ts(type = "string")]
        version_spec: VersionRange,
        #[ts(type = "string")]
        registry_url: Url,
    },
}
// filebrowser:exists,bitcoind:running:foo+bar+baz
impl FromStr for DependencyRequirement {
    type Err = Error;
    fn from_str(s: &str) -> Result<Self, Self::Err> {
        match s.split_once(':') {
            Some((id, "e")) | Some((id, "exists")) => Ok(Self::Exists {
                id: id.parse()?,
                registry_url: "".parse()?,  // TODO
                version_spec: "*".parse()?, // TODO
            }),
            Some((id, rest)) => {
                let health_checks = match rest.split_once(':') {
                    Some(("r", rest)) | Some(("running", rest)) => rest
                        .split('+')
                        .map(|id| id.parse().map_err(Error::from))
                        .collect(),
                    Some((kind, _)) => Err(Error::new(
                        eyre!("unknown dependency kind {kind}"),
                        ErrorKind::InvalidRequest,
                    )),
                    None => match rest {
                        "r" | "running" => Ok(BTreeSet::new()),
                        kind => Err(Error::new(
                            eyre!("unknown dependency kind {kind}"),
                            ErrorKind::InvalidRequest,
                        )),
                    },
                }?;
                Ok(Self::Running {
                    id: id.parse()?,
                    health_checks,
                    registry_url: "".parse()?,  // TODO
                    version_spec: "*".parse()?, // TODO
                })
            }
            None => Ok(Self::Running {
                id: s.parse()?,
                health_checks: BTreeSet::new(),
                registry_url: "".parse()?,  // TODO
                version_spec: "*".parse()?, // TODO
            }),
        }
    }
}
impl ValueParserFactory for DependencyRequirement {
    type Parser = FromStrParser<Self>;
    fn value_parser() -> Self::Parser {
        FromStrParser::new()
    }
}

#[derive(Deserialize, Serialize, Parser, TS)]
#[serde(rename_all = "camelCase")]
#[command(rename_all = "camelCase")]
#[ts(export)]
struct SetDependenciesParams {
    dependencies: Vec<DependencyRequirement>,
}

async fn set_dependencies(
    ctx: EffectContext,
    SetDependenciesParams { dependencies }: SetDependenciesParams,
) -> Result<(), Error> {
    let ctx = ctx.deref()?;
    let id = &ctx.id;
    ctx.ctx
        .db
        .mutate(|db| {
            let dependencies = CurrentDependencies(
                dependencies
                    .into_iter()
                    .map(|dependency| match dependency {
                        DependencyRequirement::Exists {
                            id,
                            registry_url,
                            version_spec,
                        } => (
                            id,
                            CurrentDependencyInfo {
                                kind: CurrentDependencyKind::Exists,
                                registry_url,
                                version_spec,
                                icon: todo!(),
                                title: todo!(),
                            },
                        ),
                        DependencyRequirement::Running {
                            id,
                            health_checks,
                            registry_url,
                            version_spec,
                        } => (
                            id,
                            CurrentDependencyInfo {
                                kind: CurrentDependencyKind::Running { health_checks },
                                registry_url,
                                version_spec,
                                icon: todo!(),
                                title: todo!(),
                            },
                        ),
                    })
                    .collect(),
            );
            db.as_public_mut()
                .as_package_data_mut()
                .as_idx_mut(id)
                .or_not_found(id)?
                .as_current_dependencies_mut()
                .ser(&dependencies)
        })
        .await
}
