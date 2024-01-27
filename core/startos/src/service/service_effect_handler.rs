use std::path::{Path, PathBuf};
use std::sync::Arc;

use imbl_value::{json, InternedString};
use models::{ActionId, HealthCheckId, ImageId, PackageId};
use patch_db::json_ptr::JsonPointer;
use rpc_toolkit::{from_fn_async, Context, Empty, HandlerExt, ParentHandler};

use crate::db::model::ExposedUI;
use crate::disk::mount::filesystem::loop_dev::LoopDev;
use crate::disk::mount::filesystem::overlayfs::OverlayGuard;
use crate::prelude::*;
use crate::service::ServiceActorSeed;
use crate::status::health_check::HealthCheckResult;
use crate::status::MainStatus;
use crate::util::new_guid;
use crate::ARCH;

#[derive(Clone)]
pub(super) struct EffectContext(Arc<ServiceActorSeed>);
impl EffectContext {
    pub fn new(seed: Arc<ServiceActorSeed>) -> Self {
        Self(seed)
    }
}
impl Context for EffectContext {}
impl std::ops::Deref for EffectContext {
    type Target = ServiceActorSeed;
    fn deref(&self) -> &Self::Target {
        self.0.deref()
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
        .subcommand("exists", from_fn_async(exists).no_cli())
        .subcommand("executeAction", from_fn_async(execute_action).no_cli())
        .subcommand("getConfigured", from_fn_async(get_configured).no_cli())
        .subcommand("stopped", from_fn_async(stopped).no_cli())
        .subcommand("running", from_fn_async(running).no_cli())
        .subcommand("restart", from_fn_async(restart).no_cli())
        .subcommand("shutdown", from_fn_async(shutdown).no_cli())
        .subcommand("setConfigured", from_fn_async(set_configured).no_cli())
        .subcommand("setHealth", from_fn_async(set_health).no_cli())
        .subcommand("getStore", from_fn_async(get_store).no_cli())
        .subcommand("setStore", from_fn_async(set_store).no_cli())
        .subcommand(
            "exposeForDependents",
            from_fn_async(expose_for_dependents).no_cli(),
        )
        .subcommand("exposeUi", from_fn_async(expose_ui).no_cli())
        .subcommand(
            "createOverlayedImage",
            from_fn_async(create_overlayed_image).no_cli(),
        )
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
    // TODO Callbacks
}
#[derive(Debug, Clone, serde::Serialize, serde::Deserialize)]
#[serde(rename_all = "camelCase")]
struct GetStoreParams {
    package_id: Option<PackageId>,
    path: JsonPointer,
}

async fn get_store(
    context: EffectContext,
    GetStoreParams { package_id, path }: GetStoreParams,
) -> Result<Value, Error> {
    let peeked = context.ctx.db.peek().await;
    let package_id = package_id.unwrap_or(context.id.clone());
    let value = peeked
        .as_package_data()
        .as_idx(&package_id)
        .or_not_found(&package_id)?
        .as_installed()
        .or_not_found(&package_id)?
        .as_store()
        .de()?;

    Ok(path
        .get(&value)
        .ok_or_else(|| Error::new(eyre!("Did not find value at path"), ErrorKind::NotFound))?
        .clone())
}
#[derive(Debug, Clone, serde::Serialize, serde::Deserialize)]
#[serde(rename_all = "camelCase")]
struct SetStoreParams {
    value: Value,
    path: JsonPointer,
}

async fn set_store(
    context: EffectContext,
    SetStoreParams { value, path }: SetStoreParams,
) -> Result<(), Error> {
    let package_id = context.id.clone();
    context
        .ctx
        .db
        .mutate(|db| {
            let model = db
                .as_package_data_mut()
                .as_idx_mut(&package_id)
                .or_not_found(&package_id)?
                .as_installed_mut()
                .or_not_found(&package_id)?
                .as_store_mut();
            let mut model_value = model.de()?;
            path.set(&mut model_value, value, true)
                .with_kind(ErrorKind::ParseDbField)?;
            model.ser(&model_value)
        })
        .await?;
    Ok(())
}

#[derive(Debug, Clone, serde::Serialize, serde::Deserialize)]
#[serde(rename_all = "camelCase")]
struct ExposeForDependentsParams {
    paths: Vec<JsonPointer>,
}

async fn expose_for_dependents(
    context: EffectContext,
    ExposeForDependentsParams { paths }: ExposeForDependentsParams,
) -> Result<(), Error> {
    let package_id = context.id.clone();
    context
        .ctx
        .db
        .mutate(|db| {
            db.as_package_data_mut()
                .as_idx_mut(&package_id)
                .or_not_found(&package_id)?
                .as_installed_mut()
                .or_not_found(&package_id)?
                .as_store_exposed_dependents_mut()
                .ser(&paths)
        })
        .await?;
    Ok(())
}
#[derive(Debug, Clone, serde::Serialize, serde::Deserialize)]
#[serde(rename_all = "camelCase")]
struct ExposeUiParams {
    paths: Vec<ExposedUI>,
}

async fn expose_ui(
    context: EffectContext,
    ExposeUiParams { paths }: ExposeUiParams,
) -> Result<(), Error> {
    let package_id = context.id.clone();
    context
        .ctx
        .db
        .mutate(|db| {
            db.as_package_data_mut()
                .as_idx_mut(&package_id)
                .or_not_found(&package_id)?
                .as_installed_mut()
                .or_not_found(&package_id)?
                .as_store_exposed_ui_mut()
                .ser(&paths)
        })
        .await?;
    Ok(())
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
    context: EffectContext,
    ExecuteAction {
        action_id,
        input,
        service_id,
    }: ExecuteAction,
) -> Result<Value, Error> {
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
    let peeked = context.ctx.db.peek().await;
    let package_id = &context.id;
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

async fn stopped(context: EffectContext, params: ParamsMaybePackageId) -> Result<Value, Error> {
    let peeked = context.ctx.db.peek().await;
    let package_id = params.package_id.unwrap_or_else(|| context.id.clone());
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
async fn running(context: EffectContext, params: ParamsMaybePackageId) -> Result<Value, Error> {
    let peeked = context.ctx.db.peek().await;
    let package_id = params.package_id.unwrap_or_else(|| context.id.clone());
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

async fn restart(context: EffectContext, _: Empty) -> Result<Value, Error> {
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
#[derive(Debug, Clone, serde::Serialize, serde::Deserialize)]
#[serde(rename_all = "camelCase")]
struct SetConfigured {
    configured: bool,
}
async fn set_configured(context: EffectContext, params: SetConfigured) -> Result<Value, Error> {
    let package_id = &context.id;
    context
        .ctx
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
}

async fn set_health(context: EffectContext, params: SetHealth) -> Result<Value, Error> {
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

    let package_id = &context.id;
    context
        .ctx
        .db
        .mutate(move |db| {
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

#[derive(serde::Deserialize, serde::Serialize)]
#[serde(rename_all = "camelCase")]
pub struct CreateOverlayedImageParams {
    image_id: ImageId,
}

#[instrument(skip_all)]
pub async fn create_overlayed_image(
    ctx: EffectContext,
    CreateOverlayedImageParams { image_id }: CreateOverlayedImageParams,
) -> Result<PathBuf, Error> {
    let path = Path::new("images")
        .join(&*ARCH)
        .join(&image_id)
        .with_extension("squashfs");
    if let Some(image) = ctx
        .persistent_container
        .s9pk
        .as_archive()
        .contents()
        .get_path(&path)
        .and_then(|e| e.as_file())
    {
        let guid = new_guid();
        let mountpoint = ctx
            .persistent_container
            .lxc_container
            .rootfs_dir()
            .join("media/images/overlays")
            .join(&*guid);
        let container_mountpoint = Path::new("/").join(
            mountpoint
                .strip_prefix(ctx.persistent_container.lxc_container.rootfs_dir())
                .with_kind(ErrorKind::Incoherent)?,
        );
        let guard = OverlayGuard::mount(&LoopDev::from(&**image), mountpoint).await?;
        ctx.persistent_container
            .overlays
            .lock()
            .await
            .insert(guid.clone(), guard);
        Ok(container_mountpoint)
    } else {
        Err(Error::new(
            eyre!("image {image_id} not found in s9pk"),
            ErrorKind::NotFound,
        ))
    }
}