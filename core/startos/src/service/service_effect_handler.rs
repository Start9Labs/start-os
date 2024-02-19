use std::ffi::OsString;
use std::os::unix::process::CommandExt;
use std::path::{Path, PathBuf};
use std::str::FromStr;
use std::sync::{Arc, Weak};

use clap::builder::{TypedValueParser, ValueParserFactory};
use clap::Parser;
use imbl_value::json;
use models::{ActionId, HealthCheckId, ImageId, PackageId};
use patch_db::json_ptr::JsonPointer;
use rpc_toolkit::{from_fn, from_fn_async, AnyContext, Context, Empty, HandlerExt, ParentHandler};
use tokio::process::Command;

use crate::db::model::ExposedUI;
use crate::disk::mount::filesystem::idmapped::IdMapped;
use crate::disk::mount::filesystem::loop_dev::LoopDev;
use crate::disk::mount::filesystem::overlayfs::OverlayGuard;
use crate::prelude::*;
use crate::s9pk::rpc::SKIP_ENV;
use crate::service::cli::ContainerCliContext;
use crate::service::start_stop::StartStop;
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
        .subcommand("exposeUi", from_fn_async(expose_ui).no_cli())
        .subcommand(
            "createOverlayedImage",
            from_fn_async(create_overlayed_image)
                .with_custom_display_fn::<AnyContext, _>(|_, path| {
                    Ok(println!("{}", path.display()))
                })
                .with_remote_cli::<ContainerCliContext>(),
        )
        .subcommand(
            "getSslCertificate",
            from_fn_async(get_ssl_certificate).no_cli(),
        )
        .subcommand("getSslKey", from_fn_async(get_ssl_key).no_cli())
    // TODO @DrBonez when we get the new api for 4.0
    // .subcommand("setDependencies",from_fn(set_dependencies))
    // .subcommand("embassyGetInterface",from_fn(embassy_get_interface))
    // .subcommand("mount",from_fn(mount))
    // .subcommand("removeAction",from_fn(remove_action))
    // .subcommand("removeAddress",from_fn(remove_address))
    // .subcommand("exportAction",from_fn(export_action))
    // .subcommand("bind",from_fn(bind))
    // .subcommand("clearServiceInterfaces",from_fn(clear_network_interfaces))
    // .subcommand("exportServiceInterface",from_fn(export_network_interface))
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

#[derive(Debug, Clone, serde::Serialize, serde::Deserialize, Parser)]
#[serde(rename_all = "camelCase")]
struct ChrootParams {
    #[arg(short = 'e', long = "env")]
    env: Option<PathBuf>,
    #[arg(short = 'w', long = "workdir")]
    workdir: Option<PathBuf>,
    #[arg(short = 'u', long = "user")]
    user: Option<String>,
    path: PathBuf,
    command: OsString,
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
#[derive(Debug, Clone, serde::Serialize, serde::Deserialize)]
#[serde(rename_all = "camelCase")]
struct GetSslCertificateParams {
    package_id: Option<String>,
    algorithm: Option<String>, //"ecdsa" | "ed25519"
}

async fn get_ssl_certificate(
    context: EffectContext,
    GetSslCertificateParams {
        package_id,
        algorithm,
    }: GetSslCertificateParams,
) -> Result<Value, Error> {
    let fake = include_str!("./fake.cert.pem");
    Ok(json!([fake, fake, fake]))
}

#[derive(Debug, Clone, serde::Serialize, serde::Deserialize)]
#[serde(rename_all = "camelCase")]
struct GetSslKeyParams {
    package_id: Option<String>,
    algorithm: Option<String>, //"ecdsa" | "ed25519"
}

async fn get_ssl_key(
    context: EffectContext,
    GetSslKeyParams {
        package_id,
        algorithm,
    }: GetSslKeyParams,
) -> Result<Value, Error> {
    let fake = include_str!("./fake.cert.key");
    Ok(json!(fake))
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
    let context = context.deref()?;
    let peeked = context.ctx.db.peek().await;
    let package_id = package_id.unwrap_or(context.id.clone());
    let value = peeked
        .as_public()
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
    let context = context.deref()?;
    let package_id = context.id.clone();
    context
        .ctx
        .db
        .mutate(|db| {
            db.as_public_mut()
                .as_package_data_mut()
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
    let context = context.deref()?;
    let package_id = context.id.clone();
    context
        .ctx
        .db
        .mutate(|db| {
            db.as_public_mut()
                .as_package_data_mut()
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
#[derive(Debug, Clone, serde::Serialize, serde::Deserialize, Parser)]
#[serde(rename_all = "camelCase")]
#[command(rename_all = "camelCase")]
struct ParamsMaybePackageId {
    package_id: Option<PackageId>,
}

async fn exists(context: EffectContext, params: ParamsPackageId) -> Result<Value, Error> {
    let context = context.deref()?;
    let peeked = context.ctx.db.peek().await;
    let package = peeked
        .as_public()
        .as_package_data()
        .as_idx(&params.package)
        .is_some();
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
    let context = context.deref()?;
    let peeked = context.ctx.db.peek().await;
    let package_id = params.package_id.unwrap_or_else(|| context.id.clone());
    let package = peeked
        .as_public()
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
    let context = context.deref()?;
    let peeked = context.ctx.db.peek().await;
    let package_id = params.package_id.unwrap_or_else(|| context.id.clone());
    let package = peeked
        .as_public()
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

#[derive(Debug, Clone, serde::Serialize, serde::Deserialize, Parser)]
#[serde(rename_all = "camelCase")]
#[command(rename_all = "camelCase")]
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

#[derive(Debug, Clone, serde::Serialize, serde::Deserialize, Parser)]
#[serde(rename_all = "camelCase")]
#[command(rename_all = "camelCase")]
struct SetMainStatus {
    status: Status,
}
async fn set_main_status(context: EffectContext, params: SetMainStatus) -> Result<Value, Error> {
    let context = context.deref()?;
    context
        .persistent_container
        .current_state
        .send_replace(match params.status {
            Status::Running => StartStop::Start,
            Status::Stopped => StartStop::Stop,
        });
    Ok(Value::Null)
}

#[derive(Debug, Clone, serde::Serialize, serde::Deserialize)]
#[serde(rename_all = "camelCase")]
struct SetHealth {
    name: HealthCheckId,
    health_result: Option<HealthCheckResult>,
}

async fn set_health(context: EffectContext, params: SetHealth) -> Result<Value, Error> {
    let context = context.deref()?;
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
                .as_public()
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
            db.as_public_mut()
                .as_package_data_mut()
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

#[derive(serde::Deserialize, serde::Serialize, Parser)]
#[serde(rename_all = "camelCase")]
#[command(rename_all = "camelCase")]
pub struct CreateOverlayedImageParams {
    image_id: ImageId,
}

#[instrument(skip_all)]
pub async fn create_overlayed_image(
    ctx: EffectContext,
    CreateOverlayedImageParams { image_id }: CreateOverlayedImageParams,
) -> Result<PathBuf, Error> {
    let ctx = ctx.deref()?;
    let path = Path::new("images")
        .join(&*ARCH)
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
        Ok(container_mountpoint)
    } else {
        Err(Error::new(
            eyre!("image {image_id} not found in s9pk"),
            ErrorKind::NotFound,
        ))
    }
}
