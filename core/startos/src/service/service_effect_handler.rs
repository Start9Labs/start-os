use std::cmp::min;
use std::collections::{BTreeMap, BTreeSet};
use std::ffi::OsString;
use std::net::Ipv4Addr;
use std::os::unix::process::CommandExt;
use std::path::{Path, PathBuf};
use std::str::FromStr;
use std::sync::{Arc, Mutex, Weak};
use std::time::{Duration, SystemTime};

use clap::builder::ValueParserFactory;
use clap::Parser;
use exver::VersionRange;
use futures::future::join_all;
use helpers::NonDetachingJoinHandle;
use imbl::{vector, Vector};
use imbl_value::{json, InternedString};
use itertools::Itertools;
use log::warn;
use models::{
    ActionId, DataUrl, HealthCheckId, HostId, ImageId, PackageId, ServiceInterfaceId, VolumeId,
};
use patch_db::json_ptr::JsonPointer;
use patch_db::ModelExt;
use rpc_toolkit::{from_fn, from_fn_async, Context, Empty, HandlerExt, ParentHandler};
use serde::{Deserialize, Serialize};
use tokio::process::Command;
use ts_rs::TS;
use url::Url;

use crate::db::model::package::{
    ActionMetadata, CurrentDependencies, CurrentDependencyInfo, CurrentDependencyKind,
    ManifestPreference,
};
use crate::disk::mount::filesystem::overlayfs::OverlayGuard;
use crate::echo;
use crate::net::host::address::HostAddress;
use crate::net::host::binding::{BindOptions, LanInfo};
use crate::net::host::{Host, HostKind};
use crate::net::service_interface::{AddressInfo, ServiceInterface, ServiceInterfaceType};
use crate::net::ssl::FullchainCertData;
use crate::prelude::*;
use crate::rpc_continuations::Guid;
use crate::s9pk::merkle_archive::source::http::HttpSource;
use crate::s9pk::rpc::SKIP_ENV;
use crate::s9pk::S9pk;
use crate::service::cli::ContainerCliContext;
use crate::service::rpc::{CallbackHandle, CallbackId};
use crate::service::{Service, ServiceActorSeed};
use crate::status::health_check::HealthCheckResult;
use crate::status::MainStatus;
use crate::system::SmtpValue;
use crate::util::clap::FromStrParser;
use crate::util::Invoke;

#[derive(Clone)]
pub(super) struct EffectContext(Weak<Service>);
impl EffectContext {
    pub fn new(service: Weak<Service>) -> Self {
        Self(service)
    }
}
impl Context for EffectContext {}
impl EffectContext {
    fn deref(&self) -> Result<Arc<Service>, Error> {
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

pub fn service_effect_handler<C: Context>() -> ParentHandler<C> {
    ParentHandler::new()
        .subcommand("gitInfo", from_fn(|_: C| crate::version::git_info()))
        .subcommand(
            "echo",
            from_fn(echo::<EffectContext>).with_call_remote::<ContainerCliContext>(),
        )
        .subcommand(
            "chroot",
            from_fn(chroot::<ContainerCliContext>).no_display(),
        )
        .subcommand("exists", from_fn_async(exists).no_cli())
        .subcommand("executeAction", from_fn_async(execute_action).no_cli())
        .subcommand("getConfigured", from_fn_async(get_configured).no_cli())
        .subcommand(
            "stopped",
            from_fn_async(stopped)
                .no_display()
                .with_call_remote::<ContainerCliContext>(),
        )
        .subcommand(
            "running",
            from_fn_async(running)
                .no_display()
                .with_call_remote::<ContainerCliContext>(),
        )
        .subcommand(
            "restart",
            from_fn_async(restart)
                .no_display()
                .with_call_remote::<ContainerCliContext>(),
        )
        .subcommand(
            "shutdown",
            from_fn_async(shutdown)
                .no_display()
                .with_call_remote::<ContainerCliContext>(),
        )
        .subcommand(
            "setConfigured",
            from_fn_async(set_configured)
                .no_display()
                .with_call_remote::<ContainerCliContext>(),
        )
        .subcommand(
            "setMainStatus",
            from_fn_async(set_main_status).with_call_remote::<ContainerCliContext>(),
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
                .with_custom_display_fn(|_, (path, _)| Ok(println!("{}", path.display())))
                .with_call_remote::<ContainerCliContext>(),
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
                .with_call_remote::<ContainerCliContext>(),
        )
        .subcommand(
            "getDependencies",
            from_fn_async(get_dependencies)
                .no_display()
                .with_call_remote::<ContainerCliContext>(),
        )
        .subcommand(
            "checkDependencies",
            from_fn_async(check_dependencies)
                .no_display()
                .with_call_remote::<ContainerCliContext>(),
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
        .subcommand("mount", from_fn_async(mount).no_cli())
        .subcommand("clearCallbacks", from_fn(clear_callbacks).no_cli())

    // TODO Callbacks
}

#[derive(Default)]
pub struct ServiceCallbacks(Mutex<ServiceCallbackMap>);

#[derive(Default)]
struct ServiceCallbackMap {
    get_service_interface: BTreeMap<(PackageId, ServiceInterfaceId), Vec<CallbackHandler>>,
    list_service_interfaces: BTreeMap<PackageId, Vec<CallbackHandler>>,
    get_system_smtp: Vec<CallbackHandler>,
    get_host_info: BTreeMap<(PackageId, HostId), Vec<CallbackHandler>>,
    get_ssl_certificate:
        BTreeMap<BTreeSet<InternedString>, (NonDetachingJoinHandle<()>, Vec<CallbackHandler>)>,
}

impl ServiceCallbacks {
    fn mutate<T>(&self, f: impl FnOnce(&mut ServiceCallbackMap) -> T) -> T {
        let mut this = self.0.lock().unwrap();
        f(&mut *this)
    }

    pub fn gc(&self) {
        self.mutate(|this| {
            this.get_service_interface.retain(|_, v| {
                v.retain(|h| h.handle.is_active() && h.seed.strong_count() > 0);
                !v.is_empty()
            });
            this.list_service_interfaces.retain(|_, v| {
                v.retain(|h| h.handle.is_active() && h.seed.strong_count() > 0);
                !v.is_empty()
            });
            this.get_system_smtp
                .retain(|h| h.handle.is_active() && h.seed.strong_count() > 0);
            this.get_host_info.retain(|_, v| {
                v.retain(|h| h.handle.is_active() && h.seed.strong_count() > 0);
                !v.is_empty()
            });
        })
    }

    fn add_get_service_interface(
        &self,
        package_id: PackageId,
        service_interface_id: ServiceInterfaceId,
        handler: CallbackHandler,
    ) {
        self.mutate(|this| {
            this.get_service_interface
                .entry((package_id, service_interface_id))
                .or_default()
                .push(handler);
        })
    }

    #[must_use]
    pub fn get_service_interface(
        &self,
        id: &(PackageId, ServiceInterfaceId),
    ) -> Option<CallbackHandlers> {
        self.mutate(|this| {
            Some(CallbackHandlers(
                this.get_service_interface.remove(id).unwrap_or_default(),
            ))
            .filter(|cb| !cb.0.is_empty())
        })
    }

    fn add_list_service_interfaces(&self, package_id: PackageId, handler: CallbackHandler) {
        self.mutate(|this| {
            this.list_service_interfaces
                .entry(package_id)
                .or_default()
                .push(handler);
        })
    }

    #[must_use]
    pub fn list_service_interfaces(&self, id: &PackageId) -> Option<CallbackHandlers> {
        self.mutate(|this| {
            Some(CallbackHandlers(
                this.list_service_interfaces.remove(id).unwrap_or_default(),
            ))
            .filter(|cb| !cb.0.is_empty())
        })
    }

    fn add_get_system_smtp(&self, handler: CallbackHandler) {
        self.mutate(|this| {
            this.get_system_smtp.push(handler);
        })
    }

    #[must_use]
    pub fn get_system_smtp(&self) -> Option<CallbackHandlers> {
        self.mutate(|this| {
            Some(CallbackHandlers(std::mem::take(&mut this.get_system_smtp)))
                .filter(|cb| !cb.0.is_empty())
        })
    }

    fn add_get_host_info(&self, package_id: PackageId, host_id: HostId, handler: CallbackHandler) {
        self.mutate(|this| {
            this.get_host_info
                .entry((package_id, host_id))
                .or_default()
                .push(handler);
        })
    }

    #[must_use]
    pub fn get_host_info(&self, id: &(PackageId, HostId)) -> Option<CallbackHandlers> {
        self.mutate(|this| {
            Some(CallbackHandlers(
                this.get_host_info.remove(id).unwrap_or_default(),
            ))
            .filter(|cb| !cb.0.is_empty())
        })
    }

    fn add_get_ssl_certificate(
        &self,
        ctx: EffectContext,
        hostnames: BTreeSet<InternedString>,
        mut cert: FullchainCertData,
        handler: CallbackHandler,
    ) {
        self.mutate(|this| {
            this.get_ssl_certificate
                .entry(hostnames)
                .or_insert_with(|| {
                    (
                        tokio::spawn(async move {
                            loop {
                                match cert
                                    .expiration()
                                    .ok()
                                    .and_then(|e| e.duration_since(SystemTime::now()).ok())
                                {
                                    Some(d) => {
                                        tokio::time::sleep(min(Duration::from_secs(86400), d)).await
                                    }
                                    _ => break,
                                }
                            }
                            let Ok(ctx) = ctx.deref() else {
                                return ();
                            };
                            ctx.seed
                                .ctx
                                .callbacks
                                .mutate(|this| this.get_ssl_certificate.get(key))
                        })
                        .into(),
                        Vec::new(),
                    )
                })
                .1
                .push(handler);
        })
    }
}

pub struct CallbackHandler {
    handle: CallbackHandle,
    seed: Weak<ServiceActorSeed>,
}
impl CallbackHandler {
    pub fn new(service: &Service, handle: CallbackHandle) -> Self {
        Self {
            handle,
            seed: Arc::downgrade(&service.seed),
        }
    }
    pub async fn call(mut self, args: Vector<Value>) -> Result<(), Error> {
        if let Some(seed) = self.seed.upgrade() {
            seed.persistent_container
                .callback(self.handle.take(), args)
                .await?;
        }
        Ok(())
    }
}
impl Drop for CallbackHandler {
    fn drop(&mut self) {
        if self.handle.is_active() {
            warn!("Callback handler dropped while still active!");
        }
    }
}

pub struct CallbackHandlers(Vec<CallbackHandler>);
impl CallbackHandlers {
    pub async fn call(self, args: Vector<Value>) -> Result<(), Error> {
        let mut err = ErrorCollection::new();
        for res in join_all(self.0.into_iter().map(|cb| cb.call(args.clone()))).await {
            err.handle(res);
        }
        err.into_result()
    }
}

#[derive(Debug, Clone, serde::Serialize, serde::Deserialize, TS)]
#[ts(export)]
#[serde(rename_all = "camelCase")]
struct GetSystemSmtpParams {
    callback: Option<CallbackId>,
}

#[derive(Debug, Clone, serde::Serialize, serde::Deserialize, TS)]
#[ts(export)]
#[serde(rename_all = "camelCase")]
struct GetServicePortForwardParams {
    #[ts(type = "string | null")]
    package_id: Option<PackageId>,
    internal_port: u32,
    host_id: HostId,
}

#[derive(Debug, Clone, serde::Serialize, serde::Deserialize, TS)]
#[ts(export)]
#[serde(rename_all = "camelCase")]
struct ExportServiceInterfaceParams {
    id: ServiceInterfaceId,
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
    service_interface_id: ServiceInterfaceId,
    callback: Option<CallbackId>,
}

#[derive(Debug, Clone, serde::Serialize, serde::Deserialize, TS)]
#[ts(export)]
#[serde(rename_all = "camelCase")]
struct ListServiceInterfacesParams {
    #[ts(type = "string | null")]
    package_id: Option<PackageId>,
    callback: Option<CallbackId>,
}

#[derive(Debug, Clone, serde::Serialize, serde::Deserialize, TS)]
#[ts(export)]
#[serde(rename_all = "camelCase")]
struct RemoveAddressParams {
    id: ServiceInterfaceId,
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
    GetSystemSmtpParams { callback }: GetSystemSmtpParams,
) -> Result<Option<SmtpValue>, Error> {
    let context = context.deref()?;
    let res = context
        .seed
        .ctx
        .db
        .peek()
        .await
        .into_public()
        .into_server_info()
        .into_smtp()
        .de()?;

    if let Some(callback) = callback {
        let callback = callback.register(&context.seed.persistent_container);
        context
            .seed
            .ctx
            .callbacks
            .add_get_system_smtp(CallbackHandler::new(&context, callback));
    }

    Ok(res)
}
async fn get_container_ip(context: EffectContext, _: Empty) -> Result<Ipv4Addr, Error> {
    let context = context.deref()?;
    let net_service = context.seed.persistent_container.net_service.lock().await;
    Ok(net_service.get_ip())
}
async fn get_service_port_forward(
    context: EffectContext,
    data: GetServicePortForwardParams,
) -> Result<LanInfo, Error> {
    let internal_port = data.internal_port as u16;

    let context = context.deref()?;
    let net_service = context.seed.persistent_container.net_service.lock().await;
    net_service.get_ext_port(data.host_id, internal_port)
}
async fn clear_network_interfaces(context: EffectContext, _: Empty) -> Result<(), Error> {
    let context = context.deref()?;
    let package_id = context.seed.id.clone();

    context
        .seed
        .ctx
        .db
        .mutate(|db| {
            let model = db
                .as_public_mut()
                .as_package_data_mut()
                .as_idx_mut(&package_id)
                .or_not_found(&package_id)?
                .as_service_interfaces_mut();
            let mut new_map = BTreeMap::new();
            model.ser(&mut new_map)
        })
        .await
}
async fn export_service_interface(
    context: EffectContext,
    ExportServiceInterfaceParams {
        id,
        name,
        description,
        has_primary,
        disabled,
        masked,
        address_info,
        r#type,
    }: ExportServiceInterfaceParams,
) -> Result<(), Error> {
    let context = context.deref()?;
    let package_id = context.seed.id.clone();

    let service_interface = ServiceInterface {
        id: id.clone(),
        name,
        description,
        has_primary,
        disabled,
        masked,
        address_info,
        interface_type: r#type,
    };

    let map = context
        .seed
        .ctx
        .db
        .mutate(|db| {
            let map = db
                .as_public_mut()
                .as_package_data_mut()
                .as_idx_mut(&package_id)
                .or_not_found(&package_id)?
                .as_service_interfaces_mut();
            map.insert(&id, &service_interface)?;
            Ok(map.clone())
        })
        .await?;
    if let Some(callbacks) = context
        .seed
        .ctx
        .callbacks
        .get_service_interface(&(package_id.clone(), id))
    {
        callbacks
            .call(vector![to_value(&service_interface)?])
            .await?;
    }
    if let Some(callbacks) = context
        .seed
        .ctx
        .callbacks
        .list_service_interfaces(&package_id)
    {
        callbacks.call(vector![map.into_value()]).await?;
    }

    Ok(())
}

async fn get_primary_url(
    context: EffectContext,
    GetPrimaryUrlParams {
        package_id,
        service_interface_id,
        callback,
    }: GetPrimaryUrlParams,
) -> Result<Option<HostAddress>, Error> {
    let context = context.deref()?;
    let package_id = package_id.unwrap_or_else(|| context.seed.id.clone());

    Ok(None) // TODO
}
async fn list_service_interfaces(
    context: EffectContext,
    ListServiceInterfacesParams {
        package_id,
        callback,
    }: ListServiceInterfacesParams,
) -> Result<BTreeMap<ServiceInterfaceId, ServiceInterface>, Error> {
    let context = context.deref()?;
    let package_id = package_id.unwrap_or_else(|| context.seed.id.clone());

    let res = context
        .seed
        .ctx
        .db
        .peek()
        .await
        .into_public()
        .into_package_data()
        .into_idx(&package_id)
        .map(|m| m.into_service_interfaces().de())
        .transpose()?
        .unwrap_or_default();

    if let Some(callback) = callback {
        let callback = callback.register(&context.seed.persistent_container);
        context
            .seed
            .ctx
            .callbacks
            .add_list_service_interfaces(package_id, CallbackHandler::new(&context, callback));
    }

    Ok(res)
}
async fn remove_address(context: EffectContext, data: RemoveAddressParams) -> Result<(), Error> {
    let context = context.deref()?;
    let package_id = context.seed.id.clone();

    context
        .seed
        .ctx
        .db
        .mutate(|db| {
            let model = db
                .as_public_mut()
                .as_package_data_mut()
                .as_idx_mut(&package_id)
                .or_not_found(&package_id)?
                .as_service_interfaces_mut();
            model.remove(&data.id)
        })
        .await?;
    Ok(())
}
async fn export_action(context: EffectContext, data: ExportActionParams) -> Result<(), Error> {
    let context = context.deref()?;
    let package_id = context.seed.id.clone();
    context
        .seed
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
    let package_id = context.seed.id.clone();
    context
        .seed
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

async fn mount(context: EffectContext, data: MountParams) -> Result<Value, Error> {
    // TODO
    todo!()
}

#[derive(Debug, Clone, serde::Serialize, serde::Deserialize, TS)]
#[serde(rename_all = "camelCase")]
#[ts(export)]
struct GetHostInfoParams {
    host_id: HostId,
    #[ts(type = "string | null")]
    package_id: Option<PackageId>,
    callback: Option<CallbackId>,
}
async fn get_host_info(
    context: EffectContext,
    GetHostInfoParams {
        host_id,
        package_id,
        callback,
    }: GetHostInfoParams,
) -> Result<Option<Host>, Error> {
    let context = context.deref()?;
    let db = context.seed.ctx.db.peek().await;
    let package_id = package_id.unwrap_or_else(|| context.seed.id.clone());

    let res = db
        .as_public()
        .as_package_data()
        .as_idx(&package_id)
        .and_then(|m| m.as_hosts().as_idx(&host_id))
        .map(|m| m.de())
        .transpose()?;

    if let Some(callback) = callback {
        let callback = callback.register(&context.seed.persistent_container);
        context.seed.ctx.callbacks.add_get_host_info(
            package_id,
            host_id,
            CallbackHandler::new(&context, callback),
        );
    }

    Ok(res)
}

async fn clear_bindings(context: EffectContext, _: Empty) -> Result<(), Error> {
    let context = context.deref()?;
    let mut svc = context.seed.persistent_container.net_service.lock().await;
    svc.clear_bindings().await?;
    Ok(())
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
async fn bind(context: EffectContext, bind_params: Value) -> Result<(), Error> {
    let BindParams {
        kind,
        id,
        internal_port,
        options,
    } = from_value(bind_params)?;
    let context = context.deref()?;
    let mut svc = context.seed.persistent_container.net_service.lock().await;
    svc.bind(kind, id, internal_port, options).await
}

#[derive(Debug, Clone, serde::Serialize, serde::Deserialize, TS)]
#[serde(rename_all = "camelCase")]
#[ts(export)]
struct GetServiceInterfaceParams {
    #[ts(type = "string | null")]
    package_id: Option<PackageId>,
    service_interface_id: ServiceInterfaceId,
    callback: Option<CallbackId>,
}

async fn get_service_interface(
    context: EffectContext,
    GetServiceInterfaceParams {
        package_id,
        service_interface_id,
        callback,
    }: GetServiceInterfaceParams,
) -> Result<Option<ServiceInterface>, Error> {
    let context = context.deref()?;
    let package_id = package_id.unwrap_or_else(|| context.seed.id.clone());
    let db = context.seed.ctx.db.peek().await;

    let interface = db
        .as_public()
        .as_package_data()
        .as_idx(&package_id)
        .and_then(|m| m.as_service_interfaces().as_idx(&service_interface_id))
        .map(|m| m.de())
        .transpose()?;

    if let Some(callback) = callback {
        let callback = callback.register(&context.seed.persistent_container);
        context.seed.ctx.callbacks.add_get_service_interface(
            package_id,
            service_interface_id,
            CallbackHandler::new(&context, callback),
        );
    }

    Ok(interface)
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
fn chroot<C: Context>(
    _: C,
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
    nix::unistd::setsid().ok(); // https://stackoverflow.com/questions/25701333/os-setsid-operation-not-permitted
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
    #[ts(type = "string[]")]
    hostnames: BTreeSet<InternedString>,
    algorithm: Option<Algorithm>, //"ecdsa" | "ed25519"
    callback: Option<CallbackId>,
}

async fn get_ssl_certificate(
    context: EffectContext,
    GetSslCertificateParams {
        hostnames,
        algorithm,
        callback,
    }: GetSslCertificateParams,
) -> Result<Vec<String>, Error> {
    let context = context.deref()?;
    let algorithm = algorithm.unwrap_or(Algorithm::Ecdsa);

    let cert = context
        .seed
        .ctx
        .db
        .mutate(|db| {
            let errfn = |h: &str| Error::new(eyre!("unknown hostname: {h}"), ErrorKind::NotFound);
            let entries = db.as_public().as_package_data().as_entries()?;
            let packages = entries.iter().map(|(k, _)| k).collect::<BTreeSet<_>>();
            let allowed_hostnames = entries
                .iter()
                .map(|(_, m)| m.as_hosts().as_entries())
                .flatten_ok()
                .map_ok(|(_, m)| m.as_addresses().de())
                .map(|a| a.and_then(|a| a))
                .flatten_ok()
                .try_collect::<_, BTreeSet<_>, _>()?;
            for hostname in &hostnames {
                if let Some(internal) = hostname
                    .strip_suffix(".embassy")
                    .or_else(|| hostname.strip_suffix(".startos"))
                {
                    if !packages.contains(internal) {
                        return Err(errfn(&*hostname));
                    }
                }
                if !allowed_hostnames.contains(&hostname.parse()?) {
                    return Err(errfn(&*hostname));
                }
            }
            db.as_private_mut()
                .as_key_store_mut()
                .as_local_certs_mut()
                .cert_for(&hostnames)
        })
        .await?;
    let fullchain = match algorithm {
        Algorithm::Ecdsa => cert.fullchain_nistp256(),
        Algorithm::Ed25519 => cert.fullchain_ed25519(),
    };

    let res = fullchain
        .into_iter()
        .map(|c| c.to_pem())
        .map_ok(String::from_utf8)
        .map(|a| Ok::<_, Error>(a??))
        .try_collect()?;

    if let Some(callback) = callback {
        let exp = cert.expiration()?;
        let callback = callback.register(&context.seed.persistent_container);
        context.seed.ctx.callbacks.add_get_ssl_certificate(
            hostnames,
            exp,
            CallbackHandler::new(&context, callback),
        );
    }

    Ok(res)
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
    // TODO
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
    callback: Option<CallbackId>,
}

async fn get_store(
    context: EffectContext,
    GetStoreParams {
        package_id,
        path,
        callback,
    }: GetStoreParams,
) -> Result<Value, Error> {
    let context = context.deref()?;
    let peeked = context.seed.ctx.db.peek().await;
    let package_id = package_id.unwrap_or(context.seed.id.clone());
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
    let package_id = context.seed.id.clone();
    context
        .seed
        .ctx
        .db
        .mutate(|db| {
            let model = db
                .as_private_mut()
                .as_package_stores_mut()
                .upsert(&package_id, || Ok(json!({})))?;
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
    let peeked = context.seed.ctx.db.peek().await;
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
    #[serde(default)]
    procedure_id: Guid,
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
        procedure_id,
        service_id,
        action_id,
        input,
    }: ExecuteAction,
) -> Result<Value, Error> {
    let context = context.deref()?;
    let package_id = service_id
        .clone()
        .unwrap_or_else(|| context.seed.id.clone());

    Ok(json!(context.action(procedure_id, action_id, input).await?))
}
#[derive(Debug, Clone, serde::Serialize, serde::Deserialize)]
#[serde(rename_all = "camelCase")]
struct FromService {}
async fn get_configured(context: EffectContext, _: Empty) -> Result<Value, Error> {
    let context = context.deref()?;
    let peeked = context.seed.ctx.db.peek().await;
    let package_id = &context.seed.id;
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
    let peeked = context.seed.ctx.db.peek().await;
    let package_id = params.package_id.unwrap_or_else(|| context.seed.id.clone());
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
    let context = context.deref()?;
    let peeked = context.seed.ctx.db.peek().await;
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

#[derive(Debug, Clone, serde::Serialize, serde::Deserialize, Parser, TS)]
#[serde(rename_all = "camelCase")]
#[ts(export)]
struct ProcedureId {
    #[serde(default)]
    #[arg(default_value_t, long)]
    procedure_id: Guid,
}

async fn restart(
    context: EffectContext,
    ProcedureId { procedure_id }: ProcedureId,
) -> Result<(), Error> {
    let context = context.deref()?;
    context.restart(procedure_id).await?;
    Ok(())
}

async fn shutdown(
    context: EffectContext,
    ProcedureId { procedure_id }: ProcedureId,
) -> Result<(), Error> {
    let context = context.deref()?;
    context.stop(procedure_id).await?;
    Ok(())
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
    let package_id = &context.seed.id;
    context
        .seed
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
enum SetMainStatusStatus {
    Running,
    Stopped,
}
impl FromStr for SetMainStatusStatus {
    type Err = color_eyre::eyre::Report;
    fn from_str(s: &str) -> Result<Self, Self::Err> {
        match s {
            "running" => Ok(Self::Running),
            "stopped" => Ok(Self::Stopped),
            _ => Err(eyre!("unknown status {s}")),
        }
    }
}
impl ValueParserFactory for SetMainStatusStatus {
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
    status: SetMainStatusStatus,
}
async fn set_main_status(context: EffectContext, params: SetMainStatus) -> Result<Value, Error> {
    let context = context.deref()?;
    match params.status {
        SetMainStatusStatus::Running => context.seed.started(),
        SetMainStatusStatus::Stopped => context.seed.stopped(),
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

    let package_id = &context.seed.id;
    context
        .seed
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
    guid: Guid,
}

#[instrument(skip_all)]
pub async fn destroy_overlayed_image(
    context: EffectContext,
    DestroyOverlayedImageParams { guid }: DestroyOverlayedImageParams,
) -> Result<(), Error> {
    let context = context.deref()?;
    if context
        .seed
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
    image_id: ImageId,
}

#[instrument(skip_all)]
pub async fn create_overlayed_image(
    context: EffectContext,
    CreateOverlayedImageParams { image_id }: CreateOverlayedImageParams,
) -> Result<(PathBuf, Guid), Error> {
    let context = context.deref()?;
    if let Some(image) = context
        .seed
        .persistent_container
        .images
        .get(&image_id)
        .cloned()
    {
        let guid = Guid::new();
        let rootfs_dir = context
            .seed
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
        let mountpoint = rootfs_dir
            .join("media/startos/overlays")
            .join(guid.as_ref());
        tokio::fs::create_dir_all(&mountpoint).await?;
        let container_mountpoint = Path::new("/").join(
            mountpoint
                .strip_prefix(rootfs_dir)
                .with_kind(ErrorKind::Incoherent)?,
        );
        tracing::info!("Mounting overlay {guid} for {image_id}");
        let guard = OverlayGuard::mount(image, &mountpoint).await?;
        Command::new("chown")
            .arg("100000:100000")
            .arg(&mountpoint)
            .invoke(ErrorKind::Filesystem)
            .await?;
        tracing::info!("Mounted overlay {guid} for {image_id}");
        context
            .seed
            .persistent_container
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
        version_range: VersionRange,
    },
    #[serde(rename_all = "camelCase")]
    Exists {
        #[ts(type = "string")]
        id: PackageId,
        #[ts(type = "string")]
        version_range: VersionRange,
    },
}
// filebrowser:exists,bitcoind:running:foo+bar+baz
impl FromStr for DependencyRequirement {
    type Err = Error;
    fn from_str(s: &str) -> Result<Self, Self::Err> {
        match s.split_once(':') {
            Some((id, "e")) | Some((id, "exists")) => Ok(Self::Exists {
                id: id.parse()?,
                version_range: "*".parse()?, // TODO
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
                    version_range: "*".parse()?, // TODO
                })
            }
            None => Ok(Self::Running {
                id: s.parse()?,
                health_checks: BTreeSet::new(),
                version_range: "*".parse()?, // TODO
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
    #[serde(default)]
    procedure_id: Guid,
    dependencies: Vec<DependencyRequirement>,
}

async fn set_dependencies(
    context: EffectContext,
    SetDependenciesParams {
        procedure_id,
        dependencies,
    }: SetDependenciesParams,
) -> Result<(), Error> {
    let context = context.deref()?;
    let id = &context.seed.id;

    let mut deps = BTreeMap::new();
    for dependency in dependencies {
        let (dep_id, kind, version_range) = match dependency {
            DependencyRequirement::Exists { id, version_range } => {
                (id, CurrentDependencyKind::Exists, version_range)
            }
            DependencyRequirement::Running {
                id,
                health_checks,
                version_range,
            } => (
                id,
                CurrentDependencyKind::Running { health_checks },
                version_range,
            ),
        };
        let config_satisfied =
            if let Some(dep_service) = &*context.seed.ctx.services.get(&dep_id).await {
                context
                    .dependency_config(
                        procedure_id.clone(),
                        dep_id.clone(),
                        dep_service.get_config(procedure_id.clone()).await?.config,
                    )
                    .await?
                    .is_none()
            } else {
                true
            };
        let info = CurrentDependencyInfo {
            title: context
                .seed
                .persistent_container
                .s9pk
                .dependency_metadata(&dep_id)
                .await?
                .map(|m| m.title),
            icon: context
                .seed
                .persistent_container
                .s9pk
                .dependency_icon_data_url(&dep_id)
                .await?,
            kind,
            version_range,
            config_satisfied,
        };
        deps.insert(dep_id, info);
    }
    context
        .seed
        .ctx
        .db
        .mutate(|db| {
            db.as_public_mut()
                .as_package_data_mut()
                .as_idx_mut(id)
                .or_not_found(id)?
                .as_current_dependencies_mut()
                .ser(&CurrentDependencies(deps))
        })
        .await
}

async fn get_dependencies(context: EffectContext) -> Result<Vec<DependencyRequirement>, Error> {
    let context = context.deref()?;
    let id = &context.seed.id;
    let db = context.seed.ctx.db.peek().await;
    let data = db
        .as_public()
        .as_package_data()
        .as_idx(id)
        .or_not_found(id)?
        .as_current_dependencies()
        .de()?;

    data.0
        .into_iter()
        .map(|(id, current_dependency_info)| {
            let CurrentDependencyInfo {
                version_range,
                kind,
                ..
            } = current_dependency_info;
            Ok::<_, Error>(match kind {
                CurrentDependencyKind::Exists => {
                    DependencyRequirement::Exists { id, version_range }
                }
                CurrentDependencyKind::Running { health_checks } => {
                    DependencyRequirement::Running {
                        id,
                        health_checks,
                        version_range,
                    }
                }
            })
        })
        .try_collect()
}

#[derive(Debug, Clone, serde::Serialize, serde::Deserialize, Parser, TS)]
#[serde(rename_all = "camelCase")]
#[command(rename_all = "camelCase")]
#[ts(export)]
struct CheckDependenciesParam {
    package_ids: Option<Vec<PackageId>>,
}
#[derive(Debug, Clone, serde::Serialize, serde::Deserialize, TS)]
#[serde(rename_all = "camelCase")]
#[ts(export)]
struct CheckDependenciesResult {
    package_id: PackageId,
    is_installed: bool,
    is_running: bool,
    config_satisfied: bool,
    health_checks: BTreeMap<HealthCheckId, HealthCheckResult>,
    #[ts(type = "string | null")]
    version: Option<exver::ExtendedVersion>,
}

async fn check_dependencies(
    context: EffectContext,
    CheckDependenciesParam { package_ids }: CheckDependenciesParam,
) -> Result<Vec<CheckDependenciesResult>, Error> {
    let context = context.deref()?;
    let db = context.seed.ctx.db.peek().await;
    let current_dependencies = db
        .as_public()
        .as_package_data()
        .as_idx(&context.seed.id)
        .or_not_found(&context.seed.id)?
        .as_current_dependencies()
        .de()?;
    let package_ids: Vec<_> = package_ids
        .unwrap_or_else(|| current_dependencies.0.keys().cloned().collect())
        .into_iter()
        .filter_map(|x| {
            let info = current_dependencies.0.get(&x)?;
            Some((x, info))
        })
        .collect();
    let mut results = Vec::with_capacity(package_ids.len());

    for (package_id, dependency_info) in package_ids {
        let Some(package) = db.as_public().as_package_data().as_idx(&package_id) else {
            results.push(CheckDependenciesResult {
                package_id,
                is_installed: false,
                is_running: false,
                config_satisfied: false,
                health_checks: Default::default(),
                version: None,
            });
            continue;
        };
        let manifest = package.as_state_info().as_manifest(ManifestPreference::New);
        let installed_version = manifest.as_version().de()?.into_version();
        let satisfies = manifest.as_satisfies().de()?;
        let version = Some(installed_version.clone());
        if ![installed_version]
            .into_iter()
            .chain(satisfies.into_iter().map(|v| v.into_version()))
            .any(|v| v.satisfies(&dependency_info.version_range))
        {
            results.push(CheckDependenciesResult {
                package_id,
                is_installed: false,
                is_running: false,
                config_satisfied: false,
                health_checks: Default::default(),
                version,
            });
            continue;
        }
        let is_installed = true;
        let status = package.as_status().as_main().de()?;
        let is_running = if is_installed {
            status.running()
        } else {
            false
        };
        let health_checks =
            if let CurrentDependencyKind::Running { health_checks } = &dependency_info.kind {
                status
                    .health()
                    .cloned()
                    .unwrap_or_default()
                    .into_iter()
                    .filter(|(id, _)| health_checks.contains(id))
                    .collect()
            } else {
                Default::default()
            };
        results.push(CheckDependenciesResult {
            package_id,
            is_installed,
            is_running,
            config_satisfied: dependency_info.config_satisfied,
            health_checks,
            version,
        });
    }
    Ok(results)
}

fn clear_callbacks(context: EffectContext) -> Result<(), Error> {
    let context = context.deref()?;
    context
        .seed
        .persistent_container
        .state
        .send_if_modified(|s| !std::mem::take(&mut s.callbacks).is_empty());
    context.seed.ctx.callbacks.gc();
    Ok(())
}
