use std::collections::BTreeMap;
use std::marker::PhantomData;

use futures::future::BoxFuture;
use futures::{Future, FutureExt};
use imbl_value::json;
use models::{ActionId, PackageId};
use rpc_toolkit::from_fn_async;
use serde::de::DeserializeOwned;

use crate::action::action;
use crate::context::RpcContext;
use crate::prelude::*;
use crate::status::MainStatus;

#[derive(Debug, Clone, serde::Serialize, serde::Deserialize)]
struct RpcData {
    id: i64,
    method: String,
    params: Value,
}
struct ServiceEffectsService {
    package_id: PackageId,
    routes: BTreeMap<String, Box<dyn Route>>,
}

impl ServiceEffectsService {
    pub fn new(package_id: PackageId) -> Self {
        let mut service_effects = Self {
            package_id,
            routes: Default::default(),
        };
        use routes::*;

        service_effects
            .route("exists", from_fn(exists))
            .route("executeAction", from_fn(execute_action))
            .route("getConfigured", from_fn(get_configured))
            .route("stopped", from_fn(stopped))
            .route("running", from_fn(running))
            .route("restart", from_fn(restart))
            .route("shutdown", from_fn(shutdown))
            .route("setConfigured", from_fn(set_configured))
            .route("setHealth", from_fn(set_health));
        // TODO @DrBonez when we get the new api for 4.0?
        // .route("setDependencies",from_fn(set_dependencies))
        // .route("getStore",from_fn(get_store))
        // .route("setStore",from_fn(set_store))
        // .route("embassyGetInterface",from_fn(embassy_get_interface))
        // .route("mount",from_fn(mount))
        // .route("getSslCertificate",from_fn(get_ssl_certificate))
        // .route("getSslKey",from_fn(get_ssl_key))
        // .route("removeAction",from_fn(remove_action))
        // .route("removeAddress",from_fn(remove_address))
        // .route("exportAction",from_fn(export_action))
        // .route("bind",from_fn(bind))
        // .route("clearNetworkInterfaces",from_fn(clear_network_interfaces))
        // .route("exportNetworkInterface",from_fn(export_network_interface))
        // .route("exposeForDependents",from_fn(expose_for_dependents))
        // .route("exposeUi",from_fn(expose_ui))
        // .route("clearBindings",from_fn(clear_bindings))
        // .route("getHostnames",from_fn(get_hostnames))
        // .route("getInterface",from_fn(get_interface))
        // .route("listInterface",from_fn(list_interface))
        // .route("getIPHostname",from_fn(get_ip_hostname))
        // .route("getContainerIp",from_fn(get_container_ip))
        // .route("getLocalHostname",from_fn(get_local_hostname))
        // .route("getPrimaryUrl",from_fn(get_primary_url))
        // .route("getServicePortForward",from_fn(get_service_port_forward))
        // .route("getServiceTorHostname",from_fn(get_service_tor_hostname))
        // .route("getSystemSmtp",from_fn(get_system_smtp))
        // .route("reverseProxy",from_fn(reverse_pro)xy)
        service_effects
    }

    fn route(&mut self, location: &str, route: impl Route) -> &mut Self {
        self.routes.insert(location.to_string(), Box::new(route));
        self
    }

    pub async fn run_effects(&self, context: RpcContext, data: RpcData) -> Value {
        let Some(route) = self.routes.get(&data.method) else {
            return json!({"Error": format!("Could not find route {method}", method = &data.method)});
        };
        let service_run_params = ServiceEffectsServiceParams {
            context,
            package_id: self.package_id.clone(),
            data,
        };
        route.run(&service_run_params).await
    }
}
fn from_fn<F, T>(f: F) -> FromFn<F, T>
where
    F: Clone,
{
    FromFn {
        f: f.clone(),
        _extractor: PhantomData,
    }
}

struct ServiceEffectsServiceParams {
    context: RpcContext,
    package_id: PackageId,
    data: RpcData,
}

trait Route {
    fn run(&self, params: &ServiceEffectsServiceParams) -> BoxFuture<Value>;
}

trait IntoResponse {
    fn into_response(self) -> Result<Value, Error>;
}

impl IntoResponse for Value {
    fn into_response(self) -> Result<Value, Error> {
        Ok(self)
    }
}
impl IntoResponse for Result<Value, Error> {
    fn into_response(self) -> Result<Value, Error> {
        self
    }
}

struct FromFn<F: Clone, T> {
    f: F,
    _extractor: PhantomData<T>,
}

impl<F, Fut, Out> Route for FromFn<F, ()>
where
    F: FnMut() -> Fut + Clone + Send,
    Fut: Future<Output = Out> + Send + 'static,
    Out: IntoResponse + 'static,
{
    fn run(&self, params: &ServiceEffectsServiceParams) -> BoxFuture<Value> {
        let mut function = self.f.clone();
        Box::pin(
            async move { (function().await).into_response() }
                .map(|x| x.unwrap_or_else(|e| json!({"error": e.to_string()}))),
        )
    }
}
impl<F, A, Fut, Out> Route for FromFn<F, A>
where
    F: FnMut(A) -> Fut + Clone + Send,
    A: ExtractParam,
    Fut: Future<Output = Out> + Send + 'static,
    Out: IntoResponse + 'static,
{
    fn run(&self, params: &ServiceEffectsServiceParams) -> BoxFuture<Value> {
        let mut function = self.f.clone();
        Box::pin(
            async move { (function(A::extract(params)?).await).into_response() }
                .map(|x| x.unwrap_or_else(|e| json!({"error": e.to_string()}))),
        )
    }
}
impl<F, A, B, Fut, Out> Route for FromFn<F, (A, B)>
where
    F: FnMut(A, B) -> Fut + Clone + Send,
    A: ExtractParam,
    B: ExtractParam,
    Fut: Future<Output = Out> + Send + 'static,
    Out: IntoResponse + 'static,
{
    fn run(&self, params: &ServiceEffectsServiceParams) -> BoxFuture<Value> {
        let mut function = self.f.clone();
        Box::pin(
            async move { (function(A::extract(params)?, B::extract(params)?).await).into_response() }
                .map(|x| x.unwrap_or_else(|e| json!({"error": e.to_string()}))),
        )
    }
}
impl<F, A, B, C, Fut, Out> Route for FromFn<F, (A, B, C)>
where
    F: FnMut(A, B, C) -> Fut + Clone + Send,
    A: ExtractParam,
    B: ExtractParam,
    C: ExtractParam,
    Fut: Future<Output = Out> + Send + 'static,
    Out: IntoResponse + 'static,
{
    fn run(&self, params: &ServiceEffectsServiceParams) -> BoxFuture<Value> {
        let mut function = self.f.clone();
        Box::pin(
            async move {
                (function(
                    A::extract(params)?,
                    B::extract(params)?,
                    C::extract(params)?,
                )
                .await)
                    .into_response()
            }
            .map(|x| x.unwrap_or_else(|e| json!({"error": e.to_string()}))),
        )
    }
}
trait ExtractParam: Sized {
    fn extract(params: &ServiceEffectsServiceParams) -> Result<Self, Error>;
}
struct EJson<A: DeserializeOwned>(A);
impl<A: DeserializeOwned> ExtractParam for EJson<A> {
    fn extract(params: &ServiceEffectsServiceParams) -> Result<Self, Error> {
        Ok(EJson(from_value(params.data.params.clone())?))
    }
}

impl ExtractParam for PackageId {
    fn extract(params: &ServiceEffectsServiceParams) -> Result<Self, Error> {
        Ok(params.package_id.clone())
    }
}
impl ExtractParam for RpcContext {
    fn extract(params: &ServiceEffectsServiceParams) -> Result<Self, Error> {
        Ok(params.context.clone())
    }
}

mod routes {
    use models::HealthCheckId;

    use super::*;
    use crate::action::ActionParams;
    use crate::status::health_check::HealthCheckResult;

    #[derive(Debug, Clone, serde::Serialize, serde::Deserialize)]
    struct ParamsPackageId {
        package: PackageId,
    }
    #[derive(Debug, Clone, serde::Serialize, serde::Deserialize)]
    #[serde(rename_all = "camelCase")]
    struct ParamsMaybePackageId {
        package_id: Option<PackageId>,
    }

    pub async fn exists(EJson(params): EJson<ParamsPackageId>, context: RpcContext) -> Value {
        let peeked = context.db.peek().await;
        let package = peeked.as_package_data().as_idx(&params.package).is_some();
        json!(package)
    }

    #[derive(Debug, Clone, serde::Serialize, serde::Deserialize)]
    #[serde(rename_all = "camelCase")]
    struct ExecuteAction {
        service_id: Option<PackageId>,
        action_id: ActionId,
        input: Value,
    }
    pub async fn execute_action(
        EJson(params): EJson<ExecuteAction>,
        context: RpcContext,
        package_id: PackageId,
    ) -> Result<Value, Error> {
        let package_id = params
            .service_id
            .clone()
            .unwrap_or_else(|| package_id.clone());
        let action_id = params.action_id.clone();
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
        todo!()
        // imbl_value::to_value(&action_result).with_kind(ErrorKind::Serialization)
    }
    pub async fn get_configured(
        context: RpcContext,
        package_id: PackageId,
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

    pub async fn stopped(
        EJson(params): EJson<ParamsMaybePackageId>,
        context: RpcContext,
        package_id: PackageId,
    ) -> Result<Value, Error> {
        let peeked = context.db.peek().await;
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
    pub async fn running(
        EJson(params): EJson<ParamsMaybePackageId>,
        context: RpcContext,
        package_id: PackageId,
    ) -> Result<Value, Error> {
        let peeked = context.db.peek().await;
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

    pub async fn restart(context: RpcContext, package_id: PackageId) -> Result<Value, Error> {
        let manager = context.services.get(&package_id).await.ok_or_else(|| {
            Error::new(
                eyre!("Could not find package {package_id}"),
                ErrorKind::Unknown,
            )
        })?;
        manager.restart().await;
        Ok(json!(()))
    }

    pub async fn shutdown(context: RpcContext, package_id: PackageId) -> Result<Value, Error> {
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
    }
    pub async fn set_configured(
        EJson(params): EJson<SetConfigured>,
        context: RpcContext,
        package_id: PackageId,
    ) -> Result<Value, Error> {
        context
            .db
            .mutate(|db| {
                db.as_package_data_mut()
                    .as_idx_mut(&package_id)
                    .or_not_found(&package_id)?
                    .as_installed_mut()
                    .or_not_found(&package_id)?
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

    pub async fn set_health(
        EJson(params): EJson<SetHealth>,
        context: RpcContext,
        package_id: PackageId,
    ) -> Result<Value, Error> {
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
            .mutate(|db| {
                let mut main = db
                    .as_package_data()
                    .as_idx(&package_id)
                    .or_not_found(&package_id)?
                    .as_installed()
                    .or_not_found(&package_id)?
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
                    .as_idx_mut(&package_id)
                    .or_not_found(&package_id)?
                    .as_installed_mut()
                    .or_not_found(&package_id)?
                    .as_status_mut()
                    .as_main_mut()
                    .ser(&main)
            })
            .await?;
        Ok(json!(()))
    }
}
