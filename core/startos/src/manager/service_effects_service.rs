use crate::util::Version;
use futures::{future::BoxFuture, FutureExt};
use imbl_value::json;
use models::{ActionId, PackageId};

use crate::{
    action::action, context::RpcContext, prelude::*, status::MainStatus, util::serde::IoFormat,
};

struct ServiceEffectsService {
    package_id: PackageId,
    version: Version,
}

#[derive(Debug, Clone, serde::Serialize, serde::Deserialize)]
struct RpcData {
    id: i64,
    method: String,
    params: Value,
}

trait RpcMethod {
    fn run_effect(
        &self,
        context: RpcContext,
        package_id: &PackageId,
        version: &Version,
    ) -> BoxFuture<'static, Value>;
}

impl ServiceEffectsService {
    pub fn new(package_id: PackageId, version: Version) -> Self {
        Self {
            package_id,
            version,
        }
    }

    pub async fn run_effects(
        &self,
        context: RpcContext,
        RpcData { id, method, params }: RpcData,
    ) -> Value {
        let error_param = params.clone();
        let Ok(effect) = (match method.as_str() {
            "exists" => from_value::<Exists>(params)
                .map(Box::new)
                .map(|x| x as Box<dyn RpcMethod>),
            "executeAction" => from_value::<ExecuteAction>(params)
                .map(Box::new)
                .map(|x| x as Box<dyn RpcMethod>),
            "getConfigured" => from_value::<GetConfigured>(params)
                .map(Box::new)
                .map(|x| x as Box<dyn RpcMethod>),
            "stopped" => from_value::<Stopped>(params)
                .map(Box::new)
                .map(|x| x as Box<dyn RpcMethod>),
            "running" => from_value::<Running>(params)
                .map(Box::new)
                .map(|x| x as Box<dyn RpcMethod>),
            "restart" => from_value::<Restart>(params)
                .map(Box::new)
                .map(|x| x as Box<dyn RpcMethod>),
            "shutdown" => from_value::<Shutdown>(params)
                .map(Box::new)
                .map(|x| x as Box<dyn RpcMethod>),
            "setConfigured" => from_value::<SetConfigured>(params)
                .map(Box::new)
                .map(|x| x as Box<dyn RpcMethod>),
            // TODO BLUJ Later
            "setDependencies" => todo!(),
            "setHealth" => todo!(),
            "getStore" => todo!(),
            "setStore" => todo!(),
            "embassyGetInterface" => todo!(),
            "mount" => todo!(),
            "getSslCertificate" => todo!(),
            "getSslKey" => todo!(),
            "removeAction" => todo!(),
            "removeAddress" => todo!(),
            "exportAction" => todo!(),
            "bind" => todo!(),
            "clearNetworkInterfaces" => todo!(),
            "exportNetworkInterface" => todo!(),
            "exposeForDependents" => todo!(),
            "exposeUi" => todo!(),
            "clearBindings" => todo!(),
            "getHostnames" => todo!(),
            "getInterface" => todo!(),
            "listInterface" => todo!(),
            "getIPHostname" => todo!(),
            "getContainerIp" => todo!(),
            "getLocalHostname" => todo!(),
            "getPrimaryUrl" => todo!(),
            "getServicePortForward" => todo!(),
            "getServiceTorHostname" => todo!(),
            "getSystemSmtp" => todo!(),
            "reverseProxy" => todo!(),
            _ => todo!(),
        }) else {
            return json!({"error": format!("Invalid Params for {method}: {:?}", error_param)});
        };
        effect
            .run_effect(context, &self.package_id, &self.version)
            .await
    }
}

// .then(|x| x.unwrap_or_else(|e| json!({"error": e.to_string()})))

#[derive(Debug, Clone, serde::Serialize, serde::Deserialize)]
struct Exists {
    package: PackageId,
}

impl RpcMethod for Exists {
    fn run_effect(
        &self,
        context: RpcContext,
        package_id: &PackageId,
        version: &Version,
    ) -> BoxFuture<'static, Value> {
        Box::pin(async move {
            let peeked = context.db.peek().await;
            let package = peeked.as_package_data().as_idx(&self.package).is_some();
            json!(package)
        })
    }
}
#[derive(Debug, Clone, serde::Serialize, serde::Deserialize)]
#[serde(rename_all = "camelCase")]
struct ExecuteAction {
    service_id: Option<PackageId>,
    action_id: ActionId,
    input: Value,
}

impl RpcMethod for ExecuteAction {
    fn run_effect(
        &self,
        context: RpcContext,
        package_id: &PackageId,
        version: &Version,
    ) -> BoxFuture<'static, Value> {
        Box::pin(
            action(
                context,
                self.service_id.unwrap_or_else(|| package_id.clone()),
                self.action_id,
                from_value(self.input).ok(),
                Some(IoFormat::Json),
            )
            .map(|x| x.and_then(|x| imbl_value::to_value(&x).with_kind(ErrorKind::Serialization)))
            .map(|x| x.unwrap_or_else(|e| json!({"error": e.to_string()}))),
        )
    }
}
#[derive(Debug, Clone, serde::Serialize, serde::Deserialize)]
#[serde(rename_all = "camelCase")]
struct GetConfigured {}

impl RpcMethod for GetConfigured {
    fn run_effect(
        &self,
        context: RpcContext,
        package_id: &PackageId,
        version: &Version,
    ) -> BoxFuture<'static, Value> {
        Box::pin(
            async move {
                let peeked = context.db.peek().await;
                let package = peeked
                    .as_package_data()
                    .as_idx(package_id)
                    .or_not_found(package_id)
                    .ok()?
                    .as_installed()
                    .or_not_found(package_id)
                    .ok()?
                    .as_status()
                    .as_configured()
                    .de()
                    .ok()?;
                Some(package)
            }
            .map(|x| json!(x.unwrap_or(false))),
        )
    }
}
#[derive(Debug, Clone, serde::Serialize, serde::Deserialize)]
#[serde(rename_all = "camelCase")]
struct Stopped {
    package_id: Option<PackageId>,
}

impl RpcMethod for Stopped {
    fn run_effect(
        &self,
        context: RpcContext,
        package_id: &PackageId,
        version: &Version,
    ) -> BoxFuture<'static, Value> {
        Box::pin(
            async move {
                let peeked = context.db.peek().await;
                let package_id = self.package_id.as_ref().unwrap_or(package_id);
                let package = peeked
                    .as_package_data()
                    .as_idx(package_id)
                    .or_not_found(package_id)
                    .ok()?
                    .as_installed()
                    .or_not_found(package_id)
                    .ok()?
                    .as_status()
                    .as_main()
                    .de()
                    .ok()?;
                Some(matches!(MainStatus::Stopped, package))
            }
            .map(|x| json!(x.unwrap_or(false))),
        )
    }
}
#[derive(Debug, Clone, serde::Serialize, serde::Deserialize)]
#[serde(rename_all = "camelCase")]
struct Running {
    package_id: Option<PackageId>,
}

impl RpcMethod for Running {
    fn run_effect(
        &self,
        context: RpcContext,
        package_id: &PackageId,
        version: &Version,
    ) -> BoxFuture<'static, Value> {
        Box::pin(
            async move {
                let peeked = context.db.peek().await;
                let package_id = self.package_id.as_ref().unwrap_or(package_id);
                let package = peeked
                    .as_package_data()
                    .as_idx(package_id)
                    .or_not_found(package_id)
                    .ok()?
                    .as_installed()
                    .or_not_found(package_id)
                    .ok()?
                    .as_status()
                    .as_main()
                    .de()
                    .ok()?;
                Some(match package {
                    MainStatus::Running { .. } => true,
                    _ => false,
                })
            }
            .map(|x| json!(x.unwrap_or(false))),
        )
    }
}
#[derive(Debug, Clone, serde::Serialize, serde::Deserialize)]
#[serde(rename_all = "camelCase")]
struct Restart {}

impl RpcMethod for Restart {
    fn run_effect(
        &self,
        context: RpcContext,
        package_id: &PackageId,
        version: &Version,
    ) -> BoxFuture<'static, Value> {
        Box::pin(
            async move {
                let manager = context
                    .managers
                    .get(&(package_id.clone(), version.clone()))
                    .await?;
                manager.restart().await;
                Some(())
            }
            .map(|x| json!(())),
        )
    }
}
#[derive(Debug, Clone, serde::Serialize, serde::Deserialize)]
#[serde(rename_all = "camelCase")]
struct Shutdown {}

impl RpcMethod for Shutdown {
    fn run_effect(
        &self,
        context: RpcContext,
        package_id: &PackageId,
        version: &Version,
    ) -> BoxFuture<'static, Value> {
        Box::pin(
            async move {
                let manager = context
                    .managers
                    .get(&(package_id.clone(), version.clone()))
                    .await?;
                manager.stop().await;
                Some(())
            }
            .map(|_| json!(())),
        )
    }
}

#[derive(Debug, Clone, serde::Serialize, serde::Deserialize)]
#[serde(rename_all = "camelCase")]
struct SetConfigured {
    configured: bool,
}

impl RpcMethod for SetConfigured {
    fn run_effect(
        &self,
        context: RpcContext,
        package_id: &PackageId,
        version: &Version,
    ) -> BoxFuture<'static, Value> {
        Box::pin(
            async move {
                context
                    .db
                    .mutate(|db| {
                        db.as_package_data()
                            .as_idx(package_id)
                            .or_not_found(package_id)?
                            .as_installed()
                            .or_not_found(package_id)?
                            .as_status()
                            .as_configured()
                            .ser(&self.configured)
                    })
                    .await
            }
            .map(|_| json!(())),
        )
    }
}
