use models::{HostId, PackageId};

use crate::net::host::binding::{BindId, BindOptions, LanInfo};
use crate::net::host::HostKind;
use crate::service::effects::prelude::*;

#[derive(Debug, Clone, Serialize, Deserialize, TS)]
#[serde(rename_all = "camelCase")]
#[ts(export)]
pub struct BindParams {
    kind: HostKind,
    id: HostId,
    internal_port: u16,
    #[serde(flatten)]
    options: BindOptions,
}
pub async fn bind(
    context: EffectContext,
    BindParams {
        kind,
        id,
        internal_port,
        options,
    }: BindParams,
) -> Result<(), Error> {
    let context = context.deref()?;
    let mut svc = context.seed.persistent_container.net_service.lock().await;
    svc.bind(kind, id, internal_port, options).await
}

#[derive(Debug, Clone, Serialize, Deserialize, TS, Parser)]
#[ts(export)]
#[serde(rename_all = "camelCase")]
pub struct ClearBindingsParams {
    #[serde(default)]
    pub except: Vec<BindId>,
}

pub async fn clear_bindings(
    context: EffectContext,
    ClearBindingsParams { except }: ClearBindingsParams,
) -> Result<(), Error> {
    let context = context.deref()?;
    let mut svc = context.seed.persistent_container.net_service.lock().await;
    svc.clear_bindings(except.into_iter().collect()).await?;
    Ok(())
}

#[derive(Debug, Clone, Serialize, Deserialize, TS)]
#[ts(export)]
#[serde(rename_all = "camelCase")]
pub struct GetServicePortForwardParams {
    #[ts(optional)]
    package_id: Option<PackageId>,
    host_id: HostId,
    internal_port: u32,
}
pub async fn get_service_port_forward(
    context: EffectContext,
    data: GetServicePortForwardParams,
) -> Result<LanInfo, Error> {
    let internal_port = data.internal_port as u16;

    let context = context.deref()?;
    let net_service = context.seed.persistent_container.net_service.lock().await;
    net_service.get_lan_port(data.host_id, internal_port)
}
