use models::{HostId, PackageId};

use crate::net::host::binding::{BindId, BindOptions, NetInfo};
use crate::service::effects::prelude::*;

#[derive(Debug, Clone, Serialize, Deserialize, TS)]
#[serde(rename_all = "camelCase")]
#[ts(export)]
pub struct BindParams {
    id: HostId,
    internal_port: u16,
    #[serde(flatten)]
    options: BindOptions,
}
pub async fn bind(
    context: EffectContext,
    BindParams {
        id,
        internal_port,
        options,
    }: BindParams,
) -> Result<(), Error> {
    let context = context.deref()?;
    context
        .seed
        .persistent_container
        .net_service
        .bind(id, internal_port, options)
        .await
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
    context
        .seed
        .persistent_container
        .net_service
        .clear_bindings(except.into_iter().collect())
        .await?;
    Ok(())
}

#[derive(Debug, Clone, Serialize, Deserialize, TS)]
#[ts(export)]
#[serde(rename_all = "camelCase")]
pub struct GetServicePortForwardParams {
    #[ts(optional)]
    package_id: Option<PackageId>,
    host_id: HostId,
    internal_port: u16,
}
pub async fn get_service_port_forward(
    context: EffectContext,
    GetServicePortForwardParams {
        package_id,
        host_id,
        internal_port,
    }: GetServicePortForwardParams,
) -> Result<NetInfo, Error> {
    let context = context.deref()?;

    let package_id = package_id.unwrap_or_else(|| context.seed.id.clone());

    Ok(context
        .seed
        .ctx
        .db
        .peek()
        .await
        .as_public()
        .as_package_data()
        .as_idx(&package_id)
        .or_not_found(&package_id)?
        .as_hosts()
        .as_idx(&host_id)
        .or_not_found(&host_id)?
        .as_bindings()
        .de()?
        .get(&internal_port)
        .or_not_found(lazy_format!("binding for port {internal_port}"))?
        .net)
}
