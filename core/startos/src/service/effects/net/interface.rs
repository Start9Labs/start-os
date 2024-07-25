use std::collections::BTreeMap;

use imbl::vector;
use models::{PackageId, ServiceInterfaceId};

use crate::net::service_interface::{AddressInfo, ServiceInterface, ServiceInterfaceType};
use crate::service::effects::callbacks::CallbackHandler;
use crate::service::effects::prelude::*;
use crate::service::rpc::CallbackId;

#[derive(Debug, Clone, Serialize, Deserialize, TS)]
#[ts(export)]
#[serde(rename_all = "camelCase")]
pub struct ExportServiceInterfaceParams {
    id: ServiceInterfaceId,
    name: String,
    description: String,
    has_primary: bool,
    disabled: bool,
    masked: bool,
    address_info: AddressInfo,
    r#type: ServiceInterfaceType,
}
pub async fn export_service_interface(
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

    context
        .seed
        .ctx
        .db
        .mutate(|db| {
            db.as_public_mut()
                .as_package_data_mut()
                .as_idx_mut(&package_id)
                .or_not_found(&package_id)?
                .as_service_interfaces_mut()
                .insert(&id, &service_interface)?;
            Ok(())
        })
        .await?;
    if let Some(callbacks) = context
        .seed
        .ctx
        .callbacks
        .get_service_interface(&(package_id.clone(), id))
    {
        callbacks.call(vector![]).await?;
    }
    if let Some(callbacks) = context
        .seed
        .ctx
        .callbacks
        .list_service_interfaces(&package_id)
    {
        callbacks.call(vector![]).await?;
    }

    Ok(())
}

#[derive(Debug, Clone, serde::Serialize, serde::Deserialize, TS)]
#[serde(rename_all = "camelCase")]
#[ts(export)]
pub struct GetServiceInterfaceParams {
    #[ts(optional)]
    package_id: Option<PackageId>,
    service_interface_id: ServiceInterfaceId,
    #[ts(optional)]
    callback: Option<CallbackId>,
}
pub async fn get_service_interface(
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

#[derive(Debug, Clone, Serialize, Deserialize, TS)]
#[ts(export)]
#[serde(rename_all = "camelCase")]
pub struct ListServiceInterfacesParams {
    #[ts(optional)]
    package_id: Option<PackageId>,
    #[ts(optional)]
    callback: Option<CallbackId>,
}
pub async fn list_service_interfaces(
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

pub async fn clear_service_interfaces(context: EffectContext) -> Result<(), Error> {
    let context = context.deref()?;
    let package_id = context.seed.id.clone();

    context
        .seed
        .ctx
        .db
        .mutate(|db| {
            db.as_public_mut()
                .as_package_data_mut()
                .as_idx_mut(&package_id)
                .or_not_found(&package_id)?
                .as_service_interfaces_mut()
                .ser(&Default::default())
        })
        .await
}
