use std::collections::BTreeMap;

use imbl_value::InternedString;

use crate::net::host::Hosts;
use crate::net::service_interface::{
    AddressInfo, RangeServiceInterface, ServiceInterface, ServiceInterfaceType,
};
use crate::service::effects::callbacks::CallbackHandler;
use crate::service::effects::prelude::*;
use crate::service::rpc::CallbackId;
use crate::{HostId, PackageId, ServiceInterfaceId};

// Every service interface lives under the binding it was exported from
// (`hosts/{hostId}/bindings/{internalPort}/interfaces/{id}` for single-port
// `Origin.export`, `hosts/{hostId}/bindingRanges/{internalStartPort}/interface`
// for `RangeOrigin.export`). The flat `PackageDataEntry.serviceInterfaces` map
// is gone — these effects read/write the host tree directly.
#[derive(Debug, Clone, Serialize, Deserialize, TS)]
#[ts(export)]
#[serde(rename_all = "camelCase")]
pub struct ExportServiceInterfaceParams {
    id: ServiceInterfaceId,
    name: String,
    description: String,
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
        masked,
        address_info,
        r#type,
    }: ExportServiceInterfaceParams,
) -> Result<(), Error> {
    let context = context.deref()?;
    let package_id = context.seed.id.clone();

    let host_id = address_info.host_id.clone();
    let internal_port = address_info.internal_port;
    let service_interface = ServiceInterface {
        id: id.clone(),
        name,
        description,
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
                .as_hosts_mut()
                .as_idx_mut(&host_id)
                .or_not_found(&host_id)?
                .as_bindings_mut()
                .as_idx_mut(&internal_port)
                .or_not_found(internal_port)?
                .as_interfaces_mut()
                .insert(&id, &service_interface)?;
            Ok(())
        })
        .await
        .result?;

    Ok(())
}

#[derive(Debug, Clone, Serialize, Deserialize, TS)]
#[ts(export)]
#[serde(rename_all = "camelCase")]
pub struct ExportRangeServiceInterfaceParams {
    host_id: HostId,
    internal_start_port: u16,
    id: ServiceInterfaceId,
    name: String,
    description: String,
    #[ts(type = "string | null")]
    scheme: Option<InternedString>,
}
pub async fn export_range_service_interface(
    context: EffectContext,
    ExportRangeServiceInterfaceParams {
        host_id,
        internal_start_port,
        id,
        name,
        description,
        scheme,
    }: ExportRangeServiceInterfaceParams,
) -> Result<(), Error> {
    let context = context.deref()?;
    let package_id = context.seed.id.clone();

    let interface = RangeServiceInterface {
        id,
        name,
        description,
        scheme,
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
                .as_hosts_mut()
                .as_idx_mut(&host_id)
                .or_not_found(&host_id)?
                .as_binding_ranges_mut()
                .as_idx_mut(&internal_start_port)
                .or_not_found(internal_start_port)?
                .as_interface_mut()
                .ser(&Some(interface))
        })
        .await
        .result?;

    Ok(())
}

/// Single-port service interface lookup, scanning every binding of every host
/// for `service_interface_id`. Range interfaces are intentionally excluded —
/// they have no addressable `AddressInfo` and are read off the host model.
fn find_service_interface(hosts: &Hosts, id: &ServiceInterfaceId) -> Option<ServiceInterface> {
    hosts
        .0
        .values()
        .flat_map(|host| host.bindings.values())
        .find_map(|bind| bind.interfaces.get(id).cloned())
}

fn list_all_service_interfaces(hosts: &Hosts) -> BTreeMap<ServiceInterfaceId, ServiceInterface> {
    hosts
        .0
        .values()
        .flat_map(|host| host.bindings.values())
        .flat_map(|bind| bind.interfaces.iter())
        .map(|(id, iface)| (id.clone(), iface.clone()))
        .collect()
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

    let ptr = format!("/public/packageData/{}/hosts", package_id)
        .parse()
        .expect("valid json pointer");
    let mut watch = context.seed.ctx.db.watch(ptr).await.typed::<Hosts>();

    let res = watch
        .peek_and_mark_seen()?
        .de()
        .ok()
        .and_then(|hosts: Hosts| find_service_interface(&hosts, &service_interface_id));

    if let Some(callback) = callback {
        let callback = callback.register(&context.seed.persistent_container);
        context.seed.ctx.callbacks.add_get_service_interface(
            package_id.clone(),
            service_interface_id.clone(),
            watch,
            CallbackHandler::new(&context, callback),
        );
    }

    Ok(res)
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

    let ptr = format!("/public/packageData/{}/hosts", package_id)
        .parse()
        .expect("valid json pointer");
    let mut watch = context.seed.ctx.db.watch(ptr).await.typed::<Hosts>();

    let res = watch
        .peek_and_mark_seen()?
        .de()
        .ok()
        .map(|hosts: Hosts| list_all_service_interfaces(&hosts))
        .unwrap_or_default();

    if let Some(callback) = callback {
        let callback = callback.register(&context.seed.persistent_container);
        context.seed.ctx.callbacks.add_list_service_interfaces(
            package_id.clone(),
            watch,
            CallbackHandler::new(&context, callback),
        );
    }

    Ok(res)
}

#[derive(Debug, Clone, Serialize, Deserialize, TS, Parser)]
#[group(skip)]
#[ts(export)]
#[serde(rename_all = "camelCase")]
pub struct ClearServiceInterfacesParams {
    pub except: Vec<ServiceInterfaceId>,
}

pub async fn clear_service_interfaces(
    context: EffectContext,
    ClearServiceInterfacesParams { except }: ClearServiceInterfacesParams,
) -> Result<(), Error> {
    let context = context.deref()?;
    let package_id = context.seed.id.clone();

    context
        .seed
        .ctx
        .db
        .mutate(|db| {
            for (_, host) in db
                .as_public_mut()
                .as_package_data_mut()
                .as_idx_mut(&package_id)
                .or_not_found(&package_id)?
                .as_hosts_mut()
                .as_entries_mut()?
            {
                for (_, bind) in host.as_bindings_mut().as_entries_mut()? {
                    bind.as_interfaces_mut()
                        .mutate(|ifaces| Ok(ifaces.retain(|id, _| except.contains(id))))?;
                }
                for (_, range) in host.as_binding_ranges_mut().as_entries_mut()? {
                    range.as_interface_mut().mutate(|iface| {
                        if iface.as_ref().map_or(false, |i| !except.contains(&i.id)) {
                            *iface = None;
                        }
                        Ok(())
                    })?;
                }
            }
            Ok(())
        })
        .await
        .result?;

    Ok(())
}
