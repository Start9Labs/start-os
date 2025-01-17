use std::collections::{BTreeMap, BTreeSet};
use std::future::Future;
use std::panic::RefUnwindSafe;

use clap::Parser;
use imbl_value::InternedString;
use models::{HostId, PackageId};
use rpc_toolkit::{from_fn_async, Context, Empty, HandlerExt, OrEmpty, ParentHandler};
use serde::{Deserialize, Serialize};
use torut::onion::OnionAddressV3;
use ts_rs::TS;

use crate::context::RpcContext;
use crate::db::model::DatabaseModel;
use crate::net::forward::AvailablePorts;
use crate::net::host::address::{address_api, DomainConfig, HostAddress};
use crate::net::host::binding::{binding, BindInfo, BindOptions};
use crate::net::service_interface::HostnameInfo;
use crate::prelude::*;

pub mod address;
pub mod binding;

#[derive(Debug, Default, Deserialize, Serialize, HasModel, TS)]
#[serde(rename_all = "camelCase")]
#[model = "Model<Self>"]
#[ts(export)]
pub struct Host {
    pub bindings: BTreeMap<u16, BindInfo>,
    #[ts(type = "string[]")]
    pub onions: BTreeSet<OnionAddressV3>,
    #[ts(as = "BTreeMap::<String, DomainConfig>")]
    pub domains: BTreeMap<InternedString, DomainConfig>,
    /// COMPUTED: NetService::update
    pub hostname_info: BTreeMap<u16, Vec<HostnameInfo>>, // internal port -> Hostnames
}
impl AsRef<Host> for Host {
    fn as_ref(&self) -> &Host {
        self
    }
}
impl Host {
    pub fn new() -> Self {
        Self::default()
    }
    pub fn addresses<'a>(&'a self) -> impl Iterator<Item = HostAddress> + 'a {
        self.onions
            .iter()
            .cloned()
            .map(|address| HostAddress::Onion { address })
            .chain(
                self.domains
                    .iter()
                    .map(
                        |(address, DomainConfig { public, acme })| HostAddress::Domain {
                            address: address.clone(),
                            public: *public,
                            acme: acme.clone(),
                        },
                    ),
            )
    }
}

#[derive(Debug, Default, Deserialize, Serialize, HasModel, TS)]
#[model = "Model<Self>"]
#[ts(export)]
pub struct Hosts(pub BTreeMap<HostId, Host>);

impl Map for Hosts {
    type Key = HostId;
    type Value = Host;
    fn key_str(key: &Self::Key) -> Result<impl AsRef<str>, Error> {
        Ok(key)
    }
    fn key_string(key: &Self::Key) -> Result<InternedString, Error> {
        Ok(key.clone().into())
    }
}

pub fn host_for<'a>(
    db: &'a mut DatabaseModel,
    package_id: Option<&PackageId>,
    host_id: &HostId,
) -> Result<&'a mut Model<Host>, Error> {
    let Some(package_id) = package_id else {
        return Ok(db.as_public_mut().as_server_info_mut().as_host_mut());
    };
    fn host_info<'a>(
        db: &'a mut DatabaseModel,
        package_id: &PackageId,
    ) -> Result<&'a mut Model<Hosts>, Error> {
        Ok::<_, Error>(
            db.as_public_mut()
                .as_package_data_mut()
                .as_idx_mut(package_id)
                .or_not_found(package_id)?
                .as_hosts_mut(),
        )
    }
    let tor_key = if host_info(db, package_id)?.as_idx(host_id).is_none() {
        Some(
            db.as_private_mut()
                .as_key_store_mut()
                .as_onion_mut()
                .new_key()?,
        )
    } else {
        None
    };
    host_info(db, package_id)?.upsert(host_id, || {
        let mut h = Host::new();
        h.onions.insert(
            tor_key
                .or_not_found("generated tor key")?
                .public()
                .get_onion_address(),
        );
        Ok(h)
    })
}

impl Model<Host> {
    pub fn add_binding(
        &mut self,
        available_ports: &mut AvailablePorts,
        internal_port: u16,
        options: BindOptions,
    ) -> Result<(), Error> {
        self.as_bindings_mut().mutate(|b| {
            let info = if let Some(info) = b.remove(&internal_port) {
                info.update(available_ports, options)?
            } else {
                BindInfo::new(available_ports, options)?
            };
            b.insert(internal_port, info);
            Ok(())
        })
    }
}

#[derive(Deserialize, Serialize, Parser)]
pub struct RequiresPackageId {
    package: PackageId,
}

#[derive(Deserialize, Serialize, Parser)]
pub struct RequiresHostId {
    host: HostId,
}

pub trait HostApiKind: 'static {
    type Params: Send + Sync + 'static;
    type InheritedParams: Send + Sync + 'static;
    type Inheritance: RefUnwindSafe + OrEmpty<Self::Inheritance> + Send + Sync + 'static;
    fn inheritance(params: Self::Params, inherited: Self::InheritedParams) -> Self::Inheritance;
    fn host_for<'a>(
        inheritance: &Self::Inheritance,
        db: &'a mut DatabaseModel,
    ) -> Result<&'a mut Model<Host>, Error>;
    fn update_host(
        ctx: &RpcContext,
        inheritance: Self::Inheritance,
    ) -> impl Future<Output = Result<(), Error>> + Send;
}
pub struct ForPackage;
impl HostApiKind for ForPackage {
    type Params = RequiresHostId;
    type InheritedParams = PackageId;
    type Inheritance = (PackageId, HostId);
    fn inheritance(
        RequiresHostId { host }: Self::Params,
        package: Self::InheritedParams,
    ) -> Self::Inheritance {
        (package, host)
    }
    fn host_for<'a>(
        (package, host): &Self::Inheritance,
        db: &'a mut DatabaseModel,
    ) -> Result<&'a mut Model<Host>, Error> {
        host_for(db, Some(package), host)
    }
    async fn update_host(
        ctx: &RpcContext,
        (package, host): Self::Inheritance,
    ) -> Result<(), Error> {
        let service = ctx.services.get(&package).await;
        let service_ref = service.as_ref().or_not_found(&package)?;
        service_ref.update_host(host).await?;
        Ok(())
    }
}
pub struct ForServer;
impl HostApiKind for ForServer {
    type Params = Empty;
    type InheritedParams = Empty;
    type Inheritance = Empty;
    fn inheritance(_: Self::Params, _: Self::InheritedParams) -> Self::Inheritance {
        Empty {}
    }
    fn host_for<'a>(
        _: &Self::Inheritance,
        db: &'a mut DatabaseModel,
    ) -> Result<&'a mut Model<Host>, Error> {
        host_for(db, None, &HostId::default())
    }
    async fn update_host(ctx: &RpcContext, _: Self::Inheritance) -> Result<(), Error> {
        ctx.os_net_service
            .lock()
            .await
            .update(
                HostId::default(),
                ctx.db
                    .peek()
                    .await
                    .as_public()
                    .as_server_info()
                    .as_host()
                    .de()?,
            )
            .await
    }
}

pub fn host_api<C: Context>() -> ParentHandler<C, RequiresPackageId> {
    ParentHandler::<C, RequiresPackageId>::new()
        .subcommand(
            "list",
            from_fn_async(list_hosts)
                .with_inherited(|RequiresPackageId { package }, _| package)
                .with_custom_display_fn(|_, ids| {
                    for id in ids {
                        println!("{id}")
                    }
                    Ok(())
                })
                .with_about("List host IDs available for this service"),
        )
        .subcommand(
            "address",
            address_api::<C, ForPackage>()
                .with_inherited(|RequiresPackageId { package }, _| package),
        )
        .subcommand(
            "binding",
            binding::<C, ForPackage>().with_inherited(|RequiresPackageId { package }, _| package),
        )
}

pub fn server_host_api<C: Context>() -> ParentHandler<C> {
    ParentHandler::<C>::new()
        .subcommand("address", address_api::<C, ForServer>())
        .subcommand("binding", binding::<C, ForServer>())
}

pub async fn list_hosts(
    ctx: RpcContext,
    _: Empty,
    package: PackageId,
) -> Result<Vec<HostId>, Error> {
    ctx.db
        .peek()
        .await
        .into_public()
        .into_package_data()
        .into_idx(&package)
        .or_not_found(&package)?
        .into_hosts()
        .keys()
}
