use std::collections::{BTreeMap, BTreeSet};

use clap::Parser;
use imbl_value::InternedString;
use models::{HostId, PackageId};
use rpc_toolkit::{from_fn_async, Context, Empty, HandlerExt, ParentHandler};
use serde::{Deserialize, Serialize};
use torut::onion::OnionAddressV3;
use ts_rs::TS;

use crate::context::RpcContext;
use crate::db::model::DatabaseModel;
use crate::net::forward::AvailablePorts;
use crate::net::host::address::{address, DomainConfig, HostAddress};
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
pub struct HostParams {
    package: PackageId,
}

pub fn host<C: Context>() -> ParentHandler<C, HostParams> {
    ParentHandler::<C, HostParams>::new()
        .subcommand(
            "list",
            from_fn_async(list_hosts)
                .with_inherited(|HostParams { package }, _| package)
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
            address::<C>().with_inherited(|HostParams { package }, _| package),
        )
        .subcommand(
            "binding",
            binding::<C>().with_inherited(|HostParams { package }, _| package),
        )
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
