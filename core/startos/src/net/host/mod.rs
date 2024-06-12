use std::collections::{BTreeMap, BTreeSet};

use imbl_value::InternedString;
use models::{HostId, PackageId};
use serde::{Deserialize, Serialize};
use ts_rs::TS;

use crate::db::model::DatabaseModel;
use crate::net::forward::AvailablePorts;
use crate::net::host::address::HostAddress;
use crate::net::host::binding::{BindInfo, BindOptions};
use crate::net::service_interface::HostnameInfo;
use crate::prelude::*;

pub mod address;
pub mod binding;

#[derive(Debug, Deserialize, Serialize, HasModel, TS)]
#[serde(rename_all = "camelCase")]
#[model = "Model<Self>"]
#[ts(export)]
pub struct Host {
    pub kind: HostKind,
    pub bindings: BTreeMap<u16, BindInfo>,
    pub addresses: BTreeSet<HostAddress>,
    /// COMPUTED: NetService::update
    pub hostname_info: BTreeMap<u16, Vec<HostnameInfo>>, // internal port -> Hostnames
}
impl AsRef<Host> for Host {
    fn as_ref(&self) -> &Host {
        self
    }
}
impl Host {
    pub fn new(kind: HostKind) -> Self {
        Self {
            kind,
            bindings: BTreeMap::new(),
            addresses: BTreeSet::new(),
            hostname_info: BTreeMap::new(),
        }
    }
}

#[derive(Clone, Copy, Debug, PartialEq, Eq, PartialOrd, Ord, Deserialize, Serialize, TS)]
#[serde(rename_all = "camelCase")]
#[ts(export)]
pub enum HostKind {
    Multi,
    // Single,
    // Static,
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
    package_id: &PackageId,
    host_id: &HostId,
    host_kind: HostKind,
) -> Result<&'a mut Model<Host>, Error> {
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
        let mut h = Host::new(host_kind);
        h.addresses.insert(HostAddress::Onion {
            address: tor_key
                .or_not_found("generated tor key")?
                .public()
                .get_onion_address(),
        });
        Ok(h)
    })
}

impl Model<Host> {
    pub fn set_kind(&mut self, kind: HostKind) -> Result<(), Error> {
        match (self.as_kind().de()?, kind) {
            (HostKind::Multi, HostKind::Multi) => Ok(()),
        }
    }
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
