use std::collections::{BTreeMap, BTreeSet};

use imbl_value::InternedString;
use models::HostId;
use serde::{Deserialize, Serialize};

use crate::net::forward::AvailablePorts;
use crate::net::host::address::HostAddress;
use crate::net::host::binding::{BindInfo, BindOptions};
use crate::prelude::*;

pub mod address;
pub mod binding;

#[derive(Debug, Deserialize, Serialize, HasModel)]
#[serde(rename_all = "camelCase")]
#[model = "Model<Self>"]
pub struct Host {
    pub kind: HostKind,
    pub bindings: BTreeMap<u16, BindInfo>,
    pub addresses: BTreeSet<HostAddress>,
    pub primary: Option<HostAddress>,
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
            primary: None,
        }
    }
}

#[derive(Debug, Deserialize, Serialize)]
#[serde(rename_all = "camelCase")]
pub enum HostKind {
    Multi,
    // Single,
    // Static,
}

#[derive(Debug, Deserialize, Serialize, HasModel)]
#[model = "Model<Self>"]
pub struct HostInfo(BTreeMap<HostId, Host>);

impl Map for HostInfo {
    type Key = HostId;
    type Value = Host;
    fn key_str(key: &Self::Key) -> Result<impl AsRef<str>, Error> {
        Ok(key)
    }
    fn key_string(key: &Self::Key) -> Result<InternedString, Error> {
        Ok(key.clone().into())
    }
}

impl Model<HostInfo> {
    pub fn add_binding(
        &mut self,
        available_ports: &mut AvailablePorts,
        kind: HostKind,
        id: &HostId,
        internal_port: u16,
        options: BindOptions,
    ) -> Result<(), Error> {
        self.upsert(id, || Host::new(kind))?
            .as_bindings_mut()
            .mutate(|b| {
                let mut info = if let Some(info) = b.remove(&internal_port) {
                    info.update(available_ports, options)?
                } else {
                    BindInfo::new(available_ports, options)?
                };
                b.insert(internal_port, info);
                Ok(())
            }) // TODO: handle host kind change
    }
}
