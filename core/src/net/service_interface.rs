use std::collections::BTreeSet;
use std::net::SocketAddr;

use imbl_value::{InOMap, InternedString};
use serde::{Deserialize, Serialize};
use ts_rs::TS;

use crate::prelude::*;
use crate::{GatewayId, HostId, PackageId, ServiceInterfaceId};

#[derive(Clone, Debug, PartialEq, Eq, PartialOrd, Ord, Deserialize, Serialize, TS)]
#[ts(export)]
#[serde(rename_all = "camelCase")]
pub struct HostnameInfo {
    pub ssl: bool,
    pub public: bool,
    pub host: InternedString,
    pub port: Option<u16>,
    pub metadata: HostnameMetadata,
}

#[derive(Clone, Debug, PartialEq, Eq, PartialOrd, Ord, Deserialize, Serialize, TS)]
#[ts(export)]
#[serde(rename_all = "kebab-case")]
#[serde(rename_all_fields = "camelCase")]
#[serde(tag = "kind")]
pub enum HostnameMetadata {
    Ipv4 {
        gateway: GatewayId,
    },
    Ipv6 {
        gateway: GatewayId,
        scope_id: u32,
    },
    Mdns {
        gateways: BTreeSet<GatewayId>,
    },
    PrivateDomain {
        gateways: BTreeSet<GatewayId>,
    },
    PublicDomain {
        gateway: GatewayId,
    },
    Plugin {
        package: PackageId,
        #[serde(flatten)]
        #[ts(skip)]
        extra: InOMap<InternedString, Value>,
    },
}

impl HostnameInfo {
    pub fn to_socket_addr(&self) -> Option<SocketAddr> {
        let ip = self.host.parse().ok()?;
        Some(SocketAddr::new(ip, self.port?))
    }

    pub fn to_san_hostname(&self) -> InternedString {
        self.host.clone()
    }
}

impl HostnameMetadata {
    pub fn is_ip(&self) -> bool {
        matches!(self, Self::Ipv4 { .. } | Self::Ipv6 { .. })
    }

    pub fn gateways(&self) -> Box<dyn Iterator<Item = &GatewayId> + '_> {
        match self {
            Self::Ipv4 { gateway }
            | Self::Ipv6 { gateway, .. }
            | Self::PublicDomain { gateway } => Box::new(std::iter::once(gateway)),
            Self::PrivateDomain { gateways } | Self::Mdns { gateways } => {
                Box::new(gateways.iter())
            }
            Self::Plugin { .. } => Box::new(std::iter::empty()),
        }
    }
}

#[derive(Clone, Debug, PartialEq, Eq, PartialOrd, Ord, Deserialize, Serialize, TS)]
#[ts(export)]
#[serde(rename_all = "camelCase")]
pub struct GatewayInfo {
    pub id: GatewayId,
    pub name: InternedString,
    pub public: bool,
}

#[derive(Clone, Debug, Deserialize, Serialize, TS)]
#[ts(export)]
#[serde(rename_all = "camelCase")]
pub struct ServiceInterface {
    pub id: ServiceInterfaceId,
    pub name: String,
    pub description: String,
    pub masked: bool,
    pub address_info: AddressInfo,
    #[serde(rename = "type")]
    pub interface_type: ServiceInterfaceType,
}

#[derive(Clone, Debug, Deserialize, Serialize, TS)]
#[ts(export)]
#[serde(rename_all = "camelCase")]
pub enum ServiceInterfaceType {
    Ui,
    P2p,
    Api,
}

#[derive(Clone, Debug, Deserialize, Serialize, TS)]
#[ts(export)]
#[serde(rename_all = "camelCase")]
pub struct AddressInfo {
    pub username: Option<String>,
    pub host_id: HostId,
    pub internal_port: u16,
    #[ts(type = "string | null")]
    pub scheme: Option<InternedString>,
    #[ts(type = "string | null")]
    pub ssl_scheme: Option<InternedString>,
    pub suffix: String,
}
