use std::collections::BTreeSet;
use std::net::SocketAddr;

use imbl_value::InternedString;
use serde::{Deserialize, Serialize};
use ts_rs::TS;

use crate::prelude::*;
use crate::{ActionId, GatewayId, HostId, PackageId, ServiceInterfaceId};

#[derive(Clone, Debug, PartialEq, Eq, PartialOrd, Ord, Deserialize, Serialize, TS)]
#[ts(export)]
#[serde(rename_all = "camelCase")]
pub struct HostnameInfo {
    pub ssl: bool,
    pub public: bool,
    pub hostname: InternedString,
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
        package_id: PackageId,
        row_actions: Vec<ActionId>,
        #[ts(type = "unknown")]
        #[serde(default)]
        info: Value,
    },
}

impl HostnameInfo {
    pub fn to_socket_addr(&self) -> Option<SocketAddr> {
        let ip = self.hostname.parse().ok()?;
        Some(SocketAddr::new(ip, self.port?))
    }

    pub fn to_san_hostname(&self) -> InternedString {
        self.hostname.clone()
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
            Self::PrivateDomain { gateways } | Self::Mdns { gateways } => Box::new(gateways.iter()),
            Self::Plugin { .. } => Box::new(std::iter::empty()),
        }
    }
}

#[derive(Clone, Debug, PartialEq, Eq, PartialOrd, Ord, Deserialize, Serialize, TS)]
#[ts(export)]
#[serde(rename_all = "camelCase")]
pub struct PluginHostnameInfo {
    pub package_id: Option<PackageId>,
    pub host_id: HostId,
    pub internal_port: u16,
    pub ssl: bool,
    pub public: bool,
    #[ts(type = "string")]
    pub hostname: InternedString,
    pub port: Option<u16>,
    #[ts(type = "unknown")]
    #[serde(default)]
    pub info: Value,
}

impl PluginHostnameInfo {
    /// Convert to a `HostnameInfo` with `Plugin` metadata, using the given plugin package ID.
    pub fn to_hostname_info(
        &self,
        plugin_package: &PackageId,
        row_actions: Vec<ActionId>,
    ) -> HostnameInfo {
        HostnameInfo {
            ssl: self.ssl,
            public: self.public,
            hostname: self.hostname.clone(),
            port: self.port,
            metadata: HostnameMetadata::Plugin {
                package_id: plugin_package.clone(),
                info: self.info.clone(),
                row_actions,
            },
        }
    }

    /// Check if a `HostnameInfo` with Plugin metadata matches this `PluginHostnameInfo`
    /// (comparing address fields only, not row_actions).
    pub fn matches_hostname_info(&self, h: &HostnameInfo, plugin_package: &PackageId) -> bool {
        match &h.metadata {
            HostnameMetadata::Plugin { package_id, info, .. } => {
                package_id == plugin_package
                    && h.ssl == self.ssl
                    && h.public == self.public
                    && h.hostname == self.hostname
                    && h.port == self.port
                    && *info == self.info
            }
            _ => false,
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
