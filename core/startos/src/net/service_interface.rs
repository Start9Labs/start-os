use std::net::{Ipv4Addr, Ipv6Addr};

use imbl_value::InternedString;
use lazy_format::lazy_format;
use models::{HostId, ServiceInterfaceId};
use serde::{Deserialize, Serialize};
use ts_rs::TS;

#[derive(Clone, Debug, Deserialize, Serialize, TS)]
#[ts(export)]
#[serde(rename_all = "camelCase")]
#[serde(rename_all_fields = "camelCase")]
#[serde(tag = "kind")]
pub enum HostnameInfo {
    Ip {
        #[ts(type = "string")]
        network_interface_id: InternedString,
        public: bool,
        hostname: IpHostname,
    },
    Onion {
        hostname: OnionHostname,
    },
}
impl HostnameInfo {
    pub fn to_san_hostname(&self) -> InternedString {
        match self {
            Self::Ip { hostname, .. } => hostname.to_san_hostname(),
            Self::Onion { hostname } => hostname.to_san_hostname(),
        }
    }
}

#[derive(Clone, Debug, Deserialize, Serialize, TS)]
#[ts(export)]
#[serde(rename_all = "camelCase")]
pub struct OnionHostname {
    #[ts(type = "string")]
    pub value: InternedString,
    pub port: Option<u16>,
    pub ssl_port: Option<u16>,
}
impl OnionHostname {
    pub fn to_san_hostname(&self) -> InternedString {
        self.value.clone()
    }
}

#[derive(Clone, Debug, Deserialize, Serialize, TS)]
#[ts(export)]
#[serde(rename_all = "camelCase")]
#[serde(rename_all_fields = "camelCase")]
#[serde(tag = "kind")]
pub enum IpHostname {
    Ipv4 {
        value: Ipv4Addr,
        port: Option<u16>,
        ssl_port: Option<u16>,
    },
    Ipv6 {
        value: Ipv6Addr,
        #[serde(default)]
        scope_id: u32,
        port: Option<u16>,
        ssl_port: Option<u16>,
    },
    Local {
        #[ts(type = "string")]
        value: InternedString,
        port: Option<u16>,
        ssl_port: Option<u16>,
    },
    Domain {
        #[ts(type = "string")]
        domain: InternedString,
        #[ts(type = "string | null")]
        subdomain: Option<InternedString>,
        port: Option<u16>,
        ssl_port: Option<u16>,
    },
}
impl IpHostname {
    pub fn to_san_hostname(&self) -> InternedString {
        match self {
            Self::Ipv4 { value, .. } => InternedString::from_display(value),
            Self::Ipv6 { value, .. } => InternedString::from_display(value),
            Self::Local { value, .. } => value.clone(),
            Self::Domain {
                domain, subdomain, ..
            } => {
                if let Some(subdomain) = subdomain {
                    InternedString::from_display(&lazy_format!("{subdomain}.{domain}"))
                } else {
                    domain.clone()
                }
            }
        }
    }
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
