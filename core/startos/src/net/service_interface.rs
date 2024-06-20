use std::net::{Ipv4Addr, Ipv6Addr};

use imbl_value::InternedString;
use models::{HostId, ServiceInterfaceId};
use serde::{Deserialize, Serialize};
use ts_rs::TS;

use crate::net::host::address::HostAddress;

#[derive(Clone, Debug, Deserialize, Serialize, TS)]
#[ts(export)]
#[serde(rename_all = "camelCase")]
#[serde(rename_all_fields = "camelCase")]
#[serde(tag = "kind")]
pub enum HostnameInfo {
    Ip {
        network_interface_id: String,
        public: bool,
        hostname: IpHostname,
    },
    Onion {
        hostname: OnionHostname,
    },
}

#[derive(Clone, Debug, Deserialize, Serialize, TS)]
#[ts(export)]
#[serde(rename_all = "camelCase")]
pub struct OnionHostname {
    pub value: String,
    pub port: Option<u16>,
    pub ssl_port: Option<u16>,
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
        port: Option<u16>,
        ssl_port: Option<u16>,
    },
    Local {
        value: String,
        port: Option<u16>,
        ssl_port: Option<u16>,
    },
    Domain {
        domain: String,
        subdomain: Option<String>,
        port: Option<u16>,
        ssl_port: Option<u16>,
    },
}

#[derive(Clone, Debug, Deserialize, Serialize, TS)]
#[ts(export)]
#[serde(rename_all = "camelCase")]
pub struct ServiceInterface {
    pub id: ServiceInterfaceId,
    pub name: String,
    pub description: String,
    pub has_primary: bool,
    pub disabled: bool,
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
