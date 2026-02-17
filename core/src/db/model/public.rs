use std::collections::{BTreeMap, BTreeSet, VecDeque};
use std::net::{IpAddr, Ipv4Addr, SocketAddr};
use std::sync::{Arc, OnceLock};

use chrono::{DateTime, Utc};
use exver::{Version, VersionRange};
use imbl::{OrdMap, OrdSet};
use imbl_value::InternedString;
use ipnet::IpNet;
use isocountry::CountryCode;
use itertools::Itertools;
use openssl::hash::MessageDigest;
use patch_db::{HasModel, Value};
use serde::{Deserialize, Serialize};
use ts_rs::TS;
use url::Url;

use crate::account::AccountInfo;
use crate::db::DbAccessByKey;
use crate::db::model::Database;
use crate::db::model::package::AllPackageData;
use crate::net::acme::AcmeProvider;
use crate::net::host::Host;
use crate::net::host::binding::{
    AddSslOptions, BindInfo, BindOptions, Bindings, DerivedAddressInfo, NetInfo,
};
use crate::net::vhost::AlpnInfo;
use crate::prelude::*;
use crate::progress::FullProgress;
use crate::system::{KeyboardOptions, SmtpValue};
use crate::util::cpupower::Governor;
use crate::util::lshw::LshwDevice;
use crate::util::serde::MaybeUtf8String;
use crate::version::{Current, VersionT};
use crate::{ARCH, GatewayId, PLATFORM, PackageId};

pub static DB_UI_SEED_CELL: OnceLock<&'static str> = OnceLock::new();

#[derive(Debug, Deserialize, Serialize, HasModel, TS)]
#[serde(rename_all = "camelCase")]
#[model = "Model<Self>"]
#[ts(export)]
pub struct Public {
    pub server_info: ServerInfo,
    pub package_data: AllPackageData,
    #[ts(type = "unknown")]
    pub ui: Value,
}
impl Public {
    pub fn init(
        account: &AccountInfo,
        kiosk: Option<bool>,
        language: Option<InternedString>,
        keyboard: Option<KeyboardOptions>,
    ) -> Result<Self, Error> {
        Ok(Self {
            server_info: ServerInfo {
                arch: get_arch(),
                platform: get_platform(),
                id: account.server_id.clone(),
                version: Current::default().semver(),
                hostname: account.hostname.no_dot_host_name(),
                last_backup: None,
                package_version_compat: Current::default().compat().clone(),
                post_init_migration_todos: BTreeMap::new(),
                network: NetworkInfo {
                    host: Host {
                        bindings: Bindings(
                            [(
                                80,
                                BindInfo {
                                    enabled: false,
                                    options: BindOptions {
                                        preferred_external_port: 80,
                                        add_ssl: Some(AddSslOptions {
                                            preferred_external_port: 443,
                                            add_x_forwarded_headers: false,
                                            alpn: Some(AlpnInfo::Specified(vec![
                                                MaybeUtf8String("h2".into()),
                                                MaybeUtf8String("http/1.1".into()),
                                            ])),
                                        }),
                                        secure: None,
                                    },
                                    net: NetInfo {
                                        assigned_port: None,
                                        assigned_ssl_port: Some(443),
                                    },
                                    addresses: DerivedAddressInfo::default(),
                                },
                            )]
                            .into_iter()
                            .collect(),
                        ),
                        public_domains: BTreeMap::new(),
                        private_domains: BTreeMap::new(),
                        port_forwards: BTreeSet::new(),
                    },
                    wifi: WifiInfo {
                        enabled: true,
                        ..Default::default()
                    },
                    gateways: OrdMap::new(),
                    acme: {
                        let mut acme: BTreeMap<AcmeProvider, AcmeSettings> = Default::default();
                        acme.insert(
                            "letsencrypt".parse()?,
                            AcmeSettings {
                                contact: Vec::new(),
                            },
                        );
                        #[cfg(feature = "dev")]
                        acme.insert(
                            "letsencrypt-staging".parse()?,
                            AcmeSettings {
                                contact: Vec::new(),
                            },
                        );
                        acme
                    },
                    dns: Default::default(),
                    default_outbound: None,
                },
                status_info: ServerStatus {
                    backup_progress: None,
                    updated: false,
                    update_progress: None,
                    shutting_down: false,
                    restarting: false,
                },
                unread_notification_count: 0,
                password_hash: account.password.clone(),
                pubkey: ssh_key::PublicKey::from(&account.ssh_key)
                    .to_openssh()
                    .unwrap(),
                ca_fingerprint: account
                    .root_ca_cert
                    .digest(MessageDigest::sha256())
                    .unwrap()
                    .iter()
                    .map(|x| format!("{x:X}"))
                    .join(":"),
                ntp_synced: false,
                zram: true,
                governor: None,
                smtp: None,
                ifconfig_url: default_ifconfig_url(),
                ram: 0,
                devices: Vec::new(),
                kiosk,
                language,
                keyboard,
            },
            package_data: AllPackageData::default(),
            ui: serde_json::from_str(*DB_UI_SEED_CELL.get().unwrap_or(&"null"))
                .with_kind(ErrorKind::Deserialization)?,
        })
    }
}

fn get_arch() -> InternedString {
    (*ARCH).into()
}

fn get_platform() -> InternedString {
    (&*PLATFORM).into()
}

pub fn default_ifconfig_url() -> Url {
    "https://ifconfig.co".parse().unwrap()
}

#[derive(Debug, Deserialize, Serialize, HasModel, TS)]
#[serde(rename_all = "camelCase")]
#[model = "Model<Self>"]
#[ts(export)]
pub struct ServerInfo {
    #[serde(default = "get_arch")]
    #[ts(type = "string")]
    pub arch: InternedString,
    #[serde(default = "get_platform")]
    #[ts(type = "string")]
    pub platform: InternedString,
    pub id: String,
    #[ts(type = "string")]
    pub hostname: InternedString,
    #[ts(type = "string")]
    pub version: Version,
    #[ts(type = "string")]
    pub package_version_compat: VersionRange,
    #[ts(type = "Record<string, unknown>")]
    pub post_init_migration_todos: BTreeMap<Version, Value>,
    #[ts(type = "string | null")]
    pub last_backup: Option<DateTime<Utc>>,
    pub network: NetworkInfo,
    #[serde(default)]
    pub status_info: ServerStatus,
    #[ts(type = "number")]
    pub unread_notification_count: u64,
    pub password_hash: String,
    pub pubkey: String,
    pub ca_fingerprint: String,
    #[serde(default)]
    pub ntp_synced: bool,
    #[serde(default)]
    pub zram: bool,
    pub governor: Option<Governor>,
    pub smtp: Option<SmtpValue>,
    #[serde(default = "default_ifconfig_url")]
    #[ts(type = "string")]
    pub ifconfig_url: Url,
    #[ts(type = "number")]
    pub ram: u64,
    pub devices: Vec<LshwDevice>,
    pub kiosk: Option<bool>,
    pub language: Option<InternedString>,
    pub keyboard: Option<KeyboardOptions>,
}

#[derive(Debug, Default, Deserialize, Serialize, HasModel, TS)]
#[serde(rename_all = "camelCase")]
#[model = "Model<Self>"]
#[ts(export)]
pub struct NetworkInfo {
    pub wifi: WifiInfo,
    pub host: Host,
    #[ts(as = "BTreeMap::<GatewayId, NetworkInterfaceInfo>")]
    #[serde(default)]
    pub gateways: OrdMap<GatewayId, NetworkInterfaceInfo>,
    #[serde(default)]
    pub acme: BTreeMap<AcmeProvider, AcmeSettings>,
    #[serde(default)]
    pub dns: DnsSettings,
    #[serde(default)]
    #[ts(type = "string | null")]
    pub default_outbound: Option<GatewayId>,
}

#[derive(Debug, Default, Deserialize, Serialize, HasModel, TS)]
#[serde(rename_all = "camelCase")]
#[model = "Model<Self>"]
#[ts(export)]
pub struct DnsSettings {
    #[ts(type = "string[]")]
    pub dhcp_servers: VecDeque<SocketAddr>,
    #[ts(type = "string[] | null")]
    pub static_servers: Option<VecDeque<SocketAddr>>,
}

#[derive(Clone, Debug, Default, Deserialize, Serialize, HasModel, TS)]
#[serde(rename_all = "camelCase")]
#[model = "Model<Self>"]
#[ts(export)]
pub struct NetworkInterfaceInfo {
    pub name: Option<InternedString>,
    pub secure: Option<bool>,
    pub ip_info: Option<Arc<IpInfo>>,
    #[serde(default, rename = "type")]
    pub gateway_type: Option<GatewayType>,
}
impl NetworkInterfaceInfo {
    pub fn secure(&self) -> bool {
        self.secure.unwrap_or(false)
    }
}

#[derive(Clone, Debug, Default, PartialEq, Eq, Deserialize, Serialize, TS, HasModel)]
#[ts(export)]
#[serde(rename_all = "camelCase")]
#[model = "Model<Self>"]
pub struct IpInfo {
    #[ts(type = "string")]
    pub name: InternedString,
    pub scope_id: u32,
    pub device_type: Option<NetworkInterfaceType>,
    #[ts(type = "string[]")]
    pub subnets: OrdSet<IpNet>,
    #[ts(type = "string[]")]
    pub lan_ip: OrdSet<IpAddr>,
    pub wan_ip: Option<Ipv4Addr>,
    #[ts(type = "string[]")]
    pub ntp_servers: OrdSet<InternedString>,
    #[ts(type = "string[]")]
    pub dns_servers: OrdSet<IpAddr>,
}

#[derive(Clone, Copy, Debug, PartialEq, Eq, PartialOrd, Ord, Deserialize, Serialize, TS)]
#[ts(export)]
#[serde(rename_all = "kebab-case")]
pub enum NetworkInterfaceType {
    Ethernet,
    Wireless,
    Bridge,
    Wireguard,
    Loopback,
}

#[derive(
    Clone,
    Copy,
    Debug,
    Default,
    PartialEq,
    Eq,
    PartialOrd,
    Ord,
    Deserialize,
    Serialize,
    TS,
    clap::ValueEnum,
)]
#[ts(export)]
#[serde(rename_all = "kebab-case")]
pub enum GatewayType {
    #[default]
    InboundOutbound,
    OutboundOnly,
}

#[derive(Debug, Deserialize, Serialize, HasModel, TS)]
#[serde(rename_all = "camelCase")]
#[model = "Model<Self>"]
#[ts(export)]
pub struct AcmeSettings {
    pub contact: Vec<String>,
}
impl DbAccessByKey<AcmeSettings> for Database {
    type Key<'a> = &'a AcmeProvider;
    fn access_by_key<'a>(
        db: &'a Model<Self>,
        key: Self::Key<'_>,
    ) -> Option<&'a Model<AcmeSettings>> {
        db.as_public()
            .as_server_info()
            .as_network()
            .as_acme()
            .as_idx(key)
    }
}

#[derive(Debug, Deserialize, Serialize, HasModel, TS)]
#[serde(rename_all = "camelCase")]
#[model = "Model<Self>"]
#[ts(export)]
pub struct DomainSettings {
    pub gateway: GatewayId,
}

#[derive(Debug, Default, Deserialize, Serialize, HasModel, TS)]
#[model = "Model<Self>"]
#[ts(export)]
pub struct BackupProgress {
    pub complete: bool,
}

#[derive(Debug, Default, Deserialize, Serialize, HasModel, TS)]
#[serde(rename_all = "camelCase")]
#[model = "Model<Self>"]
#[ts(export)]
pub struct ServerStatus {
    pub backup_progress: Option<BTreeMap<PackageId, BackupProgress>>,
    pub updated: bool,
    pub update_progress: Option<FullProgress>,
    #[serde(default)]
    pub shutting_down: bool,
    #[serde(default)]
    pub restarting: bool,
}

#[derive(Debug, Default, Deserialize, Serialize, HasModel, TS)]
#[serde(rename_all = "camelCase")]
#[model = "Model<Self>"]
#[ts(export)]
pub struct WifiInfo {
    pub enabled: bool,
    pub interface: Option<String>,
    pub ssids: BTreeSet<String>,
    pub selected: Option<String>,
    #[ts(type = "string | null")]
    pub last_region: Option<CountryCode>,
}

#[derive(Debug, Deserialize, Serialize, TS)]
#[serde(rename_all = "camelCase")]
#[ts(export)]
pub struct ServerSpecs {
    pub cpu: String,
    pub disk: String,
    pub memory: String,
}
