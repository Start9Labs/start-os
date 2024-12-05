use std::collections::{BTreeMap, BTreeSet};
use std::net::{IpAddr, Ipv4Addr, Ipv6Addr};

use chrono::{DateTime, Utc};
use exver::{Version, VersionRange};
use imbl_value::InternedString;
use ipnet::{IpNet, Ipv4Net, Ipv6Net};
use isocountry::CountryCode;
use itertools::Itertools;
use models::PackageId;
use openssl::hash::MessageDigest;
use patch_db::{HasModel, Value};
use reqwest::Url;
use serde::{Deserialize, Serialize};
use torut::onion::OnionAddressV3;
use ts_rs::TS;

use crate::account::AccountInfo;
use crate::db::model::package::AllPackageData;
use crate::net::utils::{get_iface_ipv4_addr, get_iface_ipv6_addr};
use crate::prelude::*;
use crate::progress::FullProgress;
use crate::system::SmtpValue;
use crate::util::cpupower::Governor;
use crate::util::lshw::LshwDevice;
use crate::version::{Current, VersionT};
use crate::{ARCH, PLATFORM};

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
    pub fn init(account: &AccountInfo) -> Result<Self, Error> {
        let lan_address = account.hostname.lan_address().parse().unwrap();
        Ok(Self {
            server_info: ServerInfo {
                arch: get_arch(),
                platform: get_platform(),
                id: account.server_id.clone(),
                version: Current::default().semver(),
                hostname: account.hostname.no_dot_host_name(),
                last_backup: None,
                package_version_compat: Current::default().compat().clone(),
                post_init_migration_todos: BTreeSet::new(),
                lan_address,
                onion_address: account.tor_key.public().get_onion_address(),
                tor_address: format!("https://{}", account.tor_key.public().get_onion_address())
                    .parse()
                    .unwrap(),
                network_interfaces: BTreeMap::new(),
                acme: None,
                status_info: ServerStatus {
                    backup_progress: None,
                    updated: false,
                    update_progress: None,
                    shutting_down: false,
                    restarting: false,
                },
                wifi: WifiInfo::default(),
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
                ram: 0,
                devices: Vec::new(),
            },
            package_data: AllPackageData::default(),
            ui: serde_json::from_str(include_str!(concat!(
                env!("CARGO_MANIFEST_DIR"),
                "/../../web/patchdb-ui-seed.json"
            )))
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
    #[ts(type = "string[]")]
    pub post_init_migration_todos: BTreeSet<Version>,
    #[ts(type = "string | null")]
    pub last_backup: Option<DateTime<Utc>>,
    #[ts(type = "string")]
    pub lan_address: Url,
    #[ts(type = "string")]
    pub onion_address: OnionAddressV3,
    /// for backwards compatibility
    #[ts(type = "string")]
    pub tor_address: Url,
    #[ts(as = "BTreeMap::<String, NetworkInterfaceInfo>")]
    #[serde(default)]
    pub network_interfaces: BTreeMap<InternedString, NetworkInterfaceInfo>,
    pub acme: Option<AcmeSettings>,
    #[serde(default)]
    pub status_info: ServerStatus,
    pub wifi: WifiInfo,
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
    #[ts(type = "number")]
    pub ram: u64,
    pub devices: Vec<LshwDevice>,
}

#[derive(Clone, Debug, Default, Deserialize, Serialize, HasModel, TS)]
#[serde(rename_all = "camelCase")]
#[model = "Model<Self>"]
#[ts(export)]
pub struct NetworkInterfaceInfo {
    pub public: Option<bool>,
    pub ip_info: Option<IpInfo>,
}
impl NetworkInterfaceInfo {
    pub fn public(&self) -> bool {
        self.public.unwrap_or_else(|| {
            !self.ip_info.as_ref().map_or(true, |ip_info| {
                ip_info.subnets.iter().all(|ipnet| {
                    if let IpAddr::V4(ip4) = ipnet.addr() {
                        ip4.is_loopback() || ip4.is_private() || ip4.is_link_local()
                    } else {
                        true
                    }
                })
            })
        })
    }
}

#[derive(Clone, Debug, Default, PartialEq, Eq, Deserialize, Serialize, TS)]
#[ts(export)]
#[serde(rename_all = "camelCase")]
pub struct IpInfo {
    pub scope_id: u32,
    #[ts(type = "string[]")]
    pub subnets: BTreeSet<IpNet>,
    pub wan_ip: Option<Ipv4Addr>,
}

#[derive(Debug, Deserialize, Serialize, HasModel, TS)]
#[serde(rename_all = "camelCase")]
#[model = "Model<Self>"]
#[ts(export)]
pub struct AcmeSettings {
    #[ts(type = "string")]
    pub provider: Url,
    /// email addresses for letsencrypt
    pub contact: Vec<String>,
    #[ts(type = "string[]")]
    /// domains to get letsencrypt certs for
    pub domains: BTreeSet<InternedString>,
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
