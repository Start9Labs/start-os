use std::collections::BTreeMap;
use std::net::{Ipv4Addr, Ipv6Addr};

use chrono::{DateTime, Utc};
use emver::VersionRange;
use imbl_value::InternedString;
use ipnet::{Ipv4Net, Ipv6Net};
use isocountry::CountryCode;
use itertools::Itertools;
use models::PackageId;
use openssl::hash::MessageDigest;
use patch_db::{HasModel, Value};
use reqwest::Url;
use serde::{Deserialize, Serialize};
use torut::onion::OnionAddressV3;

use crate::account::AccountInfo;
use crate::db::model::package::AllPackageData;
use crate::net::utils::{get_iface_ipv4_addr, get_iface_ipv6_addr};
use crate::prelude::*;
use crate::util::cpupower::Governor;
use crate::util::Version;
use crate::version::{Current, VersionT};
use crate::{ARCH, PLATFORM};

#[derive(Debug, Deserialize, Serialize, HasModel)]
#[serde(rename_all = "camelCase")]
#[model = "Model<Self>"]
// #[macro_debug]
pub struct Public {
    pub server_info: ServerInfo,
    pub package_data: AllPackageData,
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
                version: Current::new().semver().into(),
                hostname: account.hostname.no_dot_host_name(),
                last_backup: None,
                last_wifi_region: None,
                eos_version_compat: Current::new().compat().clone(),
                lan_address,
                onion_address: account.tor_key.public().get_onion_address(),
                tor_address: format!("https://{}", account.tor_key.public().get_onion_address())
                    .parse()
                    .unwrap(),
                ip_info: BTreeMap::new(),
                status_info: ServerStatus {
                    backup_progress: None,
                    updated: false,
                    update_progress: None,
                    shutting_down: false,
                    restarting: false,
                },
                wifi: WifiInfo {
                    ssids: Vec::new(),
                    connected: None,
                    selected: None,
                },
                unread_notification_count: 0,
                connection_addresses: ConnectionAddresses {
                    tor: Vec::new(),
                    clearnet: Vec::new(),
                },
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

#[derive(Debug, Deserialize, Serialize, HasModel)]
#[serde(rename_all = "camelCase")]
#[model = "Model<Self>"]
pub struct ServerInfo {
    #[serde(default = "get_arch")]
    pub arch: InternedString,
    #[serde(default = "get_platform")]
    pub platform: InternedString,
    pub id: String,
    pub hostname: String,
    pub version: Version,
    pub last_backup: Option<DateTime<Utc>>,
    /// Used in the wifi to determine the region to set the system to
    pub last_wifi_region: Option<CountryCode>,
    pub eos_version_compat: VersionRange,
    pub lan_address: Url,
    pub onion_address: OnionAddressV3,
    /// for backwards compatibility
    pub tor_address: Url,
    pub ip_info: BTreeMap<String, IpInfo>,
    #[serde(default)]
    pub status_info: ServerStatus,
    pub wifi: WifiInfo,
    pub unread_notification_count: u64,
    pub connection_addresses: ConnectionAddresses,
    pub password_hash: String,
    pub pubkey: String,
    pub ca_fingerprint: String,
    #[serde(default)]
    pub ntp_synced: bool,
    #[serde(default)]
    pub zram: bool,
    pub governor: Option<Governor>,
}

#[derive(Debug, Deserialize, Serialize, HasModel)]
#[serde(rename_all = "camelCase")]
#[model = "Model<Self>"]
pub struct IpInfo {
    pub ipv4_range: Option<Ipv4Net>,
    pub ipv4: Option<Ipv4Addr>,
    pub ipv6_range: Option<Ipv6Net>,
    pub ipv6: Option<Ipv6Addr>,
}
impl IpInfo {
    pub async fn for_interface(iface: &str) -> Result<Self, Error> {
        let (ipv4, ipv4_range) = get_iface_ipv4_addr(iface).await?.unzip();
        let (ipv6, ipv6_range) = get_iface_ipv6_addr(iface).await?.unzip();
        Ok(Self {
            ipv4_range,
            ipv4,
            ipv6_range,
            ipv6,
        })
    }
}

#[derive(Debug, Default, Deserialize, Serialize, HasModel)]
#[model = "Model<Self>"]
pub struct BackupProgress {
    pub complete: bool,
}

#[derive(Debug, Default, Deserialize, Serialize, HasModel)]
#[serde(rename_all = "camelCase")]
#[model = "Model<Self>"]
pub struct ServerStatus {
    pub backup_progress: Option<BTreeMap<PackageId, BackupProgress>>,
    pub updated: bool,
    pub update_progress: Option<UpdateProgress>,
    #[serde(default)]
    pub shutting_down: bool,
    #[serde(default)]
    pub restarting: bool,
}

#[derive(Debug, Deserialize, Serialize, HasModel)]
#[serde(rename_all = "camelCase")]
#[model = "Model<Self>"]
pub struct UpdateProgress {
    pub size: Option<u64>,
    pub downloaded: u64,
}

#[derive(Debug, Deserialize, Serialize, HasModel)]
#[serde(rename_all = "camelCase")]
#[model = "Model<Self>"]
pub struct WifiInfo {
    pub ssids: Vec<String>,
    pub selected: Option<String>,
    pub connected: Option<String>,
}

#[derive(Debug, Deserialize, Serialize)]
#[serde(rename_all = "camelCase")]
pub struct ServerSpecs {
    pub cpu: String,
    pub disk: String,
    pub memory: String,
}

#[derive(Debug, Deserialize, Serialize)]
#[serde(rename_all = "camelCase")]
pub struct ConnectionAddresses {
    pub tor: Vec<String>,
    pub clearnet: Vec<String>,
}
