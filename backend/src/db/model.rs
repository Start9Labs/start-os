use std::collections::{BTreeMap, BTreeSet};
use std::net::{Ipv4Addr, Ipv6Addr};
use std::sync::Arc;

use chrono::{DateTime, Utc};
use emver::VersionRange;
use ipnet::{Ipv4Net, Ipv6Net};
use isocountry::CountryCode;
use itertools::Itertools;
use openssl::hash::MessageDigest;
use patch_db::Model;
use patch_db::{HasModel, Value};
use reqwest::Url;
use serde::{Deserialize, Serialize};
use ssh_key::public::Ed25519PublicKey;

use crate::account::AccountInfo;
use crate::install::progress::InstallProgress;
use crate::net::forward::LanPortForwards;
use crate::net::interface::InterfaceId;
use crate::net::utils::{get_iface_ipv4_addr, get_iface_ipv6_addr};
use crate::prelude::*;
use crate::s9pk::manifest::{Manifest, PackageId};
use crate::status::health_check::HealthCheckId;
use crate::status::Status;
use crate::util::Version;
use crate::version::{Current, VersionT};

#[derive(Debug, Deserialize, Serialize, HasModel)]
#[serde(rename_all = "kebab-case")]
// #[macro_debug]
pub struct Database {
    pub server_info: ServerInfo,

    pub package_data: AllPackageData,
    pub lan_port_forwards: LanPortForwards,
    pub ui: Value,
}
impl Database {
    pub fn init(account: &AccountInfo) -> Self {
        let lan_address = account.hostname.lan_address().parse().unwrap();
        // TODO
        Database {
            server_info: ServerInfo {
                id: account.server_id.clone(),
                version: Current::new().semver().into(),
                hostname: account.hostname.no_dot_host_name(),
                last_backup: None,
                last_wifi_region: None,
                eos_version_compat: Current::new().compat().clone(),
                lan_address,
                tor_address: format!("http://{}", account.key.tor_address())
                    .parse()
                    .unwrap(),
                ip_info: BTreeMap::new(),
                status_info: ServerStatus {
                    backup_progress: None,
                    updated: false,
                    update_progress: None,
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
                pubkey: ssh_key::PublicKey::from(Ed25519PublicKey::from(&account.key.ssh_key()))
                    .to_openssh()
                    .unwrap(),
                ca_fingerprint: account
                    .root_ca_cert
                    .digest(MessageDigest::sha256())
                    .unwrap()
                    .iter()
                    .map(|x| format!("{x:X}"))
                    .join(":"),
                system_start_time: Utc::now().to_rfc3339(),
            },
            package_data: AllPackageData::default(),
            lan_port_forwards: LanPortForwards::new(),
            ui: serde_json::from_str(include_str!("../../../frontend/patchdb-ui-seed.json"))
                .unwrap(),
        }
    }
}

pub type DatabaseModel = Model<Database>;

#[derive(Debug, Deserialize, Serialize, HasModel)]
#[serde(rename_all = "kebab-case")]
pub struct ServerInfo {
    pub id: String,
    pub hostname: String,
    pub version: Version,
    pub last_backup: Option<DateTime<Utc>>,
    /// Used in the wifi to determine the region to set the system to
    pub last_wifi_region: Option<CountryCode>,
    pub eos_version_compat: VersionRange,
    pub lan_address: Url,
    pub tor_address: Url,
    //
    pub ip_info: BTreeMap<String, IpInfo>,

    #[serde(default)]
    pub status_info: ServerStatus,
    pub wifi: WifiInfo,
    pub unread_notification_count: u64,
    pub connection_addresses: ConnectionAddresses,
    pub password_hash: String,
    pub pubkey: String,
    pub ca_fingerprint: String,
    pub system_start_time: String,
}

#[derive(Debug, Deserialize, Serialize, HasModel)]
#[serde(rename_all = "kebab-case")]
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
pub struct BackupProgress {
    pub complete: bool,
}

#[derive(Debug, Default, Deserialize, Serialize, HasModel)]
#[serde(rename_all = "kebab-case")]
pub struct ServerStatus {
    //
    pub backup_progress: Option<BTreeMap<PackageId, BackupProgress>>,
    pub updated: bool,
    //
    pub update_progress: Option<UpdateProgress>,
}

#[derive(Debug, Deserialize, Serialize, HasModel)]
#[serde(rename_all = "kebab-case")]
pub struct UpdateProgress {
    pub size: Option<u64>,
    pub downloaded: u64,
}

#[derive(Debug, Deserialize, Serialize)]
#[serde(rename_all = "kebab-case")]
pub struct WifiInfo {
    pub ssids: Vec<String>,
    pub selected: Option<String>,
    pub connected: Option<String>,
}

#[derive(Debug, Deserialize, Serialize)]
#[serde(rename_all = "kebab-case")]
pub struct ServerSpecs {
    pub cpu: String,
    pub disk: String,
    pub memory: String,
}

#[derive(Debug, Deserialize, Serialize)]
#[serde(rename_all = "kebab-case")]
pub struct ConnectionAddresses {
    pub tor: Vec<String>,
    pub clearnet: Vec<String>,
}

#[derive(Debug, Default, Deserialize, Serialize)]
pub struct AllPackageData(pub BTreeMap<PackageId, PackageDataEntry>);
impl patch_db::Map for AllPackageData {
    type Key = PackageId;
    type Value = PackageDataEntry;
    type Error = Error;
}

#[derive(Debug, Deserialize, Serialize)]
#[serde(rename_all = "kebab-case")]
pub struct StaticFiles {
    license: String,
    instructions: String,
    icon: String,
}
impl StaticFiles {
    pub fn local(id: &PackageId, version: &Version, icon_type: &str) -> Self {
        StaticFiles {
            license: format!("/public/package-data/{}/{}/LICENSE.md", id, version),
            instructions: format!("/public/package-data/{}/{}/INSTRUCTIONS.md", id, version),
            icon: format!("/public/package-data/{}/{}/icon.{}", id, version, icon_type),
        }
    }
}

#[derive(Debug, Deserialize, Serialize, HasModel)]
#[serde(rename_all = "kebab-case")]
pub struct PackageDataEntryInstalling {
    static_files: StaticFiles,
    manifest: Manifest,
    install_progress: Arc<InstallProgress>,
}

#[derive(Debug, Deserialize, Serialize, HasModel)]
#[serde(rename_all = "kebab-case")]
pub struct PackageDataEntryUpdating {
    static_files: StaticFiles,
    manifest: Manifest,
    installed: InstalledPackageInfo,
    install_progress: Arc<InstallProgress>,
}

#[derive(Debug, Deserialize, Serialize, HasModel)]
#[serde(rename_all = "kebab-case")]
pub struct PackageDataEntryRestoring {
    static_files: StaticFiles,
    manifest: Manifest,
    install_progress: Arc<InstallProgress>,
}

#[derive(Debug, Deserialize, Serialize, HasModel)]
#[serde(rename_all = "kebab-case")]
pub struct PackageDataEntryRemoving {
    static_files: StaticFiles,
    manifest: Manifest,
    removing: InstalledPackageInfo,
}

#[derive(Debug, Deserialize, Serialize, HasModel)]
#[serde(rename_all = "kebab-case")]
pub struct PackageDataEntryInstalled {
    static_files: StaticFiles,
    manifest: Manifest,
    installed: InstalledPackageInfo,
}

#[derive(Debug, Deserialize, Serialize, HasModel)]
#[serde(tag = "state")]
#[serde(rename_all = "kebab-case")]
// #[macro_debug]
pub enum PackageDataEntry {
    Installing(PackageDataEntryInstalling),
    Updating(PackageDataEntryUpdating),
    Restoring(PackageDataEntryInstalling),
    Removing(PackageDataEntryRemoving),
    Installed(PackageDataEntryInstalled),
}
impl PackageDataEntry {
    pub fn installed(&self) -> Option<&InstalledPackageInfo> {
        match self {
            Self::Installing { .. } | Self::Restoring { .. } | Self::Removing { .. } => None,
            Self::Updating { installed, .. } | Self::Installed { installed, .. } => Some(installed),
        }
    }
    pub fn installed_mut(&mut self) -> Option<&mut InstalledPackageInfo> {
        match self {
            Self::Installing { .. } | Self::Restoring { .. } | Self::Removing { .. } => None,
            Self::Updating { installed, .. } | Self::Installed { installed, .. } => Some(installed),
        }
    }
    pub fn into_installed(self) -> Option<InstalledPackageInfo> {
        match self {
            Self::Installing { .. } | Self::Restoring { .. } | Self::Removing { .. } => None,
            Self::Updating { installed, .. } | Self::Installed { installed, .. } => Some(installed),
        }
    }
    pub fn manifest(self) -> Manifest {
        match self {
            PackageDataEntry::Installing { manifest, .. } => manifest,
            PackageDataEntry::Updating { manifest, .. } => manifest,
            PackageDataEntry::Restoring { manifest, .. } => manifest,
            PackageDataEntry::Removing { manifest, .. } => manifest,
            PackageDataEntry::Installed { manifest, .. } => manifest,
        }
    }
    pub fn manifest_borrow(&self) -> &Manifest {
        match self {
            PackageDataEntry::Installing { manifest, .. } => manifest,
            PackageDataEntry::Updating { manifest, .. } => manifest,
            PackageDataEntry::Restoring { manifest, .. } => manifest,
            PackageDataEntry::Removing { manifest, .. } => manifest,
            PackageDataEntry::Installed { manifest, .. } => manifest,
        }
    }
}
impl Model<PackageDataEntry> {
    pub fn into_manifest(self) -> Model<Manifest> {
        match self.matchable() {
            PackageDataEntryMatchModel::Installing(a) => a.into_manifest(),
            PackageDataEntryMatchModel::Updating(a) => a.into_manifest(),
            PackageDataEntryMatchModel::Restoring(a) => a.into_manifest(),
            PackageDataEntryMatchModel::Removing(a) => a.into_manifest(),
            PackageDataEntryMatchModel::Installed(a) => a.into_manifest(),
            PackageDataEntryMatchModel::Error(_) => Model::new(Value::Null),
        }
    }
    pub fn expect_installed_into(self) -> Result<Model<PackageDataEntryInstalled>, Error> {
        if let PackageDataEntryMatchModel::Installed(a) = self {
            Ok(a)
        } else {
            Err(Error::new(
                eyre!("package is not in installed state"),
                ErrorKind::InvalidRequest,
            ))
        }
    }
    pub fn as_manifest_mut(&mut self) -> &mut Model<Manifest> {
        match self.as_matchable_mut() {
            PackageDataEntryMatchModelMut::Installing(mut a) => a.manifest(),
            PackageDataEntryMatchModelMut::Updating(mut a) => a.manifest(),
            PackageDataEntryMatchModelMut::Restoring(mut a) => a.manifest(),
            PackageDataEntryMatchModelMut::Removing(mut a) => a.manifest(),
            PackageDataEntryMatchModelMut::Installed(mut a) => a.manifest(),
            PackageDataEntryMatchModelMut::Error(mut a) => panic!("unknown variant"),
        }
    }
    pub fn expect_installed_as_mut(
        &mut self,
    ) -> Result<&mut Model<PackageDataEntryInstalled>, Error> {
        if let PackageDataEntryMatchModelMut::Installed(a) = self.as_matchable_mut() {
            Ok(a)
        } else {
            Err(Error::new(
                eyre!("package is not in installed state"),
                ErrorKind::InvalidRequest,
            ))
        }
    }
}

#[derive(Debug, Deserialize, Serialize, HasModel)]
#[serde(rename_all = "kebab-case")]
pub struct InstalledPackageInfo {
    pub status: Status,
    pub marketplace_url: Option<Url>,
    #[serde(default)]
    #[serde(with = "crate::util::serde::ed25519_pubkey")]
    pub developer_key: ed25519_dalek::PublicKey,

    pub manifest: Manifest,
    pub last_backup: Option<DateTime<Utc>>,
    //
    pub dependency_info: BTreeMap<PackageId, StaticDependencyInfo>,

    pub current_dependencies: CurrentDependencies,

    pub interface_addresses: InterfaceAddressMap,
}

#[derive(Debug, Clone, Default, Deserialize, Serialize)]
pub struct CurrentDependencies(pub BTreeMap<PackageId, CurrentDependencyInfo>);
impl CurrentDependencies {
    pub fn map(
        mut self,
        transform: impl Fn(
            BTreeMap<PackageId, CurrentDependencyInfo>,
        ) -> BTreeMap<PackageId, CurrentDependencyInfo>,
    ) -> Self {
        self.0 = transform(self.0);
        self
    }
}
impl patch_db::Map for CurrentDependencies {
    type Key = PackageId;
    type Value = CurrentDependencyInfo;
    type Error = Error;
}

#[derive(Clone, Debug, Default, Deserialize, Serialize, HasModel)]
#[serde(rename_all = "kebab-case")]
pub struct StaticDependencyInfo {
    pub manifest: Option<Manifest>,
    pub icon: String,
}

#[derive(Clone, Debug, Default, Deserialize, Serialize, HasModel)]
#[serde(rename_all = "kebab-case")]
pub struct CurrentDependencyInfo {
    pub health_checks: BTreeSet<HealthCheckId>,
}

#[derive(Debug, Deserialize, Serialize)]
pub struct InterfaceAddressMap(pub BTreeMap<InterfaceId, InterfaceAddresses>);
impl patch_db::Map for InterfaceAddressMap {
    type Key = InterfaceId;
    type Value = InterfaceAddresses;
    type Error = Error;
}

#[derive(Debug, Deserialize, Serialize, HasModel)]
#[serde(rename_all = "kebab-case")]
pub struct InterfaceAddresses {
    //
    pub tor_address: Option<String>,
    //
    pub lan_address: Option<String>,
}

#[derive(Debug, Deserialize, Serialize, HasModel)]
#[serde(rename_all = "kebab-case")]
pub struct RecoveredPackageInfo {
    pub title: String,
    pub icon: String,
    pub version: Version,
}
