use std::collections::{BTreeMap, BTreeSet};
use std::net::{Ipv4Addr, Ipv6Addr};
use std::sync::Arc;

use chrono::{DateTime, Utc};
use color_eyre::eyre::eyre;
use emver::VersionRange;
use ipnet::{Ipv4Net, Ipv6Net};
use isocountry::CountryCode;
use itertools::Itertools;
use openssl::hash::MessageDigest;
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
    #[model]
    pub server_info: ServerInfo,
    #[model]
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

pub type DatabaseModel = <Database as HasModel>::Model;
pub type DatabaseModelMut<'a> = <<Database as HasModel>::Model as ModelExt>::Mut<'a>;

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
    // #[model]
    pub ip_info: BTreeMap<String, IpInfo>,
    #[model]
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
    // #[model]
    pub backup_progress: Option<BTreeMap<PackageId, BackupProgress>>,
    pub updated: bool,
    // #[model]
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
impl Map for AllPackageData {
    type Key = PackageId;
    type Value = PackageDataEntry;
}
impl HasModel for AllPackageData {
    type Model = MapModel<Self>;
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
#[serde(tag = "state")]
#[serde(rename_all = "kebab-case")]
pub enum PackageDataEntry {
    #[serde(rename_all = "kebab-case")]
    Installing {
        static_files: StaticFiles,
        #[model]
        manifest: Manifest,
        install_progress: Arc<InstallProgress>,
    },
    #[serde(rename_all = "kebab-case")]
    Updating {
        static_files: StaticFiles,
        #[model]
        manifest: Manifest,
        #[model]
        installed: InstalledPackageDataEntry,
        install_progress: Arc<InstallProgress>,
    },
    #[serde(rename_all = "kebab-case")]
    Restoring {
        static_files: StaticFiles,
        #[model]
        manifest: Manifest,
        install_progress: Arc<InstallProgress>,
    },
    #[serde(rename_all = "kebab-case")]
    Removing {
        static_files: StaticFiles,
        #[model]
        manifest: Manifest,
        #[model]
        removing: InstalledPackageDataEntry,
    },
    #[serde(rename_all = "kebab-case")]
    Installed {
        static_files: StaticFiles,
        #[model]
        manifest: Manifest,
        #[model]
        installed: InstalledPackageDataEntry,
    },
}
impl PackageDataEntry {
    pub fn installed(&self) -> Option<&InstalledPackageDataEntry> {
        match self {
            Self::Installing { .. } | Self::Restoring { .. } | Self::Removing { .. } => None,
            Self::Updating { installed, .. } | Self::Installed { installed, .. } => Some(installed),
        }
    }
    pub fn installed_mut(&mut self) -> Option<&mut InstalledPackageDataEntry> {
        match self {
            Self::Installing { .. } | Self::Restoring { .. } | Self::Removing { .. } => None,
            Self::Updating { installed, .. } | Self::Installed { installed, .. } => Some(installed),
        }
    }
    pub fn into_installed(self) -> Option<InstalledPackageDataEntry> {
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
pub type PackageDataEntryModel = package_data_entry_model::Model;
impl package_data_entry_model::Model {
    pub fn manifest(&self) -> <Manifest as HasModel>::Model {
        match self {
            Self::Installing(a) => a.manifest(),
            Self::Updating(a) => a.manifest(),
            Self::Restoring(a) => a.manifest(),
            Self::Removing(a) => a.manifest(),
            Self::Installed(a) => a.manifest(),
            Self::Error(a) => <<Manifest as HasModel>::Model as patch_db::Model>::new(a.clone()),
        }
    }
    pub fn as_installed(self) -> Result<package_data_entry_model::InstalledModel, Error> {
        if let Self::Installed(a) = self {
            Ok(a)
        } else {
            Err(Error::new(
                eyre!("package is not in installed state"),
                ErrorKind::InvalidRequest,
            ))
        }
    }
}
pub type PackageDataEntryModelMut<'a> = package_data_entry_model::ModelMut<'a>;
impl<'a> package_data_entry_model::ModelMut<'a> {
    pub fn manifest(&mut self) -> <<Manifest as HasModel>::Model as patch_db::Model>::Mut<'a> {
        match self {
            Self::Installing(mut a) => a.manifest(),
            Self::Updating(mut a) => a.manifest(),
            Self::Restoring(mut a) => a.manifest(),
            Self::Removing(mut a) => a.manifest(),
            Self::Installed(mut a) => a.manifest(),
            Self::Error(mut a) => <<<Manifest as HasModel>::Model as patch_db::Model>::Mut::<'a> as patch_db::ModelMut>::new(a),
        }
    }
    pub fn as_installed(self) -> Result<package_data_entry_model::InstalledModelMut<'a>, Error> {
        if let Self::Installed(a) = self {
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
pub struct InstalledPackageDataEntry {
    #[model]
    pub status: Status,
    pub marketplace_url: Option<Url>,
    #[serde(default)]
    #[serde(with = "crate::util::serde::ed25519_pubkey")]
    pub developer_key: ed25519_dalek::PublicKey,
    #[model]
    pub manifest: Manifest,
    pub last_backup: Option<DateTime<Utc>>,
    // #[model]
    pub dependency_info: BTreeMap<PackageId, StaticDependencyInfo>,
    #[model]
    pub current_dependencies: CurrentDependencies,
    #[model]
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
impl Map for CurrentDependencies {
    type Key = PackageId;
    type Value = CurrentDependencyInfo;
}
impl HasModel for CurrentDependencies {
    type Model = MapModel<Self>;
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
impl Map for InterfaceAddressMap {
    type Key = InterfaceId;
    type Value = InterfaceAddresses;
}
impl HasModel for InterfaceAddressMap {
    type Model = MapModel<Self>;
}

#[derive(Debug, Deserialize, Serialize, HasModel)]
#[serde(rename_all = "kebab-case")]
pub struct InterfaceAddresses {
    // #[model]
    pub tor_address: Option<String>,
    // #[model]
    pub lan_address: Option<String>,
}

#[derive(Debug, Deserialize, Serialize, HasModel)]
#[serde(rename_all = "kebab-case")]
pub struct RecoveredPackageInfo {
    pub title: String,
    pub icon: String,
    pub version: Version,
}
