use std::collections::{BTreeMap, BTreeSet};
use std::net::{Ipv4Addr, Ipv6Addr};
use std::path::{Path, PathBuf};

use chrono::{DateTime, Utc};
use emver::VersionRange;
use ipnet::{Ipv4Net, Ipv6Net};
use isocountry::CountryCode;
use itertools::Itertools;
use models::{DataUrl, InterfaceId};
use openssl::hash::MessageDigest;
use patch_db::{HasModel, Value};
use reqwest::Url;
use serde::{Deserialize, Serialize};
use ssh_key::public::Ed25519PublicKey;

use crate::account::AccountInfo;
use crate::action::Actions;
use crate::install::progress::InstallProgress;
use crate::net::forward::LanPortForwards;
use crate::net::utils::{get_iface_ipv4_addr, get_iface_ipv6_addr};
use crate::prelude::*;
use crate::s9pk::manifest::{Manifest, PackageId};
use crate::status::health_check::HealthCheckId;
use crate::status::Status;
use crate::util::Version;
use crate::version::{Current, VersionT};

#[derive(Debug, Deserialize, Serialize, HasModel)]
#[serde(rename_all = "kebab-case")]
#[model = "Model<Self>"]
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
                    shutting_down: false,
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
#[model = "Model<Self>"]
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
    pub ip_info: BTreeMap<String, IpInfo>,
    #[serde(default)]
    pub status_info: ServerStatus,
    pub unread_notification_count: u64,
    pub connection_addresses: ConnectionAddresses,
    pub password_hash: String,
    pub pubkey: String,
    pub ca_fingerprint: String,
    pub system_start_time: String,
}

#[derive(Debug, Deserialize, Serialize, HasModel)]
#[serde(rename_all = "kebab-case")]
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
#[serde(rename_all = "kebab-case")]
#[model = "Model<Self>"]
pub struct ServerStatus {
    pub backup_progress: Option<BTreeMap<PackageId, BackupProgress>>,
    pub updated: bool,
    pub update_progress: Option<UpdateProgress>,
    pub shutting_down: bool,
}

#[derive(Debug, Deserialize, Serialize, HasModel)]
#[serde(rename_all = "kebab-case")]
#[model = "Model<Self>"]
pub struct UpdateProgress {
    pub size: Option<u64>,
    pub downloaded: u64,
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

#[derive(Debug, Deserialize, Serialize, HasModel)]
#[serde(rename_all = "kebab-case")]
#[model = "Model<Self>"]
pub struct PackageDataEntryInstalling {
    pub icon: DataUrl,
    pub manifest: Manifest,
    pub install_progress: InstallProgress,
}

#[derive(Debug, Deserialize, Serialize, HasModel)]
#[serde(rename_all = "kebab-case")]
#[model = "Model<Self>"]
pub struct PackageDataEntryUpdating {
    pub icon: DataUrl,
    pub manifest: Manifest,
    pub old_manifest: Manifest,
    pub rollback_to: Option<InstalledPackageInfo>,
    pub install_progress: InstallProgress,
}

#[derive(Debug, Deserialize, Serialize, HasModel)]
#[serde(rename_all = "kebab-case")]
#[model = "Model<Self>"]
pub struct PackageDataEntryRestoring {
    pub icon: DataUrl,
    pub manifest: Manifest,
    pub install_progress: InstallProgress,
}

#[derive(Debug, Deserialize, Serialize, HasModel)]
#[serde(rename_all = "kebab-case")]
#[model = "Model<Self>"]
pub struct PackageDataEntryRemoving {
    pub icon: DataUrl,
    pub manifest: Manifest,
}

#[derive(Debug, Deserialize, Serialize, HasModel)]
#[serde(rename_all = "kebab-case")]
#[model = "Model<Self>"]
pub struct PackageDataEntryNeedsUpdate {
    pub icon: DataUrl,
    pub manifest: Manifest,
}

#[derive(Debug, Deserialize, Serialize, HasModel)]
#[serde(rename_all = "kebab-case")]
#[model = "Model<Self>"]
pub struct PackageDataEntryInstalled {
    pub icon: DataUrl,
    pub manifest: Manifest,
    pub installed: InstalledPackageInfo,
}

#[derive(Debug, Deserialize, Serialize, HasModel)]
#[serde(tag = "state")]
#[serde(rename_all = "kebab-case")]
#[model = "Model<Self>"]
// #[macro_debug]
pub enum PackageDataEntry {
    Installing(PackageDataEntryInstalling),
    Updating(PackageDataEntryUpdating),
    Restoring(PackageDataEntryRestoring),
    Removing(PackageDataEntryRemoving),
    NeedsUpdate(PackageDataEntryNeedsUpdate),
    Installed(PackageDataEntryInstalled),
}
impl Model<PackageDataEntry> {
    pub fn expect_into_installed(self) -> Result<Model<PackageDataEntryInstalled>, Error> {
        if let PackageDataEntryMatchModel::Installed(a) = self.into_match() {
            Ok(a)
        } else {
            Err(Error::new(
                eyre!("package is not in installed state"),
                ErrorKind::InvalidRequest,
            ))
        }
    }
    pub fn expect_as_installed_mut(
        &mut self,
    ) -> Result<&mut Model<PackageDataEntryInstalled>, Error> {
        if let PackageDataEntryMatchModelMut::Installed(a) = self.as_match_mut() {
            Ok(a)
        } else {
            Err(Error::new(
                eyre!("package is not in installed state"),
                ErrorKind::InvalidRequest,
            ))
        }
    }
    pub fn into_manifest(self) -> Model<Manifest> {
        match self.into_match() {
            PackageDataEntryMatchModel::Installing(a) => a.into_manifest(),
            PackageDataEntryMatchModel::Updating(a) => a.into_old_manifest(),
            PackageDataEntryMatchModel::Restoring(a) => a.into_manifest(),
            PackageDataEntryMatchModel::Removing(a) => a.into_manifest(),
            PackageDataEntryMatchModel::NeedsUpdate(a) => a.into_manifest(),
            PackageDataEntryMatchModel::Installed(a) => a.into_manifest(),
            PackageDataEntryMatchModel::Error(_) => Model::from(Value::Null),
        }
    }
    pub fn as_manifest(&self) -> Result<&Model<Manifest>, Error> {
        match self.as_match() {
            PackageDataEntryMatchModelRef::Installing(a) => Ok(a.as_manifest()),
            PackageDataEntryMatchModelRef::Updating(a) => Ok(a.as_old_manifest()),
            PackageDataEntryMatchModelRef::Restoring(a) => Ok(a.as_manifest()),
            PackageDataEntryMatchModelRef::Removing(a) => Ok(a.as_manifest()),
            PackageDataEntryMatchModelRef::NeedsUpdate(a) => Ok(a.as_manifest()),
            PackageDataEntryMatchModelRef::Installed(a) => Ok(a.as_manifest()),
            PackageDataEntryMatchModelRef::Error(a) => Err(Error::new(
                eyre!("unknown variant of PackageDataEntry"),
                ErrorKind::Deserialization,
            )),
        }
    }
    pub fn as_icon(&self) -> Result<&Model<DataUrl>, Error> {
        match self.as_match() {
            PackageDataEntryMatchModelRef::Installing(a) => Ok(a.as_icon()),
            PackageDataEntryMatchModelRef::Updating(a) => Ok(a.as_icon()),
            PackageDataEntryMatchModelRef::Restoring(a) => Ok(a.as_icon()),
            PackageDataEntryMatchModelRef::Removing(a) => Ok(a.as_icon()),
            PackageDataEntryMatchModelRef::NeedsUpdate(a) => Ok(a.as_icon()),
            PackageDataEntryMatchModelRef::Installed(a) => Ok(a.as_icon()),
            PackageDataEntryMatchModelRef::Error(a) => Err(Error::new(
                eyre!("unknown variant of PackageDataEntry"),
                ErrorKind::Deserialization,
            )),
        }
    }
}

#[derive(Debug, Deserialize, Serialize, HasModel)]
#[serde(rename_all = "kebab-case")]
#[model = "Model<Self>"]
pub struct InstalledPackageInfo {
    pub status: Status,
    pub marketplace_url: Option<Url>,
    #[serde(default)]
    #[serde(with = "crate::util::serde::ed25519_pubkey")]
    pub developer_key: ed25519_dalek::PublicKey,
    pub last_backup: Option<DateTime<Utc>>,
    pub dependency_info: BTreeMap<PackageId, StaticDependencyInfo>,
    pub current_dependencies: CurrentDependencies,
    pub address_info: AddressInfoMap,
    pub actions: Actions,
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

#[derive(Debug, Deserialize, Serialize, HasModel)]
#[serde(rename_all = "kebab-case")]
#[model = "Model<Self>"]
pub struct StaticDependencyInfo {
    pub title: String,
    pub icon: DataUrl,
}

#[derive(Clone, Debug, Default, Deserialize, Serialize, HasModel)]
#[serde(rename_all = "kebab-case")]
#[model = "Model<Self>"]
pub struct CurrentDependencyInfo {
    pub health_checks: BTreeSet<HealthCheckId>,
}

#[derive(Debug, Deserialize, Serialize)]
pub struct AddressInfoMap(pub BTreeMap<AddressId, AddressInfo>);
impl Map for InterfaceAddressMap {
    type Key = AddressId;
    type Value = AddressInfo;
}

#[derive(Debug, Deserialize, Serialize, HasModel)]
#[serde(rename_all = "kebab-case")]
#[model = "Model<Self>"]
pub struct AddressInfo {
    pub name: String,
    pub description: String,
    pub addresses: Vec<Url>,
    pub ui: bool,
}
