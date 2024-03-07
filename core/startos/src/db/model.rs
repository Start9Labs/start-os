use std::collections::{BTreeMap, BTreeSet};
use std::net::{Ipv4Addr, Ipv6Addr};

use chrono::{DateTime, Utc};
use emver::VersionRange;
use imbl_value::InternedString;
use ipnet::{Ipv4Net, Ipv6Net};
use isocountry::CountryCode;
use itertools::Itertools;
use models::{DataUrl, HealthCheckId, HostId, PackageId};
use openssl::hash::MessageDigest;
use patch_db::json_ptr::JsonPointer;
use patch_db::{HasModel, Value};
use reqwest::Url;
use serde::{Deserialize, Serialize};
use torut::onion::OnionAddressV3;

use crate::account::AccountInfo;
use crate::auth::Sessions;
use crate::backup::target::cifs::CifsTargets;
use crate::net::forward::AvailablePorts;
use crate::net::host::HostInfo;
use crate::net::keys::KeyStore;
use crate::net::utils::{get_iface_ipv4_addr, get_iface_ipv6_addr};
use crate::notifications::Notifications;
use crate::prelude::*;
use crate::progress::FullProgress;
use crate::s9pk::manifest::Manifest;
use crate::ssh::SshKeys;
use crate::status::Status;
use crate::util::cpupower::Governor;
use crate::util::serde::Pem;
use crate::util::Version;
use crate::version::{Current, VersionT};
use crate::{ARCH, PLATFORM};

fn get_arch() -> InternedString {
    (*ARCH).into()
}

fn get_platform() -> InternedString {
    (&*PLATFORM).into()
}

#[derive(Debug, Deserialize, Serialize, HasModel)]
#[serde(rename_all = "kebab-case")]
#[model = "Model<Self>"]
pub struct Database {
    pub public: Public,
    pub private: Private,
}
impl Database {
    pub fn init(account: &AccountInfo) -> Result<Self, Error> {
        let lan_address = account.hostname.lan_address().parse().unwrap();
        Ok(Database {
            public: Public {
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
                    tor_address: format!(
                        "https://{}",
                        account.tor_key.public().get_onion_address()
                    )
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
                .unwrap(),
            },
            private: Private {
                key_store: KeyStore::new(account)?,
                password: account.password.clone(),
                ssh_privkey: Pem(account.ssh_key.clone()),
                ssh_pubkeys: SshKeys::new(),
                available_ports: AvailablePorts::new(),
                sessions: Sessions::new(),
                notifications: Notifications::new(),
                cifs: CifsTargets::new(),
            }, // TODO
        })
    }
}

pub type DatabaseModel = Model<Database>;

#[derive(Debug, Deserialize, Serialize, HasModel)]
#[serde(rename_all = "kebab-case")]
#[model = "Model<Self>"]
// #[macro_debug]
pub struct Public {
    pub server_info: ServerInfo,
    pub package_data: AllPackageData,
    pub ui: Value,
}

#[derive(Debug, Deserialize, Serialize, HasModel)]
#[serde(rename_all = "kebab-case")]
#[model = "Model<Self>"]
pub struct Private {
    pub key_store: KeyStore,
    pub password: String, // argon2 hash
    pub ssh_privkey: Pem<ssh_key::PrivateKey>,
    pub ssh_pubkeys: SshKeys,
    pub available_ports: AvailablePorts,
    pub sessions: Sessions,
    pub notifications: Notifications,
    pub cifs: CifsTargets,
}

#[derive(Debug, Deserialize, Serialize, HasModel)]
#[serde(rename_all = "kebab-case")]
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
    #[serde(default)]
    pub shutting_down: bool,
    #[serde(default)]
    pub restarting: bool,
}

#[derive(Debug, Deserialize, Serialize, HasModel)]
#[serde(rename_all = "kebab-case")]
#[model = "Model<Self>"]
pub struct UpdateProgress {
    pub size: Option<u64>,
    pub downloaded: u64,
}

#[derive(Debug, Deserialize, Serialize, HasModel)]
#[serde(rename_all = "kebab-case")]
#[model = "Model<Self>"]
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
    fn key_str(key: &Self::Key) -> Result<impl AsRef<str>, Error> {
        Ok(key)
    }
    fn key_string(key: &Self::Key) -> Result<InternedString, Error> {
        Ok(key.clone().into())
    }
}

#[derive(Debug, Deserialize, Serialize, HasModel)]
#[serde(rename_all = "kebab-case")]
#[model = "Model<Self>"]
pub struct StaticFiles {
    license: String,
    instructions: String,
    icon: DataUrl<'static>,
}
impl StaticFiles {
    pub fn local(id: &PackageId, version: &Version, icon: DataUrl<'static>) -> Self {
        StaticFiles {
            license: format!("/public/package-data/{}/{}/LICENSE.md", id, version),
            instructions: format!("/public/package-data/{}/{}/INSTRUCTIONS.md", id, version),
            icon,
        }
    }
}

#[derive(Debug, Deserialize, Serialize, HasModel)]
#[serde(rename_all = "kebab-case")]
#[model = "Model<Self>"]
pub struct PackageDataEntryInstalling {
    pub static_files: StaticFiles,
    pub manifest: Manifest,
    pub install_progress: FullProgress,
}

#[derive(Debug, Deserialize, Serialize, HasModel)]
#[serde(rename_all = "kebab-case")]
#[model = "Model<Self>"]
pub struct PackageDataEntryUpdating {
    pub static_files: StaticFiles,
    pub manifest: Manifest,
    pub installed: InstalledPackageInfo,
    pub install_progress: FullProgress,
}

#[derive(Debug, Deserialize, Serialize, HasModel)]
#[serde(rename_all = "kebab-case")]
#[model = "Model<Self>"]
pub struct PackageDataEntryRestoring {
    pub static_files: StaticFiles,
    pub manifest: Manifest,
    pub install_progress: FullProgress,
}

#[derive(Debug, Deserialize, Serialize, HasModel)]
#[serde(rename_all = "kebab-case")]
#[model = "Model<Self>"]
pub struct PackageDataEntryRemoving {
    pub static_files: StaticFiles,
    pub manifest: Manifest,
    pub removing: InstalledPackageInfo,
}

#[derive(Debug, Deserialize, Serialize, HasModel)]
#[serde(rename_all = "kebab-case")]
#[model = "Model<Self>"]
pub struct PackageDataEntryInstalled {
    pub static_files: StaticFiles,
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
    pub fn expect_as_installed(&self) -> Result<&Model<PackageDataEntryInstalled>, Error> {
        if let PackageDataEntryMatchModelRef::Installed(a) = self.as_match() {
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
    pub fn expect_into_removing(self) -> Result<Model<PackageDataEntryRemoving>, Error> {
        if let PackageDataEntryMatchModel::Removing(a) = self.into_match() {
            Ok(a)
        } else {
            Err(Error::new(
                eyre!("package is not in removing state"),
                ErrorKind::InvalidRequest,
            ))
        }
    }
    pub fn expect_as_removing(&self) -> Result<&Model<PackageDataEntryRemoving>, Error> {
        if let PackageDataEntryMatchModelRef::Removing(a) = self.as_match() {
            Ok(a)
        } else {
            Err(Error::new(
                eyre!("package is not in removing state"),
                ErrorKind::InvalidRequest,
            ))
        }
    }
    pub fn expect_as_removing_mut(
        &mut self,
    ) -> Result<&mut Model<PackageDataEntryRemoving>, Error> {
        if let PackageDataEntryMatchModelMut::Removing(a) = self.as_match_mut() {
            Ok(a)
        } else {
            Err(Error::new(
                eyre!("package is not in removing state"),
                ErrorKind::InvalidRequest,
            ))
        }
    }
    pub fn expect_as_installing_mut(
        &mut self,
    ) -> Result<&mut Model<PackageDataEntryInstalling>, Error> {
        if let PackageDataEntryMatchModelMut::Installing(a) = self.as_match_mut() {
            Ok(a)
        } else {
            Err(Error::new(
                eyre!("package is not in installing state"),
                ErrorKind::InvalidRequest,
            ))
        }
    }
    pub fn into_manifest(self) -> Model<Manifest> {
        match self.into_match() {
            PackageDataEntryMatchModel::Installing(a) => a.into_manifest(),
            PackageDataEntryMatchModel::Updating(a) => a.into_installed().into_manifest(),
            PackageDataEntryMatchModel::Restoring(a) => a.into_manifest(),
            PackageDataEntryMatchModel::Removing(a) => a.into_manifest(),
            PackageDataEntryMatchModel::Installed(a) => a.into_manifest(),
            PackageDataEntryMatchModel::Error(_) => Model::from(Value::Null),
        }
    }
    pub fn as_manifest(&self) -> &Model<Manifest> {
        match self.as_match() {
            PackageDataEntryMatchModelRef::Installing(a) => a.as_manifest(),
            PackageDataEntryMatchModelRef::Updating(a) => a.as_installed().as_manifest(),
            PackageDataEntryMatchModelRef::Restoring(a) => a.as_manifest(),
            PackageDataEntryMatchModelRef::Removing(a) => a.as_manifest(),
            PackageDataEntryMatchModelRef::Installed(a) => a.as_manifest(),
            PackageDataEntryMatchModelRef::Error(_) => (&Value::Null).into(),
        }
    }
    pub fn into_installed(self) -> Option<Model<InstalledPackageInfo>> {
        match self.into_match() {
            PackageDataEntryMatchModel::Installing(_) => None,
            PackageDataEntryMatchModel::Updating(a) => Some(a.into_installed()),
            PackageDataEntryMatchModel::Restoring(_) => None,
            PackageDataEntryMatchModel::Removing(_) => None,
            PackageDataEntryMatchModel::Installed(a) => Some(a.into_installed()),
            PackageDataEntryMatchModel::Error(_) => None,
        }
    }
    pub fn as_installed(&self) -> Option<&Model<InstalledPackageInfo>> {
        match self.as_match() {
            PackageDataEntryMatchModelRef::Installing(_) => None,
            PackageDataEntryMatchModelRef::Updating(a) => Some(a.as_installed()),
            PackageDataEntryMatchModelRef::Restoring(_) => None,
            PackageDataEntryMatchModelRef::Removing(_) => None,
            PackageDataEntryMatchModelRef::Installed(a) => Some(a.as_installed()),
            PackageDataEntryMatchModelRef::Error(_) => None,
        }
    }
    pub fn as_installed_mut(&mut self) -> Option<&mut Model<InstalledPackageInfo>> {
        match self.as_match_mut() {
            PackageDataEntryMatchModelMut::Installing(_) => None,
            PackageDataEntryMatchModelMut::Updating(a) => Some(a.as_installed_mut()),
            PackageDataEntryMatchModelMut::Restoring(_) => None,
            PackageDataEntryMatchModelMut::Removing(_) => None,
            PackageDataEntryMatchModelMut::Installed(a) => Some(a.as_installed_mut()),
            PackageDataEntryMatchModelMut::Error(_) => None,
        }
    }
    pub fn as_install_progress(&self) -> Option<&Model<FullProgress>> {
        match self.as_match() {
            PackageDataEntryMatchModelRef::Installing(a) => Some(a.as_install_progress()),
            PackageDataEntryMatchModelRef::Updating(a) => Some(a.as_install_progress()),
            PackageDataEntryMatchModelRef::Restoring(a) => Some(a.as_install_progress()),
            PackageDataEntryMatchModelRef::Removing(_) => None,
            PackageDataEntryMatchModelRef::Installed(_) => None,
            PackageDataEntryMatchModelRef::Error(_) => None,
        }
    }
    pub fn as_install_progress_mut(&mut self) -> Option<&mut Model<FullProgress>> {
        match self.as_match_mut() {
            PackageDataEntryMatchModelMut::Installing(a) => Some(a.as_install_progress_mut()),
            PackageDataEntryMatchModelMut::Updating(a) => Some(a.as_install_progress_mut()),
            PackageDataEntryMatchModelMut::Restoring(a) => Some(a.as_install_progress_mut()),
            PackageDataEntryMatchModelMut::Removing(_) => None,
            PackageDataEntryMatchModelMut::Installed(_) => None,
            PackageDataEntryMatchModelMut::Error(_) => None,
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
    pub developer_key: ed25519_dalek::VerifyingKey,
    pub manifest: Manifest,
    pub last_backup: Option<DateTime<Utc>>,
    pub dependency_info: BTreeMap<PackageId, StaticDependencyInfo>,
    pub current_dependents: CurrentDependents,
    pub current_dependencies: CurrentDependencies,
    pub interface_addresses: InterfaceAddressMap,
    pub hosts: HostInfo,
    pub store: Value,
    pub store_exposed_ui: Vec<ExposedUI>,
    pub store_exposed_dependents: Vec<JsonPointer>,
}
#[derive(Debug, Deserialize, Serialize, HasModel)]
#[model = "Model<Self>"]
pub struct ExposedDependent {
    path: String,
    title: String,
    description: Option<String>,
    masked: Option<bool>,
    copyable: Option<bool>,
    qr: Option<bool>,
}
#[derive(Clone, Debug, Deserialize, Serialize, HasModel)]
#[model = "Model<Self>"]
pub struct ExposedUI {
    path: Vec<JsonPointer>,
    title: String,
    description: Option<String>,
    masked: Option<bool>,
    copyable: Option<bool>,
    qr: Option<bool>,
}

#[derive(Debug, Clone, Default, Deserialize, Serialize)]
pub struct CurrentDependents(pub BTreeMap<PackageId, CurrentDependencyInfo>);
impl CurrentDependents {
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
impl Map for CurrentDependents {
    type Key = PackageId;
    type Value = CurrentDependencyInfo;
    fn key_str(key: &Self::Key) -> Result<impl AsRef<str>, Error> {
        Ok(key)
    }
    fn key_string(key: &Self::Key) -> Result<InternedString, Error> {
        Ok(key.clone().into())
    }
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
    fn key_str(key: &Self::Key) -> Result<impl AsRef<str>, Error> {
        Ok(key)
    }
    fn key_string(key: &Self::Key) -> Result<InternedString, Error> {
        Ok(key.clone().into())
    }
}

#[derive(Debug, Deserialize, Serialize, HasModel)]
#[serde(rename_all = "kebab-case")]
#[model = "Model<Self>"]
pub struct StaticDependencyInfo {
    pub title: String,
    pub icon: DataUrl<'static>,
}

#[derive(Clone, Debug, Default, Deserialize, Serialize, HasModel)]
#[serde(rename_all = "kebab-case")]
#[model = "Model<Self>"]
pub struct CurrentDependencyInfo {
    #[serde(default)]
    pub health_checks: BTreeSet<HealthCheckId>,
}

#[derive(Debug, Default, Deserialize, Serialize)]
pub struct InterfaceAddressMap(pub BTreeMap<HostId, InterfaceAddresses>);
impl Map for InterfaceAddressMap {
    type Key = HostId;
    type Value = InterfaceAddresses;
    fn key_str(key: &Self::Key) -> Result<impl AsRef<str>, Error> {
        Ok(key)
    }
    fn key_string(key: &Self::Key) -> Result<InternedString, Error> {
        Ok(key.clone().into())
    }
}

#[derive(Debug, Deserialize, Serialize, HasModel)]
#[serde(rename_all = "kebab-case")]
#[model = "Model<Self>"]
pub struct InterfaceAddresses {
    pub tor_address: Option<String>,
    pub lan_address: Option<String>,
}
