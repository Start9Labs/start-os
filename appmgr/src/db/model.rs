use std::net::Ipv4Addr;
use std::sync::Arc;

use indexmap::{IndexMap, IndexSet};
use patch_db::json_ptr::JsonPointer;
use patch_db::{HasModel, Map, MapModel, OptionModel};
use reqwest::Url;
use serde::{Deserialize, Serialize};
use serde_json::Value;

use crate::config::spec::{PackagePointerSpecVariant, SystemPointerSpec};
use crate::id::InterfaceId;
use crate::install::progress::InstallProgress;
use crate::net::Network;
use crate::s9pk::manifest::{Manifest, PackageId};
use crate::status::health_check::HealthCheckId;
use crate::status::Status;
use crate::util::Version;
use crate::Error;

#[derive(Debug, Deserialize, Serialize, HasModel)]
#[serde(rename_all = "kebab-case")]
pub struct Database {
    #[model]
    pub server_info: ServerInfo,
    #[model]
    pub package_data: AllPackageData,
    pub broken_packages: Vec<PackageId>,
    #[model]
    pub network: Network,
    pub ui: Value,
}
impl Database {
    pub fn init() -> Self {
        // TODO
        Database {
            server_info: ServerInfo {
                id: "c3ad21d8".to_owned(),
                version: emver::Version::new(0, 3, 0, 0).into(),
                lan_address: "https://start9-c3ad21d8.local".parse().unwrap(),
                tor_address:
                    "http://privacy34kn4ez3y3nijweec6w4g54i3g54sdv7r5mr6soma3w4begyd.onion"
                        .parse()
                        .unwrap(),
                updating: false,
                registry: "https://registry.start9.com".parse().unwrap(),
                unread_notification_count: 0,
            },
            package_data: AllPackageData::default(),
            broken_packages: Vec::new(),
            network: Network::default(),
            ui: Value::Object(Default::default()),
        }
    }
}
impl DatabaseModel {
    pub fn new() -> Self {
        Self::from(JsonPointer::default())
    }
}

#[derive(Debug, Deserialize, Serialize, HasModel)]
#[serde(rename_all = "kebab-case")]
pub struct ServerInfo {
    id: String,
    version: Version,
    lan_address: Url,
    tor_address: Url,
    updating: bool,
    registry: Url,
    unread_notification_count: u64,
}

#[derive(Debug, Default, Deserialize, Serialize)]
pub struct AllPackageData(pub IndexMap<PackageId, PackageDataEntry>);
impl Map for AllPackageData {
    type Key = PackageId;
    type Value = PackageDataEntry;
    fn get(&self, key: &Self::Key) -> Option<&Self::Value> {
        self.0.get(key)
    }
}
impl HasModel for AllPackageData {
    type Model = MapModel<Self>;
}

#[derive(Debug, Deserialize, Serialize)]
#[serde(rename_all = "kebab-case")]
pub struct StaticFiles {
    license: Url,
    instructions: Url,
    icon: Url,
}
impl StaticFiles {
    pub fn local(id: &PackageId, version: &Version, icon_type: &str) -> Result<Self, Error> {
        Ok(StaticFiles {
            license: format!("/public/package-data/{}/{}/LICENSE.md", id, version).parse()?,
            instructions: format!("/public/package-data/{}/{}/INSTRUCTIONS.md", id, version)
                .parse()?,
            icon: format!("/public/package-data/{}/{}/icon.{}", id, version, icon_type).parse()?,
        })
    }
    pub fn remote(id: &PackageId, version: &Version, icon_type: &str) -> Result<Self, Error> {
        Ok(StaticFiles {
            license: format!("/registry/packages/{}/{}/LICENSE.md", id, version).parse()?,
            instructions: format!("/registry/packages/{}/{}/INSTRUCTIONS.md", id, version)
                .parse()?,
            icon: format!("/registry/packages/{}/{}/icon.{}", id, version, icon_type).parse()?,
        })
    }
}

#[derive(Debug, Deserialize, Serialize, HasModel)]
#[serde(tag = "state")]
#[serde(rename_all = "kebab-case")]
pub enum PackageDataEntry {
    #[serde(rename_all = "kebab-case")]
    Installing {
        static_files: StaticFiles,
        temp_manifest: Manifest,
        install_progress: Arc<InstallProgress>,
    }, // { state: "installing", 'install-progress': InstallProgress }
    #[serde(rename_all = "kebab-case")]
    Updating {
        static_files: StaticFiles,
        temp_manifest: Manifest,
        installed: InstalledPackageDataEntry,
        install_progress: Arc<InstallProgress>,
    },
    #[serde(rename_all = "kebab-case")]
    Removing {
        static_files: StaticFiles,
        temp_manifest: Manifest,
    },
    #[serde(rename_all = "kebab-case")]
    Installed {
        static_files: StaticFiles,
        installed: InstalledPackageDataEntry,
    },
}
impl PackageDataEntryModel {
    pub fn installed(self) -> OptionModel<InstalledPackageDataEntry> {
        self.0.child("installed").into()
    }
    pub fn install_progress(self) -> OptionModel<InstallProgress> {
        self.0.child("install-progress").into()
    }
}

#[derive(Debug, Deserialize, Serialize, HasModel)]
#[serde(rename_all = "kebab-case")]
pub struct InstalledPackageDataEntry {
    #[model]
    pub manifest: Manifest,
    #[model]
    pub status: Status,
    pub system_pointers: Vec<SystemPointerSpec>,
    #[model]
    pub current_dependents: IndexMap<PackageId, CurrentDependencyInfo>,
    #[model]
    pub current_dependencies: IndexMap<PackageId, CurrentDependencyInfo>,
    #[model]
    pub interface_info: InterfaceInfo,
}

#[derive(Clone, Debug, Default, Deserialize, Serialize, HasModel)]
#[serde(rename_all = "kebab-case")]
pub struct CurrentDependencyInfo {
    pub pointers: Vec<PackagePointerSpecVariant>,
    pub health_checks: IndexSet<HealthCheckId>,
}

#[derive(Debug, Deserialize, Serialize, HasModel)]
#[serde(rename_all = "kebab-case")]
pub struct InterfaceInfo {
    pub ip: Ipv4Addr,
    #[model]
    pub addresses: InterfaceAddressMap,
}

#[derive(Debug, Deserialize, Serialize)]
pub struct InterfaceAddressMap(pub IndexMap<InterfaceId, InterfaceAddresses>);
impl Map for InterfaceAddressMap {
    type Key = InterfaceId;
    type Value = InterfaceAddresses;
    fn get(&self, key: &Self::Key) -> Option<&Self::Value> {
        self.0.get(key)
    }
}
impl HasModel for InterfaceAddressMap {
    type Model = MapModel<Self>;
}

#[derive(Debug, Deserialize, Serialize, HasModel)]
#[serde(rename_all = "kebab-case")]
pub struct InterfaceAddresses {
    pub tor_address: Option<String>,
    pub lan_address: Option<String>,
}
