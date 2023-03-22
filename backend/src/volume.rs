use std::collections::BTreeMap;
use std::ops::{Deref, DerefMut};
use std::path::{Path, PathBuf};

pub use helpers::script_dir;
use models::InterfaceId;
pub use models::VolumeId;
use serde::{Deserialize, Serialize};

use crate::net::PACKAGE_CERT_PATH;
use crate::prelude::*;
use crate::s9pk::manifest::PackageId;
use crate::util::Version;

pub const PKG_VOLUME_DIR: &str = "package-data/volumes";
pub const BACKUP_DIR: &str = "/media/embassy/backups";

#[derive(Clone, Debug, Default, Deserialize, Serialize, PartialEq, Eq)]
pub struct Volumes(BTreeMap<VolumeId, Volume>);
impl Volumes {
    pub fn get_path_for(
        &self,
        path: &PathBuf,
        pkg_id: &PackageId,
        version: &Version,
        volume_id: &VolumeId,
    ) -> Option<PathBuf> {
        self.0
            .get(volume_id)
            .map(|volume| volume.path_for(path, pkg_id, version, volume_id))
    }
    pub fn to_readonly(&self) -> Self {
        Volumes(
            self.0
                .iter()
                .map(|(id, volume)| {
                    let mut volume = volume.clone();
                    volume.set_readonly();
                    (id.clone(), volume)
                })
                .collect(),
        )
    }
}
impl Deref for Volumes {
    type Target = BTreeMap<VolumeId, Volume>;
    fn deref(&self) -> &Self::Target {
        &self.0
    }
}
impl DerefMut for Volumes {
    fn deref_mut(&mut self) -> &mut Self::Target {
        &mut self.0
    }
}
// impl Map for Volumes {
//     type Key = VolumeId;
//     type Value = Volume;
// }
// impl HasModel for Volumes {
//     type Model = MapModel<Self>;
// }

pub fn data_dir<P: AsRef<Path>>(datadir: P, pkg_id: &PackageId, volume_id: &VolumeId) -> PathBuf {
    datadir
        .as_ref()
        .join(PKG_VOLUME_DIR)
        .join(pkg_id)
        .join("data")
        .join(volume_id)
}

pub fn asset_dir<P: AsRef<Path>>(datadir: P, pkg_id: &PackageId, version: &Version) -> PathBuf {
    datadir
        .as_ref()
        .join(PKG_VOLUME_DIR)
        .join(pkg_id)
        .join("assets")
        .join(version.as_str())
}

pub fn backup_dir(pkg_id: &PackageId) -> PathBuf {
    Path::new(BACKUP_DIR).join(pkg_id).join("data")
}

pub fn cert_dir(pkg_id: &PackageId, interface_id: &InterfaceId) -> PathBuf {
    Path::new(PACKAGE_CERT_PATH).join(pkg_id).join(interface_id)
}

#[derive(Clone, Debug, Deserialize, Serialize, HasModel, PartialEq, Eq)]
#[serde(rename_all = "kebab-case")]
#[model = "Model<Self>"]
pub struct VolumeData {
    #[serde(skip)]
    pub readonly: bool,
}
#[derive(Clone, Debug, Deserialize, Serialize, HasModel, PartialEq, Eq)]
#[serde(rename_all = "kebab-case")]
#[model = "Model<Self>"]
pub struct VolumeAssets {}

#[derive(Clone, Debug, Deserialize, Serialize, HasModel, PartialEq, Eq)]
#[serde(rename_all = "kebab-case")]
#[model = "Model<Self>"]
pub struct VolumePointer {
    pub package_id: PackageId,
    pub volume_id: VolumeId,
    pub path: PathBuf,
    pub readonly: bool,
}
impl VolumePointer {
    pub fn path(&self, datadir: impl AsRef<Path>) -> PathBuf {
        data_dir(datadir.as_ref(), &self.package_id, &self.volume_id).join(
            if self.path.is_absolute() {
                self.path.strip_prefix("/").unwrap()
            } else {
                self.path.as_ref()
            },
        )
    }
}

#[derive(Clone, Debug, Deserialize, Serialize, HasModel, PartialEq, Eq)]
#[serde(rename_all = "kebab-case")]
#[model = "Model<Self>"]
pub struct VolumeBackup {
    pub readonly: bool,
}

#[derive(Clone, Debug, Deserialize, Serialize, HasModel, PartialEq, Eq)]
#[serde(tag = "type")]
#[serde(rename_all = "kebab-case")]
#[model = "Model<Self>"]
pub enum Volume {
    Data(VolumeData),
    Assets(VolumeAssets),
    Pointer(VolumePointer),
    #[serde(skip)]
    Backup(VolumeBackup),
}
impl Volume {
    pub fn path_for(
        &self,
        data_dir_path: impl AsRef<Path>,
        pkg_id: &PackageId,
        version: &Version,
        volume_id: &VolumeId,
    ) -> PathBuf {
        match self {
            Volume::Data(_) => data_dir(data_dir_path, pkg_id, volume_id),
            Volume::Assets(_) => asset_dir(data_dir_path, pkg_id, version).join(volume_id),
            Volume::Pointer(p) => p.path(data_dir_path),
            Volume::Backup(_) => backup_dir(pkg_id),
        }
    }

    pub fn set_readonly(&mut self) {
        match self {
            Volume::Data(VolumeData { readonly }) => {
                *readonly = true;
            }
            Volume::Pointer(VolumePointer { readonly, .. }) => {
                *readonly = true;
            }
            Volume::Backup(VolumeBackup { readonly }) => {
                *readonly = true;
            }
            _ => (),
        }
    }
    pub fn readonly(&self) -> bool {
        match self {
            Volume::Data(VolumeData { readonly }) => *readonly,
            Volume::Assets(VolumeAssets {}) => true,
            Volume::Pointer(VolumePointer { readonly, .. }) => *readonly,
            Volume::Backup(VolumeBackup { readonly }) => *readonly,
        }
    }
}
