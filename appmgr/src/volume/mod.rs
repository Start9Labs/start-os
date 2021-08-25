use std::borrow::Borrow;
use std::ops::{Deref, DerefMut};
use std::path::{Path, PathBuf};

use indexmap::IndexMap;
use patch_db::{HasModel, Map, MapModel};
use serde::{Deserialize, Deserializer, Serialize};

use crate::id::{Id, IdUnchecked};
use crate::net::interface::InterfaceId;
use crate::s9pk::manifest::PackageId;
use crate::util::Version;
use crate::Error;

pub mod disk;

pub const PKG_VOLUME_DIR: &'static str = "/mnt/embassy-os/volumes/package-data";
pub const BACKUP_DIR: &'static str = "/mnt/embassy-os-backups/EmbassyBackups";

#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash)]
pub enum VolumeId<S: AsRef<str> = String> {
    Backup,
    Custom(Id<S>),
}
impl<S: AsRef<str>> std::fmt::Display for VolumeId<S> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            VolumeId::Backup => write!(f, "BACKUP"),
            VolumeId::Custom(id) => write!(f, "{}", id),
        }
    }
}
impl<S: AsRef<str>> AsRef<str> for VolumeId<S> {
    fn as_ref(&self) -> &str {
        match self {
            VolumeId::Backup => "BACKUP",
            VolumeId::Custom(id) => id.as_ref(),
        }
    }
}
impl<S: AsRef<str>> Borrow<str> for VolumeId<S> {
    fn borrow(&self) -> &str {
        self.as_ref()
    }
}
impl<S: AsRef<str>> AsRef<Path> for VolumeId<S> {
    fn as_ref(&self) -> &Path {
        AsRef::<str>::as_ref(self).as_ref()
    }
}
impl<'de, S> Deserialize<'de> for VolumeId<S>
where
    S: AsRef<str>,
    IdUnchecked<S>: Deserialize<'de>,
{
    fn deserialize<D>(deserializer: D) -> Result<Self, D::Error>
    where
        D: Deserializer<'de>,
    {
        let unchecked: IdUnchecked<S> = Deserialize::deserialize(deserializer)?;
        Ok(match unchecked.0.as_ref() {
            "BACKUP" => VolumeId::Backup,
            _ => VolumeId::Custom(Id::try_from(unchecked.0).map_err(serde::de::Error::custom)?),
        })
    }
}
impl<S: AsRef<str>> Serialize for VolumeId<S> {
    fn serialize<Ser>(&self, serializer: Ser) -> Result<Ser::Ok, Ser::Error>
    where
        Ser: serde::Serializer,
    {
        serializer.serialize_str(self.as_ref())
    }
}

#[derive(Clone, Debug, Default, Deserialize, Serialize)]
pub struct Volumes(IndexMap<VolumeId, Volume>);
impl Volumes {
    pub async fn install(&self, pkg_id: &PackageId, version: &Version) -> Result<(), Error> {
        for (volume_id, volume) in &self.0 {
            volume.install(pkg_id, version, volume_id).await?; // TODO: concurrent?
        }
        Ok(())
    }
    pub fn get_path_for(
        &self,
        pkg_id: &PackageId,
        version: &Version,
        volume_id: &VolumeId,
    ) -> Option<PathBuf> {
        self.0
            .get(volume_id)
            .map(|volume| volume.path_for(pkg_id, version, volume_id))
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
    type Target = IndexMap<VolumeId, Volume>;
    fn deref(&self) -> &Self::Target {
        &self.0
    }
}
impl DerefMut for Volumes {
    fn deref_mut(&mut self) -> &mut Self::Target {
        &mut self.0
    }
}
impl Map for Volumes {
    type Key = VolumeId;
    type Value = Volume;
    fn get(&self, key: &Self::Key) -> Option<&Self::Value> {
        self.0.get(key)
    }
}
impl HasModel for Volumes {
    type Model = MapModel<Self>;
}

#[derive(Clone, Debug, Deserialize, Serialize, HasModel)]
#[serde(tag = "type")]
#[serde(rename_all = "kebab-case")]
pub enum Volume {
    #[serde(rename_all = "kebab-case")]
    Data {
        #[serde(skip)]
        readonly: bool,
    },
    #[serde(rename_all = "kebab-case")]
    Assets {},
    #[serde(rename_all = "kebab-case")]
    Pointer {
        package_id: PackageId,
        volume_id: VolumeId,
        path: PathBuf,
        readonly: bool,
    },
    #[serde(rename_all = "kebab-case")]
    Certificate { interface_id: InterfaceId },
    #[serde(rename_all = "kebab-case")]
    #[serde(skip)]
    Backup { readonly: bool },
}
impl Volume {
    pub async fn install(
        &self,
        pkg_id: &PackageId,
        version: &Version,
        volume_id: &VolumeId,
    ) -> Result<(), Error> {
        match self {
            Volume::Data { .. } => {
                tokio::fs::create_dir_all(self.path_for(pkg_id, version, volume_id)).await?;
            }
            _ => (),
        }
        Ok(())
    }
    pub fn path_for(&self, pkg_id: &PackageId, version: &Version, volume_id: &VolumeId) -> PathBuf {
        match self {
            Volume::Data { .. } => Path::new(PKG_VOLUME_DIR)
                .join(pkg_id)
                .join("volumes")
                .join(volume_id),
            Volume::Assets {} => Path::new(PKG_VOLUME_DIR)
                .join(pkg_id)
                .join("assets")
                .join(version.as_str())
                .join(volume_id),
            Volume::Pointer {
                package_id,
                volume_id,
                path,
                ..
            } => dbg!(Path::new(PKG_VOLUME_DIR)
                .join(package_id)
                .join("volumes")
                .join(volume_id)
                .join(if path.is_absolute() {
                    path.strip_prefix("/").unwrap()
                } else {
                    path.as_ref()
                })),
            Volume::Certificate { interface_id } => Path::new(PKG_VOLUME_DIR)
                .join(pkg_id)
                .join("certificates")
                .join(interface_id),
            Volume::Backup { .. } => Path::new(BACKUP_DIR).join(pkg_id),
        }
    }
    pub fn set_readonly(&mut self) {
        match self {
            Volume::Data { readonly } => {
                *readonly = true;
            }
            Volume::Pointer { readonly, .. } => {
                *readonly = true;
            }
            Volume::Backup { readonly } => {
                *readonly = true;
            }
            _ => (),
        }
    }
    pub fn readonly(&self) -> bool {
        match self {
            Volume::Data { readonly } => *readonly,
            Volume::Assets {} => true,
            Volume::Pointer { readonly, .. } => *readonly,
            Volume::Certificate { .. } => true,
            Volume::Backup { readonly } => *readonly,
        }
    }
}
