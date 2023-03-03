use std::collections::BTreeMap;
use std::ops::{Deref, DerefMut};
use std::path::{Path, PathBuf};

pub use helpers::script_dir;
pub use models::VolumeId;
use serde::{Deserialize, Serialize};
use tracing::instrument;

use crate::context::RpcContext;
use crate::net::interface::{InterfaceId, Interfaces};
use crate::net::PACKAGE_CERT_PATH;
use crate::prelude::*;
use crate::s9pk::manifest::PackageId;
use crate::util::Version;

pub const PKG_VOLUME_DIR: &str = "package-data/volumes";
pub const BACKUP_DIR: &str = "/media/embassy/backups";

#[derive(Clone, Debug, Default, Deserialize, Serialize)]
pub struct Volumes(BTreeMap<VolumeId, Volume>);
impl Volumes {
    #[instrument]
    pub fn validate(&self, interfaces: &Interfaces) -> Result<(), Error> {
        for (id, volume) in &self.0 {
            volume
                .validate(interfaces)
                .with_ctx(|_| (ErrorKind::ValidateS9pk, format!("Volume {}", id)))?;
        }
        Ok(())
    }
    #[instrument(skip(ctx))]
    pub async fn install(
        &self,
        ctx: &RpcContext,
        pkg_id: &PackageId,
        version: &Version,
    ) -> Result<(), Error> {
        for (volume_id, volume) in &self.0 {
            volume
                .install(&ctx.datadir, pkg_id, version, volume_id)
                .await?; // TODO: concurrent?
        }
        Ok(())
    }
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

#[derive(Clone, Debug, Deserialize, Serialize, HasModel)]
#[serde(rename_all = "kebab-case")]
#[model = "Model<Self>"]
pub struct VolumeData {
    #[serde(skip)]
    pub readonly: bool,
}
#[derive(Clone, Debug, Deserialize, Serialize, HasModel)]
#[serde(rename_all = "kebab-case")]
#[model = "Model<Self>"]
pub struct VolumeAssets {}

#[derive(Clone, Debug, Deserialize, Serialize, HasModel)]
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

#[derive(Clone, Debug, Deserialize, Serialize, HasModel)]
#[serde(rename_all = "kebab-case")]
#[model = "Model<Self>"]
pub struct VolumeCertificate {
    pub interface_id: InterfaceId,
}

#[derive(Clone, Debug, Deserialize, Serialize, HasModel)]
#[serde(rename_all = "kebab-case")]
#[model = "Model<Self>"]
pub struct VolumeBackup {
    pub readonly: bool,
}

#[derive(Clone, Debug, Deserialize, Serialize, HasModel)]
#[serde(tag = "type")]
#[serde(rename_all = "kebab-case")]
#[model = "Model<Self>"]
pub enum Volume {
    Data(VolumeData),
    Assets(VolumeAssets),
    Pointer(VolumePointer),
    Certificate(VolumeCertificate),
    #[serde(skip)]
    Backup(VolumeBackup),
}
impl Volume {
    #[instrument]
    pub fn validate(&self, interfaces: &Interfaces) -> Result<(), color_eyre::eyre::Report> {
        match self {
            Volume::Certificate(VolumeCertificate { interface_id }) => {
                if !interfaces.0.contains_key(interface_id) {
                    color_eyre::eyre::bail!("unknown interface: {}", interface_id);
                }
            }
            _ => (),
        }
        Ok(())
    }
    pub async fn install(
        &self,
        path: &PathBuf,
        pkg_id: &PackageId,
        version: &Version,
        volume_id: &VolumeId,
    ) -> Result<(), Error> {
        match self {
            Volume::Data { .. } => {
                tokio::fs::create_dir_all(self.path_for(path, pkg_id, version, volume_id)).await?;
            }
            _ => (),
        }
        Ok(())
    }
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
            Volume::Certificate(VolumeCertificate { interface_id }) => {
                cert_dir(pkg_id, &interface_id)
            }
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
            Volume::Certificate(VolumeCertificate { .. }) => true,
            Volume::Backup(VolumeBackup { readonly }) => *readonly,
        }
    }
}
