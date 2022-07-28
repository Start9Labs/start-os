use std::collections::BTreeMap;
use std::ops::{Deref, DerefMut};
use std::path::{Path, PathBuf};

pub use helpers::script_dir;
pub use models::VolumeId;
use patch_db::{HasModel, Map, MapModel};
use serde::{Deserialize, Serialize};
use tracing::instrument;

use crate::context::RpcContext;
use crate::net::interface::{InterfaceId, Interfaces};
use crate::net::NetController;
use crate::s9pk::manifest::PackageId;
use crate::util::Version;
use crate::{Error, ResultExt};

pub const PKG_VOLUME_DIR: &str = "package-data/volumes";
pub const BACKUP_DIR: &str = "/media/embassy-os/backups";

#[derive(Clone, Debug, Default, Deserialize, Serialize)]
pub struct Volumes(BTreeMap<VolumeId, Volume>);
impl Volumes {
    #[instrument]
    pub fn validate(&self, interfaces: &Interfaces) -> Result<(), Error> {
        for (id, volume) in &self.0 {
            volume
                .validate(interfaces)
                .with_ctx(|_| (crate::ErrorKind::ValidateS9pk, format!("Volume {}", id)))?;
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
impl Map for Volumes {
    type Key = VolumeId;
    type Value = Volume;
    fn get(&self, key: &Self::Key) -> Option<&Self::Value> {
        self.0.get(key)
    }
}
pub type VolumesModel = MapModel<Volumes>;
impl HasModel for Volumes {
    type Model = MapModel<Self>;
}

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
    #[instrument]
    pub fn validate(&self, interfaces: &Interfaces) -> Result<(), color_eyre::eyre::Report> {
        match self {
            Volume::Certificate { interface_id } => {
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
            Volume::Data { .. } => data_dir(&data_dir_path, pkg_id, volume_id),
            Volume::Assets {} => asset_dir(&data_dir_path, pkg_id, version).join(volume_id),
            Volume::Pointer {
                package_id,
                volume_id,
                path,
                ..
            } => data_dir(&data_dir_path, package_id, volume_id).join(if path.is_absolute() {
                path.strip_prefix("/").unwrap()
            } else {
                path.as_ref()
            }),
            Volume::Certificate { interface_id: _ } => NetController::ssl_directory_for(pkg_id),
            Volume::Backup { .. } => backup_dir(pkg_id),
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
