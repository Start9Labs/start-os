use std::collections::BTreeMap;
use std::ops::{Deref, DerefMut};
use std::path::{Path, PathBuf};

pub use helpers::script_dir;
use imbl_value::InternedString;
pub use models::VolumeId;
use models::{HostId, PackageId};
use serde::{Deserialize, Serialize};
use tracing::instrument;

use crate::context::RpcContext;
use crate::net::PACKAGE_CERT_PATH;
use crate::prelude::*;
use crate::util::Version;

pub const PKG_VOLUME_DIR: &str = "package-data/volumes";
pub const BACKUP_DIR: &str = "/media/embassy/backups";

#[derive(Clone, Debug, Default, Deserialize, Serialize)]
pub struct Volumes(BTreeMap<VolumeId, Volume>);
impl Volumes {
    #[instrument(skip_all)]
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
    fn key_str(key: &Self::Key) -> Result<impl AsRef<str>, Error> {
        Ok(key)
    }
    fn key_string(key: &Self::Key) -> Result<InternedString, Error> {
        match key {
            VolumeId::Custom(id) => Ok(id.clone().into()),
            _ => Self::key_str(key).map(|s| InternedString::intern(s.as_ref())),
        }
    }
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

pub fn cert_dir(pkg_id: &PackageId, host_id: &HostId) -> PathBuf {
    Path::new(PACKAGE_CERT_PATH).join(pkg_id).join(host_id)
}

#[derive(Clone, Debug, Deserialize, Serialize)]
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
    Certificate { interface_id: HostId },
    #[serde(rename_all = "kebab-case")]
    Backup { readonly: bool },
}
impl Volume {
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
            Volume::Certificate { interface_id } => cert_dir(pkg_id, &interface_id),
            Volume::Backup { .. } => backup_dir(pkg_id),
        }
    }

    pub fn pointer_path(&self, data_dir_path: impl AsRef<Path>) -> Option<PathBuf> {
        if let Volume::Pointer {
            path,
            package_id,
            volume_id,
            ..
        } = self
        {
            Some(
                data_dir(data_dir_path.as_ref(), package_id, volume_id).join(
                    if path.is_absolute() {
                        path.strip_prefix("/").unwrap()
                    } else {
                        path.as_ref()
                    },
                ),
            )
        } else {
            None
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
