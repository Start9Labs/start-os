use std::path::{Path, PathBuf};

pub use helpers::script_dir;
pub use models::VolumeId;
use models::{HostId, PackageId};

use crate::net::PACKAGE_CERT_PATH;
use crate::prelude::*;
use crate::util::VersionString;

pub const PKG_VOLUME_DIR: &str = "package-data/volumes";
pub const BACKUP_DIR: &str = "/media/startos/backups";

pub fn data_dir<P: AsRef<Path>>(datadir: P, pkg_id: &PackageId, volume_id: &VolumeId) -> PathBuf {
    datadir
        .as_ref()
        .join(PKG_VOLUME_DIR)
        .join(pkg_id)
        .join("data")
        .join(volume_id)
}

pub fn asset_dir<P: AsRef<Path>>(datadir: P, pkg_id: &PackageId, version: &VersionString) -> PathBuf {
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
