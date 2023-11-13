use std::path::{Path, PathBuf};

use models::{PackageId, Version};

pub const PKG_SCRIPT_DIR: &str = "package-data/scripts";

pub fn script_dir<P: AsRef<Path>>(datadir: P, pkg_id: &PackageId, version: &Version) -> PathBuf {
    datadir
        .as_ref()
        .join(&*PKG_SCRIPT_DIR)
        .join(pkg_id)
        .join(version.as_str())
}
