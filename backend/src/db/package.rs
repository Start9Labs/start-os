use models::Version;

use crate::prelude::*;
use crate::s9pk::manifest::{Manifest, PackageId};

pub fn get_packages(db: Peeked) -> Result<Vec<(PackageId, Version)>, Error> {
    Ok(db
        .as_package_data()
        .keys()?
        .into_iter()
        .flat_map(|package_id| {
            let version = db
                .as_package_data()
                .as_idx(&package_id)?
                .as_manifest()
                .as_version()
                .de()
                .ok()?;
            Some((package_id, version))
        })
        .collect())
}

pub fn get_manifest(db: &Peeked, pkg: &PackageId) -> Result<Option<Manifest>, Error> {
    db.into_package_data()
        .into_idx(pkg)
        .map(|pde| pde.into_manifest().de())
        .transpose()
}
