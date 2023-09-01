use crate::prelude::*;
use crate::s9pk::manifest::{Manifest, PackageId};

pub fn get_packages(db: &Peeked) -> Result<Vec<PackageId>, Error> {
    Ok(db.as_package_data().keys()?)
}

pub fn get_manifest(db: &Peeked, pkg: &PackageId) -> Result<Option<Manifest>, Error> {
    db.into_package_data()
        .into_idx(pkg)
        .map(|pde| pde.into_manifest().de())
        .transpose()
}
