use std::collections::BTreeSet;

use crate::prelude::*;
use crate::s9pk::manifest::{Manifest, PackageId};

pub async fn get_packages(db: &PatchDb) -> Result<imbl::OrdSet<PackageId>, Error> {
    Ok(db.peek().await?.package_data().keys()?)
}

pub async fn get_manifest(db: &PatchDb, pkg: &PackageId) -> Result<Option<Manifest>, Error> {
    db.peek()
        .await?
        .package_data()
        .idx(pkg)
        .map(|pde| pde.manifest().de())
        .transpose()
}
