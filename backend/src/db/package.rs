use crate::prelude::*;
use crate::s9pk::manifest::{Manifest, PackageId};

pub async fn get_packages(db: &PatchDb) -> Result<Vec<PackageId>, Error> {
    Ok(db.peek().await?.as_package_data().keys()?)
}

pub async fn get_manifest(db: &PatchDb, pkg: &PackageId) -> Result<Option<Manifest>, Error> {
    db.peek()
        .await?
        .into_package_data()
        .into_idx(pkg)
        .map(|pde| pde.into_manifest().de())
        .transpose()
}
