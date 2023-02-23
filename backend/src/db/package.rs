use std::collections::BTreeSet;

use crate::prelude::*;
use crate::s9pk::manifest::{Manifest, PackageId};
use crate::ResultExt;

pub async fn get_packages(db: &PatchDb) -> Result<BTreeSet<PackageId>, Error> {
    db.apply_fn(|mut v| {
        v.package_data()
            .keys()
            .with_kind(ErrorKind::Deserialization)
    })
    .await
}

pub async fn get_manifest(db: &PatchDb, pkg: &PackageId) -> Result<Option<Manifest>, Error> {
    db.apply_fn(|mut v| {
        v.package_data()
            .idx(pkg)
            .map(|mut pde| pde.manifest().get())
            .transpose()
    })
    .await
}
