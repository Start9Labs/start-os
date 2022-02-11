use patch_db::DbHandle;

use crate::s9pk::manifest::{Manifest, PackageId};
use crate::Error;

pub async fn get_packages<Db: DbHandle>(db: &mut Db) -> Result<Vec<PackageId>, Error> {
    let packages = crate::db::DatabaseModel::new()
        .package_data()
        .get(db, false)
        .await?;
    Ok(packages.0.keys().cloned().collect())
}

pub async fn get_manifest<Db: DbHandle>(
    db: &mut Db,
    pkg: &PackageId,
) -> Result<Option<Manifest>, Error> {
    let mpde = crate::db::DatabaseModel::new()
        .package_data()
        .idx_model(pkg)
        .get(db, false)
        .await?
        .into_owned();
    Ok(mpde.map(|pde| pde.manifest()))
}
