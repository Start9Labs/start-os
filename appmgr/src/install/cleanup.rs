use std::borrow::Cow;

use anyhow::anyhow;
use bollard::Docker;
use patch_db::DbHandle;

use crate::context::RpcContext;
use crate::db::model::InstalledPackageDataEntry;
use crate::dependencies::DependencyError;
use crate::s9pk::manifest::{Manifest, PackageId};
use crate::util::Version;
use crate::Error;

pub async fn update_dependents<'a, Db: DbHandle, I: IntoIterator<Item = &'a PackageId>>(
    db: &mut Db,
    id: &PackageId,
    deps: I,
) -> Result<(), Error> {
    for dep in deps {
        let man = crate::db::DatabaseModel::new()
            .package_data()
            .idx_model(&dep)
            .expect(db)
            .await?
            .installed()
            .expect(db)
            .await?
            .manifest()
            .get(db, true)
            .await?;
        if let Err(e) = man
            .dependencies
            .0
            .get(id)
            .ok_or_else(|| {
                Error::new(
                    anyhow!("missing dependency info"),
                    crate::ErrorKind::Database,
                )
            })?
            .satisfied(db, id, None, dep, &man.version, &man.volumes)
            .await?
        {
            let mut errs = crate::db::DatabaseModel::new()
                .package_data()
                .idx_model(&dep)
                .expect(db)
                .await?
                .installed()
                .expect(db)
                .await?
                .status()
                .dependency_errors()
                .get_mut(db)
                .await?;
            errs.0.insert(id.clone(), e);
            errs.save(db).await?;
        }
    }
    Ok(())
}

pub async fn cleanup<Db: DbHandle>(
    ctx: &RpcContext,
    db: &mut Db,
    info: Result<InstalledPackageDataEntry, &Manifest>,
) -> Result<(), Error> {
    let man = match info {
        Ok(pde) => {
            // TODO

            Cow::Owned(pde.manifest)
        }
        Err(man) => Cow::Borrowed(man),
    };
    ctx.managers
        .remove(&(man.id.clone(), man.version.clone()))
        .await;
    // docker images start9/$APP_ID/*:$VERSION -q | xargs docker rmi
    Ok(()) // TODO
}
