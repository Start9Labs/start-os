use std::borrow::Cow;
use std::path::Path;

use anyhow::anyhow;
use bollard::Docker;
use patch_db::DbHandle;

use super::PKG_PUBLIC_DIR;
use crate::context::RpcContext;
use crate::db::model::{InstalledPackageDataEntry, PackageDataEntry};
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
    id: &PackageId,
    version: &Version,
) -> Result<(), Error> {
    let pde = crate::db::DatabaseModel::new()
        .package_data()
        .idx_model(id)
        .expect(db)
        .await?
        .get(db, true)
        .await?
        .to_owned();
    if let Some(manifest) = match &pde {
        PackageDataEntry::Installing { manifest, .. } => Some(manifest),
        PackageDataEntry::Updating { manifest, .. } => {
            if &manifest.version != version {
                Some(manifest)
            } else {
                None
            }
        }
        _ => {
            log::warn!("{}: Nothing to clean up!", id);
            None
        }
    } {
        ctx.managers
            .remove(&(manifest.id.clone(), manifest.version.clone()))
            .await;
        // docker images start9/$APP_ID/*:$VERSION -q | xargs docker rmi
        let public_dir_path = Path::new(PKG_PUBLIC_DIR).join(id).join(version.as_str());
    }

    match pde {
        PackageDataEntry::Installing { .. } => {
            crate::db::DatabaseModel::new()
                .package_data()
                .remove(db, id)
                .await?;
        }
        PackageDataEntry::Updating {
            installed,
            manifest,
            static_files,
            ..
        } => {
            crate::db::DatabaseModel::new()
                .package_data()
                .idx_model(id)
                .put(
                    db,
                    &PackageDataEntry::Installed {
                        installed,
                        manifest,
                        static_files,
                    },
                )
                .await?;
        }
        _ => (),
    }

    Ok(()) // TODO
}

pub async fn uninstall<Db: DbHandle>(
    ctx: &RpcContext,
    db: &mut Db,
    entry: InstalledPackageDataEntry,
) -> Result<(), Error> {
    //TODO
    Ok(())
}
