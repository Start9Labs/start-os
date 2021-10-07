use std::collections::{BTreeMap, HashMap};

use bollard::image::ListImagesOptions;
use patch_db::{DbHandle, PatchDbHandle};

use super::PKG_DOCKER_DIR;
use crate::context::RpcContext;
use crate::db::model::{CurrentDependencyInfo, InstalledPackageDataEntry, PackageDataEntry};
use crate::dependencies::update_current_dependents;
use crate::s9pk::manifest::PackageId;
use crate::util::Version;
use crate::Error;

pub async fn update_dependents<'a, Db: DbHandle, I: IntoIterator<Item = &'a PackageId>>(
    ctx: &RpcContext,
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
        if let Err(e) = if let Some(info) = man.dependencies.0.get(id) {
            info.satisfied(ctx, db, id, None, dep).await?
        } else {
            Ok(())
        } {
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
        } else {
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
            errs.0.remove(id);
            errs.save(db).await?;
        }
    }
    Ok(())
}

pub async fn cleanup(ctx: &RpcContext, id: &PackageId, version: &Version) -> Result<(), Error> {
    ctx.managers.remove(&(id.clone(), version.clone())).await;
    // docker images start9/$APP_ID/*:$VERSION -q | xargs docker rmi
    let images = ctx
        .docker
        .list_images(Some(ListImagesOptions {
            all: false,
            filters: {
                let mut f = HashMap::new();
                f.insert(
                    "reference".to_owned(),
                    vec![format!("start9/{}/*:{}", id, version)],
                );
                f
            },
            digests: false,
        }))
        .await?;
    futures::future::try_join_all(images.into_iter().map(|image| async {
        let image = image; // move into future
        ctx.docker.remove_image(&image.id, None, None).await
    }))
    .await?;
    let docker_path = ctx
        .datadir
        .join(PKG_DOCKER_DIR)
        .join(id)
        .join(version.as_str());
    if tokio::fs::metadata(&docker_path).await.is_ok() {
        tokio::fs::remove_dir_all(&docker_path).await?;
    }
    // TODO: delete public dir if not a dependency

    Ok(())
}

pub async fn cleanup_failed<Db: DbHandle>(
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
        .into_owned();
    if match &pde {
        PackageDataEntry::Installing { .. } => true,
        PackageDataEntry::Updating { manifest, .. } => {
            if &manifest.version != version {
                true
            } else {
                false
            }
        }
        _ => {
            log::warn!("{}: Nothing to clean up!", id);
            false
        }
    } {
        cleanup(ctx, id, version).await?;
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

    Ok(())
}

pub async fn remove_current_dependents<'a, Db: DbHandle, I: IntoIterator<Item = &'a PackageId>>(
    db: &mut Db,
    id: &PackageId,
    current_dependencies: I,
) -> Result<(), Error> {
    for dep in current_dependencies {
        if let Some(current_dependents) = crate::db::DatabaseModel::new()
            .package_data()
            .idx_model(dep)
            .and_then(|m| m.installed())
            .map::<_, BTreeMap<PackageId, CurrentDependencyInfo>>(|m| m.current_dependents())
            .check(db)
            .await?
        {
            if current_dependents
                .clone()
                .idx_model(id)
                .exists(db, true)
                .await?
            {
                current_dependents.remove(db, id).await?
            }
        }
    }
    Ok(())
}

pub async fn uninstall(
    ctx: &RpcContext,
    db: &mut PatchDbHandle,
    entry: &InstalledPackageDataEntry,
) -> Result<(), Error> {
    cleanup(ctx, &entry.manifest.id, &entry.manifest.version).await?;
    let mut tx = db.begin().await?;
    crate::db::DatabaseModel::new()
        .package_data()
        .remove(&mut tx, &entry.manifest.id)
        .await?;
    remove_current_dependents(
        &mut tx,
        &entry.manifest.id,
        entry.current_dependencies.keys(),
    )
    .await?;
    update_dependents(
        ctx,
        &mut tx,
        &entry.manifest.id,
        entry.current_dependents.keys(),
    )
    .await?;
    tx.commit(None).await?;
    Ok(())
}
