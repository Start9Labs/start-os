use std::collections::{BTreeMap, HashMap};

use bollard::image::ListImagesOptions;
use color_eyre::eyre::eyre;
use patch_db::{DbHandle, PatchDbHandle};
use tracing::instrument;

use super::{PKG_ARCHIVE_DIR, PKG_DOCKER_DIR};
use crate::context::RpcContext;
use crate::db::model::{CurrentDependencyInfo, InstalledPackageDataEntry, PackageDataEntry};
use crate::error::ErrorCollection;
use crate::s9pk::manifest::{Manifest, PackageId};
use crate::util::{Apply, Version};
use crate::Error;

#[instrument(skip(ctx, db, deps))]
pub async fn update_dependents<'a, Db: DbHandle, I: IntoIterator<Item = &'a PackageId>>(
    ctx: &RpcContext,
    db: &mut Db,
    id: &PackageId,
    deps: I,
) -> Result<(), Error> {
    for dep in deps {
        if let Some(man) = &*crate::db::DatabaseModel::new()
            .package_data()
            .idx_model(&dep)
            .and_then(|m| m.installed())
            .map::<_, Manifest>(|m| m.manifest())
            .get(db, true)
            .await?
        {
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
    }
    Ok(())
}

#[instrument(skip(ctx))]
pub async fn cleanup(ctx: &RpcContext, id: &PackageId, version: &Version) -> Result<(), Error> {
    let mut errors = ErrorCollection::new();
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
        .await
        .apply(|res| errors.handle(res));
    errors.extend(
        futures::future::join_all(images.into_iter().flatten().map(|image| async {
            let image = image; // move into future
            ctx.docker.remove_image(&image.id, None, None).await
        }))
        .await,
    );
    let pkg_archive_dir = ctx
        .datadir
        .join(PKG_ARCHIVE_DIR)
        .join(id)
        .join(version.as_str());
    if tokio::fs::metadata(&pkg_archive_dir).await.is_ok() {
        tokio::fs::remove_dir_all(&pkg_archive_dir)
            .await
            .apply(|res| errors.handle(res));
    }
    let docker_path = ctx
        .datadir
        .join(PKG_DOCKER_DIR)
        .join(id)
        .join(version.as_str());
    if tokio::fs::metadata(&docker_path).await.is_ok() {
        tokio::fs::remove_dir_all(&docker_path)
            .await
            .apply(|res| errors.handle(res));
    }

    errors.into_result()
}

#[instrument(skip(ctx, db))]
pub async fn cleanup_failed<Db: DbHandle>(
    ctx: &RpcContext,
    db: &mut Db,
    id: &PackageId,
) -> Result<(), Error> {
    let pde = crate::db::DatabaseModel::new()
        .package_data()
        .idx_model(id)
        .expect(db)
        .await?
        .get(db, true)
        .await?
        .into_owned();
    if let Some(manifest) = match &pde {
        PackageDataEntry::Installing { manifest, .. }
        | PackageDataEntry::Restoring { manifest, .. } => Some(manifest),
        PackageDataEntry::Updating {
            manifest,
            installed:
                InstalledPackageDataEntry {
                    manifest: installed_manifest,
                    ..
                },
            ..
        } => {
            if &manifest.version != &installed_manifest.version {
                Some(manifest)
            } else {
                None
            }
        }
        _ => {
            tracing::warn!("{}: Nothing to clean up!", id);
            None
        }
    } {
        cleanup(ctx, id, &manifest.version).await?;
    }

    match pde {
        PackageDataEntry::Installing { .. } | PackageDataEntry::Restoring { .. } => {
            crate::db::DatabaseModel::new()
                .package_data()
                .remove(db, id)
                .await?;
        }
        PackageDataEntry::Updating {
            installed,
            static_files,
            ..
        } => {
            crate::db::DatabaseModel::new()
                .package_data()
                .idx_model(id)
                .put(
                    db,
                    &PackageDataEntry::Installed {
                        manifest: installed.manifest.clone(),
                        installed,
                        static_files,
                    },
                )
                .await?;
        }
        _ => (),
    }

    Ok(())
}

#[instrument(skip(db, current_dependencies))]
pub async fn remove_current_dependents<'a, Db: DbHandle, I: IntoIterator<Item = &'a PackageId>>(
    db: &mut Db,
    id: &'a PackageId,
    current_dependencies: I,
) -> Result<(), Error> {
    for dep in current_dependencies.into_iter().chain(std::iter::once(id)) {
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

#[instrument(skip(ctx, db))]
pub async fn uninstall(
    ctx: &RpcContext,
    db: &mut PatchDbHandle,
    id: &PackageId,
) -> Result<(), Error> {
    let mut tx = db.begin().await?;
    let entry = crate::db::DatabaseModel::new()
        .package_data()
        .idx_model(id)
        .and_then(|pde| pde.removing())
        .get(&mut tx, true)
        .await?
        .into_owned()
        .ok_or_else(|| {
            Error::new(
                eyre!("Package not in removing state: {}", id),
                crate::ErrorKind::NotFound,
            )
        })?;
    cleanup(ctx, &entry.manifest.id, &entry.manifest.version).await?;
    crate::db::DatabaseModel::new()
        .package_data()
        .remove(&mut tx, id)
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
    let volumes = ctx
        .datadir
        .join(crate::volume::PKG_VOLUME_DIR)
        .join(&entry.manifest.id);
    if tokio::fs::metadata(&volumes).await.is_ok() {
        tokio::fs::remove_dir_all(&volumes).await?;
    }
    tx.commit(None).await?;
    Ok(())
}
