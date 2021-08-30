use std::borrow::Cow;
use std::collections::HashMap;
use std::path::Path;

use anyhow::anyhow;
use bollard::image::ListImagesOptions;
use bollard::Docker;
use patch_db::{DbHandle, PatchDbHandle};
use tokio::process::Command;

use super::PKG_PUBLIC_DIR;
use crate::context::RpcContext;
use crate::db::model::{InstalledPackageDataEntry, PackageDataEntry};
use crate::dependencies::DependencyError;
use crate::s9pk::manifest::{Manifest, PackageId};
use crate::util::{Invoke, Version};
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
        .to_owned();
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
    update_dependents(&mut tx, &entry.manifest.id, entry.current_dependents.keys()).await?;
    tx.commit(None).await?;
    Ok(())
}

#[tokio::test]
async fn test() {
    dbg!(
        Docker::connect_with_socket_defaults()
            .unwrap()
            .list_images(Some(ListImagesOptions {
                all: false,
                filters: {
                    let mut f = HashMap::new();
                    f.insert("reference", vec!["start9/*:latest"]);
                    f
                },
                digests: false
            }))
            .await
    );
}
