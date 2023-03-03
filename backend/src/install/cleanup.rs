use std::path::PathBuf;

use futures::FutureExt;
use sqlx::{Executor, Postgres};
use tracing::instrument;

use super::PKG_ARCHIVE_DIR;
use crate::context::RpcContext;
use crate::db::model::{
    PackageDataEntry, PackageDataEntryInstalled, PackageDataEntryMatchModel,
    PackageDataEntryMatchModelMut, PackageDataEntryMatchModelRef,
};
use crate::dependencies::DependencyError;
use crate::prelude::*;
use crate::s9pk::manifest::PackageId;
use crate::util::{Apply, Version};
use crate::volume::{asset_dir, script_dir};

#[instrument(skip(ctx))]
pub async fn cleanup(ctx: &RpcContext, id: &PackageId, version: &Version) -> Result<(), Error> {
    let mut errors = ErrorCollection::new();
    ctx.managers.remove(id).await;
    let images = crate::docker::images_for(id, version)
        .await
        .apply(|res| errors.handle(res));
    errors.extend(
        futures::future::join_all(
            images
                .into_iter()
                .flatten()
                .map(|sha| async move { crate::docker::remove_image(&sha).await }),
        )
        .await,
    );
    errors.handle(crate::docker::prune_images().await);
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
    let assets_path = asset_dir(&ctx.datadir, id, version);
    if tokio::fs::metadata(&assets_path).await.is_ok() {
        tokio::fs::remove_dir_all(&assets_path)
            .await
            .apply(|res| errors.handle(res));
    }
    let scripts_path = script_dir(&ctx.datadir, id, version);
    if tokio::fs::metadata(&scripts_path).await.is_ok() {
        tokio::fs::remove_dir_all(&scripts_path)
            .await
            .apply(|res| errors.handle(res));
    }

    errors.into_result()
}

#[instrument(skip(ctx))]
pub async fn cleanup_failed(ctx: &RpcContext, id: &PackageId) -> Result<(), Error> {
    let db = ctx.db.peek().await?;
    let pde = db.as_package_data().as_idx(id).or_not_found(id)?;
    if let Some(version) = match pde.as_match() {
        PackageDataEntryMatchModelRef::Installing(m) => Some(m.as_manifest().as_version()),
        PackageDataEntryMatchModelRef::Restoring(m) => Some(m.as_manifest().as_version()),
        PackageDataEntryMatchModelRef::Updating(m) => {
            if &m.as_new_manifest().as_version().clone().de()?
                != &m.as_manifest().as_version().clone().de()?
            {
                Some(m.as_new_manifest().as_version())
            } else {
                None
            }
        }
        _ => {
            tracing::warn!("{}: Nothing to clean up!", id);
            None
        }
    } {
        cleanup(ctx, id, &version.clone().de()?).await?;
    }

    ctx.db
        .mutate(|v| {
            let package_data = v.as_package_data_mut();
            let pde = package_data.as_idx_mut(id).or_not_found(id)?;
            match pde.as_match() {
                PackageDataEntryMatchModelRef::Installing(_)
                | PackageDataEntryMatchModelRef::Restoring(_) => {
                    package_data.remove(id);
                }
                PackageDataEntryMatchModelRef::Updating(m) => {
                    *pde = Model::<PackageDataEntry>::from_match(
                        PackageDataEntryMatchModel::Installed(
                            Model::<PackageDataEntryInstalled>::from_parts(
                                m.as_static_files().clone(),
                                m.as_manifest().clone(),
                                m.as_installed().clone(),
                            ),
                        ),
                    )
                }
                _ => (),
            }
            Ok(())
        })
        .await?;

    Ok(())
}

#[instrument(skip(ctx))]
pub async fn uninstall(ctx: &RpcContext, id: &PackageId) -> Result<(), Error> {
    let db = ctx.db.peek().await?.clone();
    let PackageDataEntryMatchModelRef::Removing(removing) = db.as_package_data().as_idx(id).or_not_found(id)?.as_match() else {
        return Err(Error::new(eyre!("{id} is not in removing state"), ErrorKind::InvalidRequest));
    };
    cleanup(ctx, id, &removing.as_manifest().as_version().clone().de()?).await?;

    let mut dependents_paths = Vec::new();
    for (dep, info) in db.as_package_data().as_entries()? {
        for (_, v) in info.as_manifest()?.as_volumes().clone().de()?.iter() {
            match v {
                crate::volume::Volume::Pointer(p) if &p.package_id == id => {
                    dependents_paths.push(p.path(&ctx.datadir));
                }
                _ => (),
            }
        }
    }

    // todo!("update_dependency_errors_of_dependents");
    let volumes = ctx.datadir.join(crate::volume::PKG_VOLUME_DIR).join(id);

    tracing::debug!("Cleaning up {:?} except {:?}", volumes, dependents_paths);
    remove_except(volumes, &dependents_paths).await;
    remove_tor_keys(&mut ctx.secret_store.acquire().await?, id).await?;

    ctx.db
        .mutate(|v| {
            let package_data = v.as_package_data_mut();
            package_data.remove(id);
            for (_, pde) in package_data.as_entries_mut()? {
                let installed = match pde.as_match_mut() {
                    PackageDataEntryMatchModelMut::Installed(m) => m.as_installed_mut(),
                    PackageDataEntryMatchModelMut::Updating(m) => m.as_installed_mut(),
                    _ => continue,
                };
                if installed.as_current_dependencies().keys()?.contains(id) {
                    installed
                        .as_status_mut()
                        .as_dependency_errors_mut()
                        .insert(id, &DependencyError::NotInstalled);
                }
            }
            Ok(())
        })
        .await?;
    Ok(())
}

#[instrument(skip(secrets))]
async fn remove_tor_keys<Ex>(secrets: &mut Ex, id: &PackageId) -> Result<(), Error>
where
    for<'a> &'a mut Ex: Executor<'a, Database = Postgres>,
{
    sqlx::query!("DELETE FROM tor WHERE package = $1", id)
        .execute(secrets)
        .await?;
    Ok(())
}

/// Needed to remove, without removing the folders that are mounted in the other docker containers
fn remove_except<'a>(
    path: PathBuf,
    except: &'a [PathBuf],
) -> futures::future::BoxFuture<'a, Result<(), Error>> {
    async move {
        let meta_data = match tokio::fs::metadata(&path).await {
            Ok(a) => a,
            Err(_e) => {
                return Ok(());
            }
        };
        if !meta_data.is_dir() {
            tracing::info!("{:?} is not dir: removing", path);
            tokio::fs::remove_file(&path).await?;
            return Ok(());
        }
        if !except.iter().any(|v| v.starts_with(&path)) {
            tracing::info!("{:?} has no pointers: removing", path);
            tokio::fs::remove_dir_all(&path).await?;
            return Ok(());
        }
        let mut read_dir = tokio::fs::read_dir(&path).await?;
        tracing::info!("{:?} contains pointers: traversing", path);
        let mut errors = ErrorCollection::new();
        while let Some(entry) = read_dir.next_entry().await.ok().flatten() {
            let entry_path = entry.path();
            errors.handle(remove_except(entry_path, except).await);
        }
        errors.into_result()
    }
    .boxed()
}
