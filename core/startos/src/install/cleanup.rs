use std::path::PathBuf;
use std::sync::Arc;

use models::{OptionExt, PackageId};
use sqlx::{Executor, Postgres};
use tracing::instrument;

use super::PKG_ARCHIVE_DIR;
use crate::context::RpcContext;
use crate::db::model::{
    CurrentDependencies, Database, PackageDataEntry, PackageDataEntryInstalled,
    PackageDataEntryMatchModelRef,
};
use crate::error::ErrorCollection;
use crate::prelude::*;
use crate::util::{Apply, Version};
use crate::volume::{asset_dir, script_dir};
use crate::Error;

#[instrument(skip_all)]
pub async fn cleanup(ctx: &RpcContext, id: &PackageId, version: &Version) -> Result<(), Error> {
    let mut errors = ErrorCollection::new();
    ctx.managers.remove(&(id.clone(), version.clone())).await;
    // docker images start9/$APP_ID/*:$VERSION -q | xargs docker rmi
    let images = crate::util::docker::images_for(id, version).await?;
    errors.extend(
        futures::future::join_all(images.into_iter().map(|sha| async {
            let sha = sha; // move into future
            crate::util::docker::remove_image(&sha).await
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

#[instrument(skip_all)]
pub async fn cleanup_failed(ctx: &RpcContext, id: &PackageId) -> Result<(), Error> {
    if let Some(version) = match ctx
        .db
        .peek()
        .await
        .as_package_data()
        .as_idx(id)
        .or_not_found(id)?
        .as_match()
    {
        PackageDataEntryMatchModelRef::Installing(m) => Some(m.as_manifest().as_version().de()?),
        PackageDataEntryMatchModelRef::Restoring(m) => Some(m.as_manifest().as_version().de()?),
        PackageDataEntryMatchModelRef::Updating(m) => {
            let manifest_version = m.as_manifest().as_version().de()?;
            let installed = m.as_installed().as_manifest().as_version().de()?;
            if manifest_version != installed {
                Some(manifest_version)
            } else {
                None // do not remove existing data
            }
        }
        _ => {
            tracing::warn!("{}: Nothing to clean up!", id);
            None
        }
    } {
        cleanup(ctx, id, &version).await?;
    }

    ctx.db
        .mutate(|v| {
            match v
                .clone()
                .into_package_data()
                .into_idx(id)
                .or_not_found(id)?
                .as_match()
            {
                PackageDataEntryMatchModelRef::Installing(_)
                | PackageDataEntryMatchModelRef::Restoring(_) => {
                    v.as_package_data_mut().remove(id)?;
                }
                PackageDataEntryMatchModelRef::Updating(pde) => {
                    v.as_package_data_mut()
                        .as_idx_mut(id)
                        .or_not_found(id)?
                        .ser(&PackageDataEntry::Installed(PackageDataEntryInstalled {
                            manifest: pde.as_installed().as_manifest().de()?,
                            static_files: pde.as_static_files().de()?,
                            installed: pde.as_installed().de()?,
                        }))?;
                }
                _ => (),
            }
            Ok(())
        })
        .await
}

#[instrument(skip_all)]
pub fn remove_from_current_dependents_lists(
    db: &mut Model<Database>,
    id: &PackageId,
    current_dependencies: &CurrentDependencies,
) -> Result<(), Error> {
    for dep in current_dependencies.0.keys().chain(std::iter::once(id)) {
        if let Some(current_dependents) = db
            .as_package_data_mut()
            .as_idx_mut(dep)
            .and_then(|d| d.as_installed_mut())
            .map(|i| i.as_current_dependents_mut())
        {
            current_dependents.remove(id)?;
        }
    }
    Ok(())
}

#[instrument(skip_all)]
pub async fn uninstall<Ex>(ctx: &RpcContext, secrets: &mut Ex, id: &PackageId) -> Result<(), Error>
where
    for<'a> &'a mut Ex: Executor<'a, Database = Postgres>,
{
    let db = ctx.db.peek().await;
    let entry = db
        .as_package_data()
        .as_idx(id)
        .or_not_found(id)?
        .expect_as_removing()?;

    let dependents_paths: Vec<PathBuf> = todo!();

    let volume_dir = ctx
        .datadir
        .join(crate::volume::PKG_VOLUME_DIR)
        .join(&*entry.as_manifest().as_id().de()?);
    let version = entry.as_removing().as_manifest().as_version().de()?;
    tracing::debug!(
        "Cleaning up {:?} except for {:?}",
        volume_dir,
        dependents_paths
    );
    cleanup(ctx, id, &version).await?;
    cleanup_folder(volume_dir, Arc::new(dependents_paths)).await;
    remove_network_keys(secrets, id).await?;

    ctx.db
        .mutate(|d| {
            d.as_package_data_mut().remove(id)?;
            remove_from_current_dependents_lists(
                d,
                id,
                &entry.as_removing().as_current_dependencies().de()?,
            )
        })
        .await
}

#[instrument(skip_all)]
pub async fn remove_network_keys<Ex>(secrets: &mut Ex, id: &PackageId) -> Result<(), Error>
where
    for<'a> &'a mut Ex: Executor<'a, Database = Postgres>,
{
    sqlx::query!("DELETE FROM network_keys WHERE package = $1", &*id)
        .execute(&mut *secrets)
        .await?;
    sqlx::query!("DELETE FROM tor WHERE package = $1", &*id)
        .execute(&mut *secrets)
        .await?;
    Ok(())
}

/// Needed to remove, without removing the folders that are mounted in the other docker containers
pub fn cleanup_folder(
    path: PathBuf,
    dependents_volumes: Arc<Vec<PathBuf>>,
) -> futures::future::BoxFuture<'static, ()> {
    Box::pin(async move {
        let meta_data = match tokio::fs::metadata(&path).await {
            Ok(a) => a,
            Err(_e) => {
                return;
            }
        };
        if !meta_data.is_dir() {
            tracing::error!("is_not dir, remove {:?}", path);
            let _ = tokio::fs::remove_file(&path).await;
            return;
        }
        if !dependents_volumes
            .iter()
            .any(|v| v.starts_with(&path) || v == &path)
        {
            tracing::error!("No parents, remove {:?}", path);
            let _ = tokio::fs::remove_dir_all(&path).await;
            return;
        }
        let mut read_dir = match tokio::fs::read_dir(&path).await {
            Ok(a) => a,
            Err(_e) => {
                return;
            }
        };
        tracing::error!("Parents, recurse {:?}", path);
        while let Some(entry) = read_dir.next_entry().await.ok().flatten() {
            let entry_path = entry.path();
            cleanup_folder(entry_path, dependents_volumes.clone()).await;
        }
    })
}
