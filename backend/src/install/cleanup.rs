use std::collections::HashMap;
use std::path::PathBuf;
use std::sync::Arc;

use bollard::image::ListImagesOptions;
use patch_db::{DbHandle, LockReceipt, LockTargetId, LockType, PatchDbHandle, Verifier};
use sqlx::{Executor, Postgres};
use tracing::instrument;

use super::PKG_ARCHIVE_DIR;
use crate::config::{not_found, ConfigReceipts};
use crate::context::RpcContext;
use crate::db::model::{
    AllPackageData, CurrentDependencies, CurrentDependents, InstalledPackageDataEntry,
    PackageDataEntry,
};
use crate::dependencies::{
    reconfigure_dependents_with_live_pointers, DependencyErrors, TryHealReceipts,
};
use crate::error::ErrorCollection;
use crate::s9pk::manifest::{Manifest, PackageId};
use crate::util::{Apply, Version};
use crate::volume::{asset_dir, script_dir};
use crate::Error;

pub struct UpdateDependencyReceipts {
    try_heal: TryHealReceipts,
    dependency_errors: LockReceipt<DependencyErrors, String>,
    manifest: LockReceipt<Manifest, String>,
}
impl UpdateDependencyReceipts {
    pub async fn new<'a>(db: &'a mut impl DbHandle) -> Result<Self, Error> {
        let mut locks = Vec::new();

        let setup = Self::setup(&mut locks);
        Ok(setup(&db.lock_all(locks).await?)?)
    }

    pub fn setup(locks: &mut Vec<LockTargetId>) -> impl FnOnce(&Verifier) -> Result<Self, Error> {
        let dependency_errors = crate::db::DatabaseModel::new()
            .package_data()
            .star()
            .installed()
            .map(|x| x.status().dependency_errors())
            .make_locker(LockType::Write)
            .add_to_keys(locks);
        let manifest = crate::db::DatabaseModel::new()
            .package_data()
            .star()
            .installed()
            .map(|x| x.manifest())
            .make_locker(LockType::Write)
            .add_to_keys(locks);
        let try_heal = TryHealReceipts::setup(locks);
        move |skeleton_key| {
            Ok(Self {
                dependency_errors: dependency_errors.verify(skeleton_key)?,
                manifest: manifest.verify(skeleton_key)?,
                try_heal: try_heal(skeleton_key)?,
            })
        }
    }
}

#[instrument(skip(ctx, db, deps, receipts))]
pub async fn update_dependency_errors_of_dependents<'a, Db: DbHandle>(
    ctx: &RpcContext,
    db: &mut Db,
    id: &PackageId,
    deps: &CurrentDependents,
    receipts: &UpdateDependencyReceipts,
) -> Result<(), Error> {
    for dep in deps.0.keys() {
        if let Some(man) = receipts.manifest.get(db, dep).await? {
            if let Err(e) = if let Some(info) = man.dependencies.0.get(id) {
                info.satisfied(ctx, db, id, None, dep, &receipts.try_heal)
                    .await?
            } else {
                Ok(())
            } {
                let mut errs = receipts
                    .dependency_errors
                    .get(db, dep)
                    .await?
                    .ok_or_else(not_found)?;
                errs.0.insert(id.clone(), e);
                receipts.dependency_errors.set(db, errs, dep).await?
            } else {
                let mut errs = receipts
                    .dependency_errors
                    .get(db, dep)
                    .await?
                    .ok_or_else(not_found)?;
                errs.0.remove(id);
                receipts.dependency_errors.set(db, errs, dep).await?
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
        futures::future::join_all(
            images
                .into_iter()
                .flatten()
                .flat_map(|image| image.repo_tags)
                .filter(|tag| {
                    tag.starts_with(&format!("start9/{}/", id))
                        && tag.ends_with(&format!(":{}", version))
                })
                .map(|tag| async {
                    let tag = tag; // move into future
                    ctx.docker.remove_image(&tag, None, None).await
                }),
        )
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

pub struct CleanupFailedReceipts {
    package_data_entry: LockReceipt<PackageDataEntry, String>,
    package_entries: LockReceipt<AllPackageData, ()>,
}

impl CleanupFailedReceipts {
    pub async fn new<'a>(db: &'a mut impl DbHandle) -> Result<Self, Error> {
        let mut locks = Vec::new();

        let setup = Self::setup(&mut locks);
        Ok(setup(&db.lock_all(locks).await?)?)
    }

    pub fn setup(locks: &mut Vec<LockTargetId>) -> impl FnOnce(&Verifier) -> Result<Self, Error> {
        let package_data_entry = crate::db::DatabaseModel::new()
            .package_data()
            .star()
            .make_locker(LockType::Write)
            .add_to_keys(locks);
        let package_entries = crate::db::DatabaseModel::new()
            .package_data()
            .make_locker(LockType::Write)
            .add_to_keys(locks);
        move |skeleton_key| {
            Ok(Self {
                package_data_entry: package_data_entry.verify(skeleton_key).unwrap(),
                package_entries: package_entries.verify(skeleton_key).unwrap(),
            })
        }
    }
}

#[instrument(skip(ctx, db, receipts))]
pub async fn cleanup_failed<Db: DbHandle>(
    ctx: &RpcContext,
    db: &mut Db,
    id: &PackageId,
    receipts: &CleanupFailedReceipts,
) -> Result<(), Error> {
    let pde = receipts
        .package_data_entry
        .get(db, id)
        .await?
        .ok_or_else(not_found)?;
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
            let mut entries = receipts.package_entries.get(db).await?;
            entries.0.remove(id);
            receipts.package_entries.set(db, entries).await?;
        }
        PackageDataEntry::Updating {
            installed,
            static_files,
            ..
        } => {
            receipts
                .package_data_entry
                .set(
                    db,
                    PackageDataEntry::Installed {
                        manifest: installed.manifest.clone(),
                        installed,
                        static_files,
                    },
                    id,
                )
                .await?;
        }
        _ => (),
    }

    Ok(())
}

#[instrument(skip(db, current_dependencies, current_dependent_receipt))]
pub async fn remove_from_current_dependents_lists<'a, Db: DbHandle>(
    db: &mut Db,
    id: &'a PackageId,
    current_dependencies: &'a CurrentDependencies,
    current_dependent_receipt: &LockReceipt<CurrentDependents, String>,
) -> Result<(), Error> {
    for dep in current_dependencies.0.keys().chain(std::iter::once(id)) {
        if let Some(mut current_dependents) = current_dependent_receipt.get(db, dep).await? {
            if current_dependents.0.remove(id).is_some() {
                current_dependent_receipt
                    .set(db, current_dependents, dep)
                    .await?;
            }
        }
    }
    Ok(())
}
pub struct UninstallReceipts {
    config: ConfigReceipts,
    removing: LockReceipt<InstalledPackageDataEntry, ()>,
    packages: LockReceipt<AllPackageData, ()>,
    current_dependents: LockReceipt<CurrentDependents, String>,
    update_depenency_receipts: UpdateDependencyReceipts,
}
impl UninstallReceipts {
    pub async fn new<'a>(db: &'a mut impl DbHandle, id: &PackageId) -> Result<Self, Error> {
        let mut locks = Vec::new();

        let setup = Self::setup(&mut locks, id);
        Ok(setup(&db.lock_all(locks).await?)?)
    }

    pub fn setup(
        locks: &mut Vec<LockTargetId>,
        id: &PackageId,
    ) -> impl FnOnce(&Verifier) -> Result<Self, Error> {
        let config = ConfigReceipts::setup(locks);
        let removing = crate::db::DatabaseModel::new()
            .package_data()
            .idx_model(id)
            .and_then(|pde| pde.removing())
            .make_locker(LockType::Write)
            .add_to_keys(locks);

        let current_dependents = crate::db::DatabaseModel::new()
            .package_data()
            .star()
            .installed()
            .map(|x| x.current_dependents())
            .make_locker(LockType::Write)
            .add_to_keys(locks);
        let packages = crate::db::DatabaseModel::new()
            .package_data()
            .make_locker(LockType::Write)
            .add_to_keys(locks);
        let update_depenency_receipts = UpdateDependencyReceipts::setup(locks);
        move |skeleton_key| {
            Ok(Self {
                config: config(skeleton_key)?,
                removing: removing.verify(skeleton_key)?,
                current_dependents: current_dependents.verify(skeleton_key)?,
                update_depenency_receipts: update_depenency_receipts(skeleton_key)?,
                packages: packages.verify(skeleton_key)?,
            })
        }
    }
}
#[instrument(skip(ctx, secrets, db))]
pub async fn uninstall<Ex>(
    ctx: &RpcContext,
    db: &mut PatchDbHandle,
    secrets: &mut Ex,
    id: &PackageId,
) -> Result<(), Error>
where
    for<'a> &'a mut Ex: Executor<'a, Database = Postgres>,
{
    let mut tx = db.begin().await?;
    crate::db::DatabaseModel::new()
        .package_data()
        .idx_model(&id)
        .lock(&mut tx, LockType::Write)
        .await?;
    let receipts = UninstallReceipts::new(&mut tx, id).await?;
    let entry = receipts.removing.get(&mut tx).await?;
    cleanup(ctx, &entry.manifest.id, &entry.manifest.version).await?;

    let packages = {
        let mut packages = receipts.packages.get(&mut tx).await?;
        packages.0.remove(id);
        packages
    };
    let dependents_paths: Vec<PathBuf> = entry
        .current_dependents
        .0
        .keys()
        .flat_map(|x| packages.0.get(x))
        .flat_map(|x| x.manifest_borrow().volumes.values())
        .flat_map(|x| x.pointer_path(&ctx.datadir))
        .collect();
    receipts.packages.set(&mut tx, packages).await?;
    // once we have removed the package entry, we can change all the dependent pointers to null
    reconfigure_dependents_with_live_pointers(ctx, &mut tx, &receipts.config, &entry).await?;

    remove_from_current_dependents_lists(
        &mut tx,
        &entry.manifest.id,
        &entry.current_dependencies,
        &receipts.current_dependents,
    )
    .await?;
    update_dependency_errors_of_dependents(
        ctx,
        &mut tx,
        &entry.manifest.id,
        &entry.current_dependents,
        &receipts.update_depenency_receipts,
    )
    .await?;
    let volumes = ctx
        .datadir
        .join(crate::volume::PKG_VOLUME_DIR)
        .join(&entry.manifest.id);

    tracing::debug!("Cleaning up {:?} at {:?}", volumes, dependents_paths);
    cleanup_folder(volumes, Arc::new(dependents_paths)).await;
    remove_tor_keys(secrets, &entry.manifest.id).await?;
    tx.commit().await?;
    Ok(())
}

#[instrument(skip(secrets))]
pub async fn remove_tor_keys<Ex>(secrets: &mut Ex, id: &PackageId) -> Result<(), Error>
where
    for<'a> &'a mut Ex: Executor<'a, Database = Postgres>,
{
    let id_str = id.as_str();
    sqlx::query!("DELETE FROM tor WHERE package = $1", id_str)
        .execute(secrets)
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
