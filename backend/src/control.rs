use std::collections::BTreeMap;

use color_eyre::eyre::eyre;
use rpc_toolkit::command;
use tracing::instrument;

use crate::context::RpcContext;
use crate::dependencies::{
    break_all_dependents_transitive, heal_all_dependents_transitive, BreakageRes, DependencyError,
    DependencyReceipt, TaggedDependencyError,
};
use crate::prelude::*;
use crate::s9pk::manifest::PackageId;
use crate::status::MainStatus;
use crate::util::display_none;
use crate::util::serde::display_serializable;
use crate::Error;

// impl StartReceipts {
//     pub async fn new(db: PatchDb, id: &PackageId) -> Result<Self, Error> {
//         let mut locks = Vec::new();

//         let setup = Self::setup(&mut locks, id);
//         setup(&db.lock_all(locks).await?)
//     }

//     pub fn setup(
//         locks: &mut Vec<patch_db::LockTargetId>,
//         id: &PackageId,
//     ) -> impl FnOnce(&patch_db::Verifier) -> Result<Self, Error> {
//         let dependency_receipt = DependencyReceipt::setup(locks);
//         let status = crate::db::DatabaseModel::new()
//             .package_data()
//             .idx_model(id)
//             .and_then(|x| x.installed())
//             .map(|x| x.status().main())
//             .make_locker(LockType::Write)
//             .add_to_keys(locks);
//         let version = crate::db::DatabaseModel::new()
//             .package_data()
//             .idx_model(id)
//             .and_then(|x| x.installed())
//             .map(|x| x.manifest().version())
//             .make_locker(LockType::Read)
//             .add_to_keys(locks);
//         move |skeleton_key| {
//             Ok(Self {
//                 dependency_receipt: dependency_receipt(skeleton_key)?,
//                 status: status.verify(skeleton_key)?,
//                 version: version.verify(skeleton_key)?,
//             })
//         }
//     }
// }

#[command(display(display_none), metadata(sync_db = true))]
#[instrument(skip_all)]
pub async fn start(#[context] ctx: RpcContext, #[arg] id: PackageId) -> Result<(), Error> {
    let peek = ctx.db.peek().await?;
    let version = peek
        .as_package_data()
        .as_idx(&id)
        .or_not_found(&id)?
        .as_installed()
        .or_not_found(&id)?
        .as_manifest()
        .as_version()
        .de()?;
    heal_all_dependents_transitive(&ctx, &mut tx, &id).await?;

    ctx.managers
        .get(&(id, version))
        .await
        .ok_or_else(|| Error::new(eyre!("Manager not found"), crate::ErrorKind::InvalidRequest))?
        .start();

    Ok(())
}
// impl StopReceipts {
//     pub async fn new<'a>(db: &'a mut impl DbHandle, id: &PackageId) -> Result<Self, Error> {
//         let mut locks = Vec::new();

//         let setup = Self::setup(&mut locks, id);
//         setup(&db.lock_all(locks).await?)
//     }

//     pub fn setup(
//         locks: &mut Vec<patch_db::LockTargetId>,
//         id: &PackageId,
//     ) -> impl FnOnce(&patch_db::Verifier) -> Result<Self, Error> {
//         let breaks = crate::dependencies::BreakTransitiveReceipts::setup(locks);
//         let status = crate::db::DatabaseModel::new()
//             .package_data()
//             .idx_model(id)
//             .and_then(|x| x.installed())
//             .map(|x| x.status().main())
//             .make_locker(LockType::Write)
//             .add_to_keys(locks);
//         move |skeleton_key| {
//             Ok(Self {
//                 breaks: breaks(skeleton_key)?,
//                 status: status.verify(skeleton_key)?,
//             })
//         }
//     }
// }

#[instrument(skip_all)]
pub async fn stop_common(
    db: PatchDb,
    id: &PackageId,
    breakages: &mut BTreeMap<PackageId, TaggedDependencyError>,
) -> Result<MainStatus, Error> {
    let last_status = db
        .mutate(|v| {
            v.as_package_data_mut()
                .as_idx_mut(id)
                .and_then(|x| x.as_installed_mut())
                .ok_or_else(|| Error::new(eyre!("{} is not installed", id), ErrorKind::NotFound))?
                .as_status_mut()
                .as_main_mut()
                .replace(&MainStatus::Stopping)
        })
        .await?;
    break_all_dependents_transitive(db, id, DependencyError::NotRunning, breakages).await?;

    Ok(last_status)
}

#[command(
    subcommands(self(stop_impl(async)), stop_dry),
    display(display_none),
    metadata(sync_db = true)
)]
pub fn stop(#[arg] id: PackageId) -> Result<PackageId, Error> {
    Ok(id)
}

#[command(rename = "dry", display(display_serializable))]
#[instrument(skip_all)]
pub async fn stop_dry(
    #[context] ctx: RpcContext,
    #[parent_data] id: PackageId,
) -> Result<BreakageRes, Error> {
    let mut db = ctx.db.handle();
    let mut tx = db.begin().await?;

    let mut breakages = BTreeMap::new();
    stop_common(&mut tx, &id, &mut breakages).await?;

    tx.abort().await?;

    Ok(BreakageRes(breakages))
}

#[instrument(skip_all)]
pub async fn stop_impl(ctx: RpcContext, id: PackageId) -> Result<MainStatus, Error> {
    let peek = ctx.db.peek().await?;
    let version = peek
        .as_package_data()
        .as_idx(&id)
        .or_not_found(&id)?
        .as_installed()
        .or_not_found(&id)?
        .as_manifest()
        .as_version()
        .de()?;

    let last_statuts = stop_common(ctx.db.clone(), &id, &mut BTreeMap::new()).await?;

    ctx.managers
        .get(&(id, version))
        .await
        .ok_or_else(|| Error::new(eyre!("Manager not found"), crate::ErrorKind::InvalidRequest))?
        .stop();

    Ok(last_statuts)
}

#[command(display(display_none), metadata(sync_db = true))]
pub async fn restart(#[context] ctx: RpcContext, #[arg] id: PackageId) -> Result<(), Error> {
    let peek = ctx.db.peek().await?;
    let version = peek
        .as_package_data()
        .as_idx(&id)
        .or_not_found(&id)?
        .as_installed()
        .or_not_found(&id)?
        .as_manifest()
        .as_version()
        .de()?;

    ctx.managers
        .get(&(id, version))
        .await
        .ok_or_else(|| Error::new(eyre!("Manager not found"), crate::ErrorKind::InvalidRequest))?
        .restart()
        .await;

    Ok(())
}
