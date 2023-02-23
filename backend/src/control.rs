use std::collections::BTreeMap;

use color_eyre::eyre::eyre;
use rpc_toolkit::command;
use tracing::instrument;

use crate::context::RpcContext;
use crate::dependencies::{
    break_all_dependents_transitive, heal_all_dependents_transitive, BreakageRes, DependencyError,
    TaggedDependencyError,
};
use crate::prelude::*;
use crate::s9pk::manifest::PackageId;
use crate::status::MainStatus;
use crate::util::display_none;
use crate::util::serde::display_serializable;

#[command(display(display_none), metadata(sync_db = true))]
#[instrument(skip(ctx))]
pub async fn start(#[context] ctx: RpcContext, #[arg] id: PackageId) -> Result<(), Error> {
    let mut db = ctx.db.handle();
    let mut tx = db.begin().await?;
    let receipts = todo!(); // StartReceipts::new(&mut tx, &id).await?;
    let version = receipts.version.get(&mut tx).await?;
    receipts.status.set(&mut tx, MainStatus::Starting).await?;
    heal_all_dependents_transitive(&ctx, &id, &receipts.dependency_receipt).await?;

    tx.commit().await?;
    drop(receipts);

    ctx.managers
        .get(&(id, version))
        .await
        .ok_or_else(|| Error::new(eyre!("Manager not found"), ErrorKind::InvalidRequest))?
        .start();

    Ok(())
}

#[instrument(skip(db))]
pub async fn stop_common(
    db: &PatchDb,
    id: &PackageId,
    breakages: &mut BTreeMap<PackageId, TaggedDependencyError>,
) -> Result<MainStatus, Error> {
    let mut tx = db.begin().await?;
    let receipts = todo!(); // StopReceipts::new(&mut tx, id).await?;
    let last_status = receipts.status.get(&mut tx).await?;
    receipts.status.set(&mut tx, MainStatus::Stopping).await?;

    tx.save().await?;
    break_all_dependents_transitive(
        db,
        id,
        DependencyError::NotRunning,
        breakages,
        &receipts.breaks,
    )
    .await?;

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
#[instrument(skip(ctx))]
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

#[instrument(skip(ctx))]
pub async fn stop_impl(ctx: RpcContext, id: PackageId) -> Result<MainStatus, Error> {
    let mut db = ctx.db.handle();
    let mut tx = db.begin().await?;
    let version = todo!(); /* crate::db::DatabaseModel::new()
                           .package_data()
                           .idx_model(&id)
                           .expect(&mut tx)
                           .await?
                           .installed()
                           .expect(&mut tx)
                           .await?
                           .manifest()
                           .version()
                           .get(&mut tx)
                           .await?
                           .clone();*/

    let last_statuts = stop_common(&mut tx, &id, &mut BTreeMap::new()).await?;

    tx.commit().await?;
    ctx.managers
        .get(&(id, version))
        .await
        .ok_or_else(|| Error::new(eyre!("Manager not found"), ErrorKind::InvalidRequest))?
        .stop();

    Ok(last_statuts)
}

#[command(display(display_none), metadata(sync_db = true))]
pub async fn restart(#[context] ctx: RpcContext, #[arg] id: PackageId) -> Result<(), Error> {
    let mut db = ctx.db.handle();
    let mut tx = db.begin().await?;
    let version = todo!(); /* crate::db::DatabaseModel::new()
                           .package_data()
                           .idx_model(&id)
                           .expect(&mut tx)
                           .await?
                           .installed()
                           .expect(&mut tx)
                           .await?
                           .manifest()
                           .version()
                           .get(&mut tx)
                           .await?
                           .clone();*/

    tx.commit().await?;

    ctx.managers
        .get(&(id, version))
        .await
        .ok_or_else(|| Error::new(eyre!("Manager not found"), ErrorKind::InvalidRequest))?
        .restart()
        .await;

    Ok(())
}
