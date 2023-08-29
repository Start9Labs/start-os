use std::collections::BTreeMap;

use color_eyre::eyre::eyre;
use rpc_toolkit::command;
use tracing::instrument;

use crate::context::RpcContext;
use crate::prelude::*;
use crate::s9pk::manifest::PackageId;
use crate::status::MainStatus;
use crate::util::display_none;
use crate::util::serde::display_serializable;
use crate::Error;

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
    heal_all_dependents_transitive(&ctx, &id).await?;

    ctx.managers
        .get(&(id, version))
        .await
        .ok_or_else(|| Error::new(eyre!("Manager not found"), crate::ErrorKind::InvalidRequest))?
        .start();

    Ok(())
}

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
