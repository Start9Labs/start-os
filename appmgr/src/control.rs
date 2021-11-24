use std::collections::BTreeMap;

use chrono::Utc;
use color_eyre::eyre::eyre;
use patch_db::DbHandle;
use rpc_toolkit::command;
use tracing::instrument;

use crate::context::RpcContext;
use crate::db::util::WithRevision;
use crate::dependencies::{
    break_all_dependents_transitive, heal_all_dependents_transitive, BreakageRes, DependencyError,
    TaggedDependencyError,
};
use crate::s9pk::manifest::PackageId;
use crate::status::MainStatus;
use crate::util::{display_none, display_serializable};
use crate::{Error, ResultExt};

#[command(display(display_none))]
#[instrument(skip(ctx))]
pub async fn start(
    #[context] ctx: RpcContext,
    #[arg] id: PackageId,
) -> Result<WithRevision<()>, Error> {
    let mut db = ctx.db.handle();
    let mut tx = db.begin().await?;
    let installed = crate::db::DatabaseModel::new()
        .package_data()
        .idx_model(&id)
        .and_then(|pkg| pkg.installed())
        .expect(&mut tx)
        .await
        .with_ctx(|_| {
            (
                crate::ErrorKind::NotFound,
                format!("{} is not installed", id),
            )
        })?;
    let version = installed
        .clone()
        .manifest()
        .version()
        .get(&mut tx, true)
        .await?
        .to_owned();
    let mut status = installed.status().main().get_mut(&mut tx).await?;

    *status = MainStatus::Starting;
    status.save(&mut tx).await?;
    heal_all_dependents_transitive(&ctx, &mut tx, &id).await?;

    let revision = tx.commit(None).await?;

    ctx.managers
        .get(&(id, version))
        .await
        .ok_or_else(|| Error::new(eyre!("Manager not found"), crate::ErrorKind::InvalidRequest))?
        .synchronize()
        .await;

    Ok(WithRevision {
        revision,
        response: (),
    })
}

#[instrument(skip(db))]
async fn stop_common<Db: DbHandle>(
    db: &mut Db,
    id: &PackageId,
    breakages: &mut BTreeMap<PackageId, TaggedDependencyError>,
) -> Result<(), Error> {
    let mut tx = db.begin().await?;
    let mut status = crate::db::DatabaseModel::new()
        .package_data()
        .idx_model(&id)
        .and_then(|pkg| pkg.installed())
        .expect(&mut tx)
        .await
        .with_ctx(|_| {
            (
                crate::ErrorKind::NotFound,
                format!("{} is not installed", id),
            )
        })?
        .status()
        .main()
        .get_mut(&mut tx)
        .await?;

    *status = MainStatus::Stopping;
    status.save(&mut tx).await?;
    tx.save().await?;
    break_all_dependents_transitive(db, &id, DependencyError::NotRunning, breakages).await?;

    Ok(())
}

#[command(subcommands(self(stop_impl(async)), stop_dry), display(display_none))]
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
pub async fn stop_impl(ctx: RpcContext, id: PackageId) -> Result<WithRevision<()>, Error> {
    let mut db = ctx.db.handle();
    let mut tx = db.begin().await?;

    stop_common(&mut tx, &id, &mut BTreeMap::new()).await?;

    Ok(WithRevision {
        revision: tx.commit(None).await?,
        response: (),
    })
}
