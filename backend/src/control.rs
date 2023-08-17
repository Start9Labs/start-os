use std::collections::BTreeMap;

use color_eyre::eyre::eyre;
use patch_db::{DbHandle, LockReceipt, LockType};
use rpc_toolkit::command;
use tracing::instrument;

use crate::context::RpcContext;
use crate::dependencies::{
    break_all_dependents_transitive, heal_all_dependents_transitive, BreakageRes, DependencyError,
    DependencyReceipt, TaggedDependencyError,
};
use crate::s9pk::manifest::PackageId;
use crate::status::MainStatus;
use crate::util::display_none;
use crate::util::serde::display_serializable;
use crate::Error;

#[derive(Clone)]
pub struct StartReceipts {
    dependency_receipt: DependencyReceipt,
    status: LockReceipt<MainStatus, ()>,
    version: LockReceipt<crate::util::Version, ()>,
}

impl StartReceipts {
    pub async fn new(db: &mut impl DbHandle, id: &PackageId) -> Result<Self, Error> {
        let mut locks = Vec::new();

        let setup = Self::setup(&mut locks, id);
        setup(&db.lock_all(locks).await?)
    }

    pub fn setup(
        locks: &mut Vec<patch_db::LockTargetId>,
        id: &PackageId,
    ) -> impl FnOnce(&patch_db::Verifier) -> Result<Self, Error> {
        let dependency_receipt = DependencyReceipt::setup(locks);
        let status = crate::db::DatabaseModel::new()
            .package_data()
            .idx_model(id)
            .and_then(|x| x.installed())
            .map(|x| x.status().main())
            .make_locker(LockType::Write)
            .add_to_keys(locks);
        let version = crate::db::DatabaseModel::new()
            .package_data()
            .idx_model(id)
            .and_then(|x| x.installed())
            .map(|x| x.manifest().version())
            .make_locker(LockType::Read)
            .add_to_keys(locks);
        move |skeleton_key| {
            Ok(Self {
                dependency_receipt: dependency_receipt(skeleton_key)?,
                status: status.verify(skeleton_key)?,
                version: version.verify(skeleton_key)?,
            })
        }
    }
}

#[command(display(display_none), metadata(sync_db = true))]
#[instrument(skip_all)]
pub async fn start(#[context] ctx: RpcContext, #[arg] id: PackageId) -> Result<(), Error> {
    let mut db = ctx.db.handle();
    let mut tx = db.begin().await?;
    let receipts = StartReceipts::new(&mut tx, &id).await?;
    let version = receipts.version.get(&mut tx).await?;
    receipts.status.set(&mut tx, MainStatus::Starting).await?;
    heal_all_dependents_transitive(&ctx, &mut tx, &id, &receipts.dependency_receipt).await?;

    tx.commit().await?;
    drop(receipts);

    ctx.managers
        .get(&(id, version))
        .await
        .ok_or_else(|| Error::new(eyre!("Manager not found"), crate::ErrorKind::InvalidRequest))?
        .start();

    Ok(())
}
#[derive(Clone)]
pub struct StopReceipts {
    breaks: crate::dependencies::BreakTransitiveReceipts,
    status: LockReceipt<MainStatus, ()>,
}

impl StopReceipts {
    pub async fn new<'a>(db: &'a mut impl DbHandle, id: &PackageId) -> Result<Self, Error> {
        let mut locks = Vec::new();

        let setup = Self::setup(&mut locks, id);
        setup(&db.lock_all(locks).await?)
    }

    pub fn setup(
        locks: &mut Vec<patch_db::LockTargetId>,
        id: &PackageId,
    ) -> impl FnOnce(&patch_db::Verifier) -> Result<Self, Error> {
        let breaks = crate::dependencies::BreakTransitiveReceipts::setup(locks);
        let status = crate::db::DatabaseModel::new()
            .package_data()
            .idx_model(id)
            .and_then(|x| x.installed())
            .map(|x| x.status().main())
            .make_locker(LockType::Write)
            .add_to_keys(locks);
        move |skeleton_key| {
            Ok(Self {
                breaks: breaks(skeleton_key)?,
                status: status.verify(skeleton_key)?,
            })
        }
    }
}

#[instrument(skip_all)]
pub async fn stop_common<Db: DbHandle>(
    db: &mut Db,
    id: &PackageId,
    breakages: &mut BTreeMap<PackageId, TaggedDependencyError>,
) -> Result<MainStatus, Error> {
    let mut tx = db.begin().await?;
    let receipts = StopReceipts::new(&mut tx, id).await?;
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
    let mut db = ctx.db.handle();
    let mut tx = db.begin().await?;
    let version = crate::db::DatabaseModel::new()
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
        .clone();

    let last_statuts = stop_common(&mut tx, &id, &mut BTreeMap::new()).await?;

    tx.commit().await?;
    ctx.managers
        .get(&(id, version))
        .await
        .ok_or_else(|| Error::new(eyre!("Manager not found"), crate::ErrorKind::InvalidRequest))?
        .stop();

    Ok(last_statuts)
}

#[command(display(display_none), metadata(sync_db = true))]
pub async fn restart(#[context] ctx: RpcContext, #[arg] id: PackageId) -> Result<(), Error> {
    let mut db = ctx.db.handle();
    let mut tx = db.begin().await?;
    let version = crate::db::DatabaseModel::new()
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
        .clone();

    tx.commit().await?;

    ctx.managers
        .get(&(id, version))
        .await
        .ok_or_else(|| Error::new(eyre!("Manager not found"), crate::ErrorKind::InvalidRequest))?
        .restart()
        .await;

    Ok(())
}
