use std::collections::BTreeMap;

use color_eyre::eyre::eyre;
use patch_db::{DbHandle, LockReceipt, LockType};
use rpc_toolkit::command;
use tracing::instrument;

use crate::context::RpcContext;
use crate::db::util::WithRevision;
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
    pub async fn new<'a>(db: &'a mut impl DbHandle, id: &PackageId) -> Result<Self, Error> {
        let mut locks = Vec::new();

        let setup = Self::setup(&mut locks, id);
        Ok(setup(&db.lock_all(locks).await?)?)
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

#[command(display(display_none))]
#[instrument(skip(ctx))]
pub async fn start(
    #[context] ctx: RpcContext,
    #[arg] id: PackageId,
) -> Result<WithRevision<()>, Error> {
    let mut db = ctx.db.handle();
    let mut tx = db.begin().await?;
    let receipts = StartReceipts::new(&mut tx, &id).await?;
    let version = receipts.version.get(&mut tx).await?;
    receipts
        .status
        .set(&mut tx, MainStatus::Starting { restarting: false })
        .await?;
    heal_all_dependents_transitive(&ctx, &mut tx, &id, &receipts.dependency_receipt).await?;

    let revision = tx.commit(None).await?;
    drop(receipts);

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
#[derive(Clone)]
pub struct StopReceipts {
    breaks: crate::dependencies::BreakTransitiveReceipts,
    status: LockReceipt<MainStatus, ()>,
}

impl StopReceipts {
    pub async fn new<'a>(db: &'a mut impl DbHandle, id: &PackageId) -> Result<Self, Error> {
        let mut locks = Vec::new();

        let setup = Self::setup(&mut locks, id);
        Ok(setup(&db.lock_all(locks).await?)?)
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

#[instrument(skip(db))]
async fn stop_common<Db: DbHandle>(
    db: &mut Db,
    id: &PackageId,
    breakages: &mut BTreeMap<PackageId, TaggedDependencyError>,
) -> Result<(), Error> {
    let mut tx = db.begin().await?;
    let receipts = StopReceipts::new(&mut tx, id).await?;
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

#[command(display(display_none))]
pub async fn restart(
    #[context] ctx: RpcContext,
    #[arg] id: PackageId,
) -> Result<WithRevision<()>, Error> {
    let mut db = ctx.db.handle();
    let mut tx = db.begin().await?;

    let mut status = crate::db::DatabaseModel::new()
        .package_data()
        .idx_model(&id)
        .and_then(|pde| pde.installed())
        .map(|i| i.status().main())
        .get_mut(&mut tx)
        .await?;
    if !matches!(&*status, Some(MainStatus::Running { .. })) {
        return Err(Error::new(
            eyre!("{} is not running", id),
            crate::ErrorKind::InvalidRequest,
        ));
    }
    *status = Some(MainStatus::Restarting);
    status.save(&mut tx).await?;

    Ok(WithRevision {
        revision: tx.commit(None).await?,
        response: (),
    })
}
