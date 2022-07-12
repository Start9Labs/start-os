use std::collections::BTreeMap;
use std::sync::atomic::{AtomicBool, Ordering};

use patch_db::{DbHandle, LockReceipt, LockType};
use tracing::instrument;

use crate::context::RpcContext;
use crate::db::model::CurrentDependents;
use crate::dependencies::{break_transitive, heal_transitive, DependencyError};
use crate::s9pk::manifest::{Manifest, PackageId};
use crate::status::health_check::{HealthCheckId, HealthCheckResult};
use crate::status::MainStatus;
use crate::Error;

struct HealthCheckPreInformationReceipt {
    status_model: LockReceipt<MainStatus, ()>,
    manifest: LockReceipt<Manifest, ()>,
}
impl HealthCheckPreInformationReceipt {
    pub async fn new(db: &'_ mut impl DbHandle, id: &PackageId) -> Result<Self, Error> {
        let mut locks = Vec::new();

        let setup = Self::setup(&mut locks, id);
        setup(&db.lock_all(locks).await?)
    }

    pub fn setup(
        locks: &mut Vec<patch_db::LockTargetId>,
        id: &PackageId,
    ) -> impl FnOnce(&patch_db::Verifier) -> Result<Self, Error> {
        let status_model = crate::db::DatabaseModel::new()
            .package_data()
            .idx_model(id)
            .and_then(|x| x.installed())
            .map(|x| x.status().main())
            .make_locker(LockType::Read)
            .add_to_keys(locks);
        let manifest = crate::db::DatabaseModel::new()
            .package_data()
            .idx_model(id)
            .and_then(|x| x.installed())
            .map(|x| x.manifest())
            .make_locker(LockType::Read)
            .add_to_keys(locks);
        move |skeleton_key| {
            Ok(Self {
                status_model: status_model.verify(skeleton_key)?,
                manifest: manifest.verify(skeleton_key)?,
            })
        }
    }
}

struct HealthCheckStatusReceipt {
    status: LockReceipt<MainStatus, ()>,
    current_dependents: LockReceipt<CurrentDependents, ()>,
}
impl HealthCheckStatusReceipt {
    pub async fn new(db: &'_ mut impl DbHandle, id: &PackageId) -> Result<Self, Error> {
        let mut locks = Vec::new();

        let setup = Self::setup(&mut locks, id);
        setup(&db.lock_all(locks).await?)
    }

    pub fn setup(
        locks: &mut Vec<patch_db::LockTargetId>,
        id: &PackageId,
    ) -> impl FnOnce(&patch_db::Verifier) -> Result<Self, Error> {
        let status = crate::db::DatabaseModel::new()
            .package_data()
            .idx_model(id)
            .and_then(|x| x.installed())
            .map(|x| x.status().main())
            .make_locker(LockType::Write)
            .add_to_keys(locks);
        let current_dependents = crate::db::DatabaseModel::new()
            .package_data()
            .idx_model(id)
            .and_then(|x| x.installed())
            .map(|x| x.current_dependents())
            .make_locker(LockType::Read)
            .add_to_keys(locks);
        move |skeleton_key| {
            Ok(Self {
                status: status.verify(skeleton_key)?,
                current_dependents: current_dependents.verify(skeleton_key)?,
            })
        }
    }
}

#[instrument(skip(ctx, db))]
pub async fn check<Db: DbHandle>(
    ctx: &RpcContext,
    db: &mut Db,
    id: &PackageId,
    should_commit: &AtomicBool,
) -> Result<(), Error> {
    let mut tx = db.begin().await?;
    let (manifest, started) = {
        let mut checkpoint = tx.begin().await?;
        let receipts = HealthCheckPreInformationReceipt::new(&mut checkpoint, id).await?;

        let manifest = receipts.manifest.get(&mut checkpoint).await?;

        let started = receipts.status_model.get(&mut checkpoint).await?.started();

        checkpoint.save().await?;
        (manifest, started)
    };

    let health_results = if let Some(started) = started {
        manifest
            .health_checks
            .check_all(ctx, started, id, &manifest.version, &manifest.volumes)
            .await?
    } else {
        return Ok(());
    };

    if !should_commit.load(Ordering::SeqCst) {
        return Ok(());
    }
    let current_dependents = {
        let mut checkpoint = tx.begin().await?;
        let receipts = HealthCheckStatusReceipt::new(&mut checkpoint, id).await?;

        let status = receipts.status.get(&mut checkpoint).await?;

        match status {
            MainStatus::Running { health, started } => {
                receipts
                    .status
                    .set(
                        &mut checkpoint,
                        MainStatus::Running {
                            health: health_results.clone(),
                            started,
                        },
                    )
                    .await?;
            }
            _ => (),
        }
        let current_dependents = receipts.current_dependents.get(&mut checkpoint).await?;

        checkpoint.save().await?;
        current_dependents
    };

    tracing::debug!("Checking health of {}", id);
    let receipts = crate::dependencies::BreakTransitiveReceipts::new(&mut tx).await?;
    tracing::debug!("Got receipts {}", id);

    for (dependent, info) in (current_dependents).0.iter() {
        let failures: BTreeMap<HealthCheckId, HealthCheckResult> = health_results
            .iter()
            .filter(|(_, hc_res)| !matches!(hc_res, HealthCheckResult::Success { .. }))
            .filter(|(hc_id, _)| info.health_checks.contains(hc_id))
            .map(|(k, v)| (k.clone(), v.clone()))
            .collect();

        if !failures.is_empty() {
            break_transitive(
                &mut tx,
                &dependent,
                id,
                DependencyError::HealthChecksFailed { failures },
                &mut BTreeMap::new(),
                &receipts,
            )
            .await?;
        } else {
            heal_transitive(ctx, &mut tx, &dependent, id, &receipts.dependency_receipt).await?;
        }
    }

    tx.save().await?;

    Ok(())
}
