use std::collections::BTreeMap;
use std::sync::atomic::{AtomicBool, Ordering};

use patch_db::{DbHandle, LockType};
use tracing::instrument;

use crate::context::RpcContext;
use crate::dependencies::{break_transitive, heal_transitive, DependencyError};
use crate::s9pk::manifest::PackageId;
use crate::status::health_check::{HealthCheckId, HealthCheckResult};
use crate::status::MainStatus;
use crate::Error;

#[instrument(skip(ctx, db))]
pub async fn check<Db: DbHandle>(
    ctx: &RpcContext,
    db: &mut Db,
    id: &PackageId,
    should_commit: &AtomicBool,
) -> Result<(), Error> {
    let mut tx = db.begin().await?;

    let mut checkpoint = tx.begin().await?;

    let installed_model = crate::db::DatabaseModel::new()
        .package_data()
        .idx_model(id)
        .expect(&mut checkpoint)
        .await?
        .installed()
        .expect(&mut checkpoint)
        .await?;

    let manifest = installed_model
        .clone()
        .manifest()
        .get(&mut checkpoint, true)
        .await?
        .into_owned();

    let started = installed_model
        .clone()
        .status()
        .main()
        .started()
        .get(&mut checkpoint, true)
        .await?
        .into_owned();

    checkpoint.save().await?;

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

    let mut checkpoint = tx.begin().await?;

    crate::db::DatabaseModel::new()
        .package_data()
        .lock(&mut checkpoint, LockType::Write)
        .await?;

    let mut status = crate::db::DatabaseModel::new()
        .package_data()
        .idx_model(id)
        .expect(&mut checkpoint)
        .await?
        .installed()
        .expect(&mut checkpoint)
        .await?
        .status()
        .main()
        .get_mut(&mut checkpoint)
        .await?;

    match &mut *status {
        MainStatus::Running { health, .. } => {
            *health = health_results.clone();
        }
        _ => (),
    }

    status.save(&mut checkpoint).await?;

    let current_dependents = installed_model
        .current_dependents()
        .get(&mut checkpoint, true)
        .await?;

    checkpoint.save().await?;

    tracing::debug!("Checking health of {}", id);
    let receipts = crate::dependencies::BreakTransitiveReceipts::new(&mut tx).await?;
    tracing::debug!("Got receipts {}", id);

    for (dependent, info) in &*current_dependents {
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
