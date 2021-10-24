use std::collections::BTreeMap;

use patch_db::DbHandle;
use tracing::instrument;

use crate::context::RpcContext;
use crate::dependencies::{break_transitive, DependencyError};
use crate::s9pk::manifest::PackageId;
use crate::status::health_check::{HealthCheckId, HealthCheckResult};
use crate::status::MainStatus;
use crate::Error;

#[instrument(skip(ctx, db))]
pub async fn check<Db: DbHandle>(
    ctx: &RpcContext,
    db: &mut Db,
    id: &PackageId,
) -> Result<(), Error> {
    let mut tx = db.begin().await?;

    let installed_model = crate::db::DatabaseModel::new()
        .package_data()
        .idx_model(id)
        .expect(&mut tx)
        .await?
        .installed()
        .expect(&mut tx)
        .await?;

    let mut checkpoint = tx.begin().await?;

    let manifest = installed_model
        .clone()
        .manifest()
        .get(&mut checkpoint, true)
        .await?;

    let mut status = installed_model
        .clone()
        .status()
        .get_mut(&mut checkpoint)
        .await?;

    status.main.check(&ctx, &mut checkpoint, &*manifest).await?;

    let failed = match &status.main {
        MainStatus::Running { health, .. } => health.clone(),
        MainStatus::BackingUp { health, .. } => health.clone(),
        _ => BTreeMap::new(),
    };

    status.save(&mut checkpoint).await?;

    checkpoint.save().await?;

    let current_dependents = installed_model
        .current_dependents()
        .get(&mut tx, true)
        .await?;
    for (dependent, info) in &*current_dependents {
        let failures: BTreeMap<HealthCheckId, HealthCheckResult> = failed
            .iter()
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
            )
            .await?;
        }
    }

    tx.save().await?;

    Ok(())
}
