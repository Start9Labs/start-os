use std::collections::BTreeMap;

use tracing::instrument;

use crate::context::RpcContext;
use crate::dependencies::{break_transitive, heal_transitive, DependencyError};
use crate::prelude::*;
use crate::s9pk::manifest::{Manifest, PackageId};
use crate::status::health_check::{HealthCheckId, HealthCheckResult};
use crate::status::MainStatus;

#[instrument(skip(ctx))]
pub async fn check(ctx: &RpcContext, id: &PackageId) -> Result<(), Error> {
    let mut tx = todo!(); // db.begin().await?;
    let (manifest, started) = {
        let mut checkpoint = tx.begin().await?;
        let receipts = todo!(); // HealthCheckPreInformationReceipt::new(&mut checkpoint, id).await?;

        let manifest = receipts.manifest.get(&mut checkpoint).await?;

        let started = receipts.status_model.get(&mut checkpoint).await?.started();

        checkpoint.save().await?;
        (manifest, started)
    };

    let health_results = if let Some(started) = started {
        manifest
            .health_checks
            .check_all(
                ctx,
                &manifest.containers,
                started,
                id,
                &manifest.version,
                &manifest.volumes,
            )
            .await?
    } else {
        return Ok(());
    };

    let current_dependents = {
        let mut checkpoint = tx.begin().await?;
        let receipts = todo!(); // HealthCheckStatusReceipt::new(&mut checkpoint, id).await?;

        let status = receipts.status.get(&mut checkpoint).await?;

        if let MainStatus::Running { health: _, started } = status {
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
                &dependent,
                id,
                DependencyError::HealthChecksFailed { failures },
                &mut BTreeMap::new(),
                &receipts,
            )
            .await?;
        } else {
            heal_transitive(ctx, &dependent, id, &receipts.dependency_receipt).await?;
        }
    }

    tx.save().await?;

    Ok(())
}
