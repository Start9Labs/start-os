use std::collections::BTreeMap;

use models::OptionExt;
use tracing::instrument;

use crate::context::RpcContext;
use crate::db::model::CurrentDependents;
use crate::db::prelude::*;
use crate::dependencies::{break_transitive, heal_transitive, DependencyError};
use crate::s9pk::manifest::{Manifest, PackageId};
use crate::status::health_check::{HealthCheckId, HealthCheckResult};
use crate::status::MainStatus;
use crate::Error;

/// So, this is used for a service to run a health check cycle, go out and run the health checks, and store those in the db
#[instrument(skip_all)]
pub async fn check(ctx: &RpcContext, id: &PackageId) -> Result<(), Error> {
    let (manifest, started) = {
        let pde = ctx
            .db
            .peek()
            .await?
            .as_package_data()
            .as_idx(id)
            .or_not_found(id)?
            .expect_as_installed()?;

        let manifest = pde.as_installed().as_manifest().de()?;

        let started = pde.as_installed().as_status().as_main().de()?.started();

        (manifest, started)
    };

    let health_results = if let Some(started) = started {
        tracing::debug!("Checking health of {}", id);
        manifest
            .health_checks
            .check_all(ctx, started, id, &manifest.version, &manifest.volumes)
            .await?
    } else {
        return Ok(());
    };

    let current_dependents = ctx
        .db
        .mutate(|v| {
            let mut pde = v
                .as_package_data_mut()
                .as_idx_mut(id)
                .or_not_found(id)?
                .expect_as_installed_mut()?;
            let mut status = pde.as_installed_mut().as_status_mut().as_main_mut();

            if let MainStatus::Running { health: _, started } = status.de()? {
                status.ser(&MainStatus::Running {
                    health: health_results.clone(),
                    started,
                })?;
            }

            pde.as_installed().as_current_dependents().de()
        })
        .await?;

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
