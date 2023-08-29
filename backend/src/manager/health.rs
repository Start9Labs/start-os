use models::OptionExt;
use tracing::instrument;

use crate::context::RpcContext;
use crate::prelude::*;
use crate::s9pk::manifest::PackageId;
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

    Ok(())
}
