use models::{OptionExt, PackageId};
use tracing::instrument;

use crate::context::RpcContext;
use crate::prelude::*;
use crate::status::MainStatus;
use crate::Error;

/// So, this is used for a service to run a health check cycle, go out and run the health checks, and store those in the db
#[instrument(skip_all)]
pub async fn check(ctx: &RpcContext, id: &PackageId) -> Result<(), Error> {
    let (manifest, started) = {
        let peeked = ctx.db.peek().await;
        let pde = peeked
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

    ctx.db
        .mutate(|v| {
            let pde = v
                .as_package_data_mut()
                .as_idx_mut(id)
                .or_not_found(id)?
                .expect_as_installed_mut()?;
            let status = pde.as_installed_mut().as_status_mut().as_main_mut();

            if let MainStatus::Running { health: _, started } = status.de()? {
                status.ser(&MainStatus::Running {
                    health: health_results.clone(),
                    started,
                })?;
            }
            Ok(())
        })
        .await
}
