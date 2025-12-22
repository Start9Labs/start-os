use clap::Parser;
use serde::{Deserialize, Serialize};
use tracing::instrument;
use ts_rs::TS;

use crate::context::RpcContext;
use crate::prelude::*;
use crate::{Error, PackageId};

#[derive(Deserialize, Serialize, Parser, TS)]
#[serde(rename_all = "camelCase")]
#[command(rename_all = "kebab-case")]
pub struct ControlParams {
    pub id: PackageId,
}

#[instrument(skip_all)]
pub async fn start(ctx: RpcContext, ControlParams { id }: ControlParams) -> Result<(), Error> {
    ctx.db
        .mutate(|db| {
            db.as_public_mut()
                .as_package_data_mut()
                .as_idx_mut(&id)
                .or_not_found(&id)?
                .as_status_info_mut()
                .as_desired_mut()
                .map_mutate(|s| Ok(s.start()))
        })
        .await
        .result?;

    Ok(())
}

pub async fn stop(ctx: RpcContext, ControlParams { id }: ControlParams) -> Result<(), Error> {
    ctx.db
        .mutate(|db| {
            db.as_public_mut()
                .as_package_data_mut()
                .as_idx_mut(&id)
                .or_not_found(&id)?
                .as_status_info_mut()
                .stop()
        })
        .await
        .result?;

    Ok(())
}

pub async fn restart(ctx: RpcContext, ControlParams { id }: ControlParams) -> Result<(), Error> {
    ctx.db
        .mutate(|db| {
            db.as_public_mut()
                .as_package_data_mut()
                .as_idx_mut(&id)
                .or_not_found(&id)?
                .as_status_info_mut()
                .as_desired_mut()
                .map_mutate(|s| Ok(s.restart()))
        })
        .await
        .result?;

    Ok(())
}
