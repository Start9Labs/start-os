use clap::Parser;
use color_eyre::eyre::eyre;
use models::PackageId;
use rpc_toolkit::command;
use serde::{Deserialize, Serialize};
use tracing::instrument;

use crate::context::RpcContext;
use crate::prelude::*;
use crate::status::MainStatus;
use crate::Error;

#[derive(Deserialize, Serialize, Parser)]
#[serde(rename_all = "kebab-case")]
#[command(rename_all = "kebab-case")]
pub struct ControlParams {
    pub id: PackageId,
}

#[instrument(skip_all)]
pub async fn start(ctx: RpcContext, ControlParams { id }: ControlParams) -> Result<(), Error> {
    ctx.managers
        .get(&id)
        .await
        .or_not_found(lazy_format!("Manager for {id}"))?
        .start()
        .await;

    Ok(())
}

pub async fn stop(
    ctx: RpcContext,
    ControlParams { id }: ControlParams,
) -> Result<MainStatus, Error> {
    let peek = ctx.db.peek().await;
    let version = peek
        .as_package_data()
        .as_idx(&id)
        .or_not_found(&id)?
        .as_installed()
        .or_not_found(&id)?
        .as_manifest()
        .as_version()
        .de()?;

    let last_statuts = ctx
        .db
        .mutate(|v| {
            v.as_package_data_mut()
                .as_idx_mut(&id)
                .and_then(|x| x.as_installed_mut())
                .ok_or_else(|| Error::new(eyre!("{} is not installed", id), ErrorKind::NotFound))?
                .as_status_mut()
                .as_main_mut()
                .replace(&MainStatus::Stopping)
        })
        .await?;

    ctx.managers
        .get(&id)
        .await
        .ok_or_else(|| Error::new(eyre!("Manager not found"), crate::ErrorKind::InvalidRequest))?
        .stop()
        .await;

    Ok(last_statuts)
}

pub async fn restart(ctx: RpcContext, ControlParams { id }: ControlParams) -> Result<(), Error> {
    let peek = ctx.db.peek().await;
    let version = peek
        .as_package_data()
        .as_idx(&id)
        .or_not_found(&id)?
        .expect_as_installed()?
        .as_manifest()
        .as_version()
        .de()?;

    ctx.managers
        .get(&id)
        .await
        .ok_or_else(|| Error::new(eyre!("Manager not found"), crate::ErrorKind::InvalidRequest))?
        .restart()
        .await;

    Ok(())
}
