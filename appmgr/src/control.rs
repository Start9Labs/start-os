use anyhow::anyhow;
use chrono::{DateTime, Utc};
use indexmap::IndexMap;
use rpc_toolkit::command;

use crate::context::EitherContext;
use crate::s9pk::manifest::PackageId;
use crate::status::MainStatus;
use crate::util::display_none;
use crate::{Error, ResultExt};

#[command(display(display_none))]
pub async fn start(#[context] ctx: EitherContext, #[arg] id: PackageId) -> Result<(), Error> {
    let rpc_ctx = ctx.as_rpc().unwrap();
    let mut db = rpc_ctx.db.handle();
    let installed = crate::db::DatabaseModel::new()
        .package_data()
        .idx_model(&id)
        .and_then(|pkg| pkg.installed())
        .expect(&mut db)
        .await
        .with_ctx(|_| {
            (
                crate::ErrorKind::NotFound,
                format!("{} is not installed", id),
            )
        })?;
    let version = installed
        .clone()
        .manifest()
        .version()
        .get(&mut db, true)
        .await?
        .to_owned();
    let mut status = installed.status().main().get_mut(&mut db).await?;

    *status = MainStatus::Running {
        started: Utc::now(),
        health: IndexMap::new(),
    };
    status
        .synchronize(
            &*rpc_ctx.managers.get(&(id, version)).await.ok_or_else(|| {
                Error::new(anyhow!("Manager not found"), crate::ErrorKind::Docker)
            })?,
        )
        .await?;
    status.save(&mut db).await?;

    Ok(())
}

#[command(display(display_none))]
pub async fn stop(#[context] ctx: EitherContext, #[arg] id: PackageId) -> Result<(), Error> {
    let rpc_ctx = ctx.as_rpc().unwrap();
    let mut db = rpc_ctx.db.handle();
    let mut status = crate::db::DatabaseModel::new()
        .package_data()
        .idx_model(&id)
        .and_then(|pkg| pkg.installed())
        .expect(&mut db)
        .await
        .with_ctx(|_| {
            (
                crate::ErrorKind::NotFound,
                format!("{} is not installed", id),
            )
        })?
        .status()
        .main()
        .get_mut(&mut db)
        .await?;

    *status = MainStatus::Stopping;
    status.save(&mut db).await?;

    Ok(())
}
