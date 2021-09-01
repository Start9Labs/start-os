use anyhow::anyhow;
use chrono::{DateTime, Utc};
use indexmap::IndexMap;
use patch_db::DbHandle;
use rpc_toolkit::command;

use crate::context::RpcContext;
use crate::db::util::WithRevision;
use crate::s9pk::manifest::PackageId;
use crate::status::MainStatus;
use crate::util::display_none;
use crate::{Error, ResultExt};

#[command(display(display_none))]
pub async fn start(
    #[context] ctx: RpcContext,
    #[arg] id: PackageId,
) -> Result<WithRevision<()>, Error> {
    let mut db = ctx.db.handle();
    let mut tx = db.begin().await?;
    let installed = crate::db::DatabaseModel::new()
        .package_data()
        .idx_model(&id)
        .and_then(|pkg| pkg.installed())
        .expect(&mut tx)
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
        .get(&mut tx, true)
        .await?
        .to_owned();
    let mut status = installed.status().main().get_mut(&mut tx).await?;

    *status = MainStatus::Running {
        started: Utc::now(),
        health: IndexMap::new(),
    };
    status
        .synchronize(
            &*ctx.managers.get(&(id, version)).await.ok_or_else(|| {
                Error::new(anyhow!("Manager not found"), crate::ErrorKind::Docker)
            })?,
        )
        .await?;
    status.save(&mut tx).await?;

    Ok(WithRevision {
        revision: tx.commit(None).await?,
        response: (),
    })
}

#[command(display(display_none))]
pub async fn stop(
    #[context] ctx: RpcContext,
    #[arg] id: PackageId,
) -> Result<WithRevision<()>, Error> {
    let mut db = ctx.db.handle();
    let mut tx = db.begin().await?;

    let mut status = crate::db::DatabaseModel::new()
        .package_data()
        .idx_model(&id)
        .and_then(|pkg| pkg.installed())
        .expect(&mut tx)
        .await
        .with_ctx(|_| {
            (
                crate::ErrorKind::NotFound,
                format!("{} is not installed", id),
            )
        })?
        .status()
        .main()
        .get_mut(&mut tx)
        .await?;

    *status = MainStatus::Stopping;
    status.save(&mut tx).await?;

    Ok(WithRevision {
        revision: tx.commit(None).await?,
        response: (),
    })
}
