use std::collections::BTreeMap;

use color_eyre::eyre::eyre;
use rpc_toolkit::command;
use tracing::instrument;

use crate::dependencies::{
    heal_all_dependents_transitive, BreakageRes, DependencyError, TaggedDependencyError,
};
use crate::prelude::*;
use crate::s9pk::manifest::PackageId;
use crate::status::MainStatus;
use crate::util::display_none;
use crate::util::serde::display_serializable;
use crate::{context::RpcContext, db::model::Database};

#[command(display(display_none), metadata(sync_db = true))]
#[instrument(skip(ctx))]
pub async fn start(#[context] ctx: RpcContext, #[arg] id: PackageId) -> Result<(), Error> {
    ctx.managers
        .get(&id)
        .await
        .ok_or_else(|| Error::new(eyre!("Manager not found"), ErrorKind::InvalidRequest))?
        .start();

    Ok(())
}

#[command(
    subcommands(self(stop_impl(async)), stop_dry),
    display(display_none),
    metadata(sync_db = true)
)]
pub fn stop(#[arg] id: PackageId) -> Result<PackageId, Error> {
    Ok(id)
}

#[command(rename = "dry", display(display_serializable))]
#[instrument(skip(ctx))]
pub async fn stop_dry(
    #[context] ctx: RpcContext,
    #[parent_data] id: PackageId,
) -> Result<BreakageRes, Error> {
    let db = ctx.db.peek().await?;

    let error = TaggedDependencyError {
        dependency: id.clone(),
        error: DependencyError::NotRunning,
    };
    Ok(BreakageRes::default().not_running_dependencies(&db, &id, error))
}
trait NotRunningDependencies {
    fn not_running_dependencies(
        self,
        db: &<Database as HasModel>::Model,
        id: &PackageId,
        error: TaggedDependencyError,
    ) -> Result<Self, Error>;
}
impl NotRunningDependencies for BreakageRes {
    fn not_running_dependencies(
        self,
        db: &<Database as HasModel>::Model,
        id: &PackageId,
        error: TaggedDependencyError,
    ) -> Result<Self, Error> {
        let mut not_running = self;
        let dependencies = db
            .package_data()
            .idx(id)
            .or_not_found(id)?
            .manifest()
            .dependencies()
            .keys()?;
        for dep in dependencies {
            if not_running.0.insert(dep.clone(), error.clone()).is_none() {
                not_running = not_running.not_running_dependencies(db, &dep, error.clone())?;
            }
        }

        Ok(not_running)
    }
}

#[instrument(skip(ctx))]
pub async fn stop_impl(ctx: RpcContext, id: PackageId) -> Result<(), Error> {
    let mut breakages = BTreeMap::new();
    ctx.managers
        .get(&id)
        .await
        .ok_or_else(|| Error::new(eyre!("Manager not found"), ErrorKind::InvalidRequest))?
        .stop();

    Ok(())
}

#[command(display(display_none), metadata(sync_db = true))]
pub async fn restart(#[context] ctx: RpcContext, #[arg] id: PackageId) -> Result<(), Error> {
    ctx.managers
        .get(&id)
        .await
        .ok_or_else(|| Error::new(eyre!("Manager not found"), ErrorKind::InvalidRequest))?
        .restart()
        .await;

    Ok(())
}
