use std::collections::BTreeMap;

use rpc_toolkit::command;
use tracing::instrument;

use crate::config::not_found;
use crate::context::RpcContext;
use crate::dependencies::{break_transitive, BreakageRes, DependencyError};
use crate::prelude::*;
use crate::s9pk::manifest::PackageId;
use crate::util::serde::display_serializable;
use crate::util::Version;

#[command(subcommands(dry))]
pub async fn update() -> Result<(), Error> {
    Ok(())
}

#[instrument(skip_all)]
#[command(display(display_serializable))]
pub async fn dry(
    #[context] ctx: RpcContext,
    #[arg] id: PackageId,
    #[arg] version: Version,
) -> Result<BreakageRes, Error> {
    let mut db = ctx.db.handle();
    let mut tx = db.begin().await?;
    let mut breakages = BTreeMap::new();
    let receipts = todo!("BLUJ"); // UpdateReceipts::new(&mut tx).await?;

    for dependent in receipts
        .current_dependents
        .get(&mut tx, &id)
        .await?
        .ok_or_else(not_found)?
        .0
        .keys()
        .into_iter()
        .filter(|dependent| &&id != dependent)
    {
        if let Some(dep_info) = receipts.dependency.get(&mut tx, (&dependent, &id)).await? {
            let version_req = dep_info.version;
            if !version.satisfies(&version_req) {
                break_transitive(
                    &dependent,
                    &id,
                    DependencyError::IncorrectVersion {
                        expected: version_req,
                        received: version.clone(),
                    },
                    &mut breakages,
                )
                .await?;
            }
        }
    }
    tx.abort().await?;
    Ok(BreakageRes(breakages))
}
