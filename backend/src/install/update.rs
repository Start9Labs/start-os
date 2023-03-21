use std::collections::BTreeMap;

use rpc_toolkit::command;
use tracing::instrument;

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
    let db = ctx.db.peek().await?;
    let mut breakages = BTreeMap::new();
    let dependencies = db
        .into_package_data()
        .into_idx(&id)
        .or_not_found(&id)?
        .into_manifest()
        .into_dependencies();

    for dependent in dependencies
        .keys()?
        .into_iter()
        .filter(|dependent| &&id != dependent)
    {
        if let Some(dep_info) = dependencies.into_idx(&dependent).await? {
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
    Ok(BreakageRes(breakages))
}
