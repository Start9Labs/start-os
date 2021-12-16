use std::collections::BTreeMap;

use patch_db::{DbHandle, LockType};
use rpc_toolkit::command;

use crate::context::RpcContext;
use crate::dependencies::{break_transitive, BreakageRes, DependencyError};
use crate::s9pk::manifest::PackageId;
use crate::util::serde::display_serializable;
use crate::util::Version;
use crate::Error;

#[command(subcommands(dry))]
pub async fn update() -> Result<(), Error> {
    Ok(())
}

#[command(display(display_serializable))]
pub async fn dry(
    #[context] ctx: RpcContext,
    #[arg] id: PackageId,
    #[arg] version: Version,
) -> Result<BreakageRes, Error> {
    let mut db = ctx.db.handle();
    let mut tx = db.begin().await?;
    let mut breakages = BTreeMap::new();
    crate::db::DatabaseModel::new()
        .package_data()
        .lock(&mut tx, LockType::Read)
        .await?;
    for dependent in crate::db::DatabaseModel::new()
        .package_data()
        .idx_model(&id)
        .and_then(|m| m.installed())
        .expect(&mut tx)
        .await?
        .current_dependents()
        .keys(&mut tx, true)
        .await?
        .into_iter()
        .filter(|dependent| &id != dependent)
    {
        let version_req = crate::db::DatabaseModel::new()
            .package_data()
            .idx_model(&dependent)
            .and_then(|m| m.installed())
            .expect(&mut tx)
            .await?
            .manifest()
            .dependencies()
            .idx_model(&id)
            .expect(&mut tx)
            .await?
            .get(&mut tx, true)
            .await?
            .into_owned()
            .version;
        if !version.satisfies(&version_req) {
            break_transitive(
                &mut tx,
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
    tx.abort().await?;
    Ok(BreakageRes(breakages))
}
