use std::collections::BTreeMap;

use patch_db::{DbHandle, LockReceipt, LockTargetId, LockType, Verifier};
use rpc_toolkit::command;
use tracing::instrument;

use crate::config::not_found;
use crate::context::RpcContext;
use crate::db::model::CurrentDependents;
use crate::dependencies::{
    break_transitive, BreakTransitiveReceipts, BreakageRes, DependencyError,
};
use crate::s9pk::manifest::PackageId;
use crate::util::serde::display_serializable;
use crate::util::Version;
use crate::Error;

pub struct UpdateReceipts {
    break_receipts: BreakTransitiveReceipts,
    current_dependents: LockReceipt<CurrentDependents, String>,
    dependency: LockReceipt<crate::dependencies::DepInfo, (String, String)>,
}

impl UpdateReceipts {
    pub async fn new<'a>(db: &'a mut impl DbHandle) -> Result<Self, Error> {
        let mut locks = Vec::new();

        let setup = Self::setup(&mut locks);
        Ok(setup(&db.lock_all(locks).await?)?)
    }

    pub fn setup(locks: &mut Vec<LockTargetId>) -> impl FnOnce(&Verifier) -> Result<Self, Error> {
        let break_receipts = BreakTransitiveReceipts::setup(locks);
        let current_dependents = crate::db::DatabaseModel::new()
            .package_data()
            .star()
            .installed()
            .map(|x| x.current_dependents())
            .make_locker(LockType::Write)
            .add_to_keys(locks);
        let dependency = crate::db::DatabaseModel::new()
            .package_data()
            .star()
            .installed()
            .map(|x| x.manifest().dependencies().star())
            .make_locker(LockType::Write)
            .add_to_keys(locks);
        move |skeleton_key| {
            Ok(Self {
                break_receipts: break_receipts(skeleton_key)?,
                current_dependents: current_dependents.verify(skeleton_key)?,
                dependency: dependency.verify(skeleton_key)?,
            })
        }
    }
}

#[command(subcommands(dry))]
pub async fn update() -> Result<(), Error> {
    Ok(())
}

#[instrument(skip(ctx))]
#[command(display(display_serializable))]
pub async fn dry(
    #[context] ctx: RpcContext,
    #[arg] id: PackageId,
    #[arg] version: Version,
) -> Result<BreakageRes, Error> {
    let mut db = ctx.db.handle();
    let mut tx = db.begin().await?;
    let mut breakages = BTreeMap::new();
    let receipts = UpdateReceipts::new(&mut tx).await?;

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
                    &mut tx,
                    &dependent,
                    &id,
                    DependencyError::IncorrectVersion {
                        expected: version_req,
                        received: version.clone(),
                    },
                    &mut breakages,
                    &receipts.break_receipts,
                )
                .await?;
            }
        }
    }
    tx.abort().await?;
    Ok(BreakageRes(breakages))
}
