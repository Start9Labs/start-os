use std::collections::BTreeMap;

use models::{ActionId, PackageId};

use crate::action::ActionResult;
use crate::db::model::package::ActionMetadata;
use crate::rpc_continuations::Guid;
use crate::service::effects::prelude::*;

#[derive(Debug, Clone, Serialize, Deserialize, TS)]
#[ts(export)]
#[serde(rename_all = "camelCase")]
pub struct ExportActionParams {
    #[ts(optional)]
    package_id: Option<PackageId>,
    id: ActionId,
    metadata: ActionMetadata,
}
pub async fn export_action(context: EffectContext, data: ExportActionParams) -> Result<(), Error> {
    let context = context.deref()?;
    let package_id = context.seed.id.clone();
    context
        .seed
        .ctx
        .db
        .mutate(|db| {
            let model = db
                .as_public_mut()
                .as_package_data_mut()
                .as_idx_mut(&package_id)
                .or_not_found(&package_id)?
                .as_actions_mut();
            let mut value = model.de()?;
            value
                .insert(data.id, data.metadata)
                .map(|_| ())
                .unwrap_or_default();
            model.ser(&value)
        })
        .await?;
    Ok(())
}

pub async fn clear_actions(context: EffectContext) -> Result<(), Error> {
    let context = context.deref()?;
    let package_id = context.seed.id.clone();
    context
        .seed
        .ctx
        .db
        .mutate(|db| {
            db.as_public_mut()
                .as_package_data_mut()
                .as_idx_mut(&package_id)
                .or_not_found(&package_id)?
                .as_actions_mut()
                .ser(&BTreeMap::new())
        })
        .await?;
    Ok(())
}

#[derive(Debug, Clone, Serialize, Deserialize, TS)]
#[serde(rename_all = "camelCase")]
#[ts(export)]
pub struct ExecuteAction {
    #[serde(default)]
    #[ts(skip)]
    procedure_id: Guid,
    #[ts(optional)]
    package_id: Option<PackageId>,
    action_id: ActionId,
    #[ts(type = "any")]
    input: Value,
}
pub async fn execute_action(
    context: EffectContext,
    ExecuteAction {
        procedure_id,
        package_id,
        action_id,
        input,
    }: ExecuteAction,
) -> Result<ActionResult, Error> {
    let context = context.deref()?;

    if let Some(package_id) = package_id {
        context
            .seed
            .ctx
            .services
            .get(&package_id)
            .await
            .as_ref()
            .or_not_found(&package_id)?
            .action(procedure_id, action_id, input)
            .await
    } else {
        context.action(procedure_id, action_id, input).await
    }
}
