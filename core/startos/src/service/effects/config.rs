use models::PackageId;

use crate::service::effects::prelude::*;

#[derive(Debug, Clone, Serialize, Deserialize, Parser, TS)]
#[serde(rename_all = "camelCase")]
#[ts(export)]
pub struct GetConfiguredParams {
    #[ts(optional)]
    package_id: Option<PackageId>,
}
pub async fn get_configured(context: EffectContext) -> Result<bool, Error> {
    let context = context.deref()?;
    let peeked = context.seed.ctx.db.peek().await;
    let package_id = &context.seed.id;
    peeked
        .as_public()
        .as_package_data()
        .as_idx(package_id)
        .or_not_found(package_id)?
        .as_status()
        .as_configured()
        .de()
}

#[derive(Debug, Clone, Serialize, Deserialize, Parser, TS)]
#[serde(rename_all = "camelCase")]
#[ts(export)]
pub struct SetConfigured {
    configured: bool,
}
pub async fn set_configured(
    context: EffectContext,
    SetConfigured { configured }: SetConfigured,
) -> Result<(), Error> {
    let context = context.deref()?;
    let package_id = &context.seed.id;
    context
        .seed
        .ctx
        .db
        .mutate(|db| {
            db.as_public_mut()
                .as_package_data_mut()
                .as_idx_mut(package_id)
                .or_not_found(package_id)?
                .as_status_mut()
                .as_configured_mut()
                .ser(&configured)
        })
        .await?;
    Ok(())
}
