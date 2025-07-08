use crate::service::effects::prelude::*;

#[derive(Debug, Clone, Serialize, Deserialize, TS, Parser)]
#[serde(rename_all = "camelCase")]
#[ts(export)]
pub struct SetDataVersionParams {
    #[ts(type = "string")]
    version: Option<String>,
}
pub async fn set_data_version(
    context: EffectContext,
    SetDataVersionParams { version }: SetDataVersionParams,
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
                .as_data_version_mut()
                .ser(&version)
        })
        .await
        .result?;

    Ok(())
}

pub async fn get_data_version(context: EffectContext) -> Result<Option<String>, Error> {
    let context = context.deref()?;
    let package_id = &context.seed.id;
    context
        .seed
        .ctx
        .db
        .peek()
        .await
        .as_public()
        .as_package_data()
        .as_idx(package_id)
        .or_not_found(package_id)?
        .as_data_version()
        .de()
}
