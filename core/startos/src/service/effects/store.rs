use imbl::vector;
use imbl_value::json;
use models::{PackageId, VersionString};
use patch_db::json_ptr::JsonPointer;

use crate::service::effects::callbacks::CallbackHandler;
use crate::service::effects::prelude::*;
use crate::service::rpc::CallbackId;

#[derive(Debug, Clone, Serialize, Deserialize, TS)]
#[serde(rename_all = "camelCase")]
#[ts(export)]
pub struct GetStoreParams {
    #[ts(optional)]
    package_id: Option<PackageId>,
    #[ts(type = "string")]
    path: JsonPointer,
    #[ts(optional)]
    callback: Option<CallbackId>,
}
pub async fn get_store(
    context: EffectContext,
    GetStoreParams {
        package_id,
        path,
        callback,
    }: GetStoreParams,
) -> Result<Value, Error> {
    dbg!(&callback);
    let context = context.deref()?;
    let peeked = context.seed.ctx.db.peek().await;
    let package_id = package_id.unwrap_or(context.seed.id.clone());
    let value = peeked
        .as_private()
        .as_package_stores()
        .as_idx(&package_id)
        .map(|s| s.de())
        .transpose()?
        .unwrap_or_default();

    if let Some(callback) = callback {
        let callback = callback.register(&context.seed.persistent_container);
        context.seed.ctx.callbacks.add_get_store(
            package_id,
            path.clone(),
            CallbackHandler::new(&context, callback),
        );
    }

    Ok(path.get(&value).cloned().unwrap_or_default())
}

#[derive(Debug, Clone, Serialize, Deserialize, TS)]
#[serde(rename_all = "camelCase")]
#[ts(export)]
pub struct SetStoreParams {
    #[ts(type = "any")]
    value: Value,
    #[ts(type = "string")]
    path: JsonPointer,
}
pub async fn set_store(
    context: EffectContext,
    SetStoreParams { value, path }: SetStoreParams,
) -> Result<(), Error> {
    let context = context.deref()?;
    let package_id = &context.seed.id;
    context
        .seed
        .ctx
        .db
        .mutate(|db| {
            let model = db
                .as_private_mut()
                .as_package_stores_mut()
                .upsert(package_id, || Ok(json!({})))?;
            let mut model_value = model.de()?;
            if model_value.is_null() {
                model_value = json!({});
            }
            path.set(&mut model_value, value, true)
                .with_kind(ErrorKind::ParseDbField)?;
            model.ser(&model_value)
        })
        .await?;

    if let Some(callbacks) = context.seed.ctx.callbacks.get_store(package_id, &path) {
        callbacks.call(vector![]).await?;
    }

    Ok(())
}

#[derive(Debug, Clone, Serialize, Deserialize, TS, Parser)]
#[serde(rename_all = "camelCase")]
#[ts(export)]
pub struct SetDataVersionParams {
    #[ts(type = "string")]
    version: VersionString,
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
                .ser(&Some(version))
        })
        .await?;

    Ok(())
}

pub async fn get_data_version(context: EffectContext) -> Result<Option<VersionString>, Error> {
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
