use std::path::Path;

use crate::service::effects::prelude::*;
use crate::util::io::{delete_file, maybe_read_file_to_string, write_file_atomic};
use crate::volume::PKG_VOLUME_DIR;
use crate::DATA_DIR;

#[derive(Debug, Clone, Serialize, Deserialize, TS, Parser)]
#[serde(rename_all = "camelCase")]
#[ts(export)]
pub struct SetDataVersionParams {
    #[ts(type = "string")]
    version: Option<String>,
}
#[instrument(skip(context))]
pub async fn set_data_version(
    context: EffectContext,
    SetDataVersionParams { version }: SetDataVersionParams,
) -> Result<(), Error> {
    let context = context.deref()?;
    let package_id = &context.seed.id;
    let path = Path::new(DATA_DIR)
        .join(PKG_VOLUME_DIR)
        .join(package_id)
        .join("data")
        .join(".version");
    if let Some(version) = version {
        write_file_atomic(path, version.as_bytes()).await?;
    } else {
        delete_file(path).await?;
    }

    Ok(())
}

#[instrument(skip_all)]
pub async fn get_data_version(context: EffectContext) -> Result<Option<String>, Error> {
    let context = context.deref()?;
    let package_id = &context.seed.id;
    let path = Path::new(DATA_DIR)
        .join(PKG_VOLUME_DIR)
        .join(package_id)
        .join("data")
        .join(".version");
    maybe_read_file_to_string(path).await
}
