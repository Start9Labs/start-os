use models::{HostId, PackageId};

use crate::net::host::Host;
use crate::service::effects::callbacks::CallbackHandler;
use crate::service::effects::prelude::*;
use crate::service::rpc::CallbackId;

#[derive(Debug, Clone, serde::Serialize, serde::Deserialize, TS)]
#[serde(rename_all = "camelCase")]
#[ts(export)]
pub struct GetHostInfoParams {
    host_id: HostId,
    #[ts(optional)]
    package_id: Option<PackageId>,
    #[ts(optional)]
    callback: Option<CallbackId>,
}
pub async fn get_host_info(
    context: EffectContext,
    GetHostInfoParams {
        host_id,
        package_id,
        callback,
    }: GetHostInfoParams,
) -> Result<Option<Host>, Error> {
    let context = context.deref()?;
    let db = context.seed.ctx.db.peek().await;
    let package_id = package_id.unwrap_or_else(|| context.seed.id.clone());

    let res = db
        .as_public()
        .as_package_data()
        .as_idx(&package_id)
        .and_then(|m| m.as_hosts().as_idx(&host_id))
        .map(|m| m.de())
        .transpose()?;

    if let Some(callback) = callback {
        let callback = callback.register(&context.seed.persistent_container);
        context.seed.ctx.callbacks.add_get_host_info(
            package_id,
            host_id,
            CallbackHandler::new(&context, callback),
        );
    }

    Ok(res)
}
