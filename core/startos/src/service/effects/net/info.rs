use std::net::Ipv4Addr;

use models::PackageId;

use crate::service::effects::callbacks::CallbackHandler;
use crate::service::effects::prelude::*;
use crate::service::rpc::CallbackId;
use crate::HOST_IP;

#[derive(Debug, Clone, serde::Serialize, serde::Deserialize, TS)]
#[serde(rename_all = "camelCase")]
#[ts(export)]
pub struct GetContainerIpParams {
    #[ts(optional)]
    package_id: Option<PackageId>,
    #[ts(optional)]
    callback: Option<CallbackId>,
}

pub async fn get_container_ip(
    context: EffectContext,
    GetContainerIpParams {
        package_id,
        callback,
    }: GetContainerIpParams,
) -> Result<Option<Ipv4Addr>, Error> {
    let context = context.deref()?;

    if let Some(package_id) = package_id.filter(|id| id != &context.seed.id) {
        if let Some(callback) = callback {
            // ip is static for the lifetime of the container, so callback unnecessary for self
            let callback = callback.register(&context.seed.persistent_container);
            context
                .seed
                .ctx
                .callbacks
                .add_get_container_ip(package_id.clone(), CallbackHandler::new(&context, callback));
        }
        let Some(svc) = &*context.seed.ctx.services.get(&package_id).await else {
            return Ok(None);
        };

        let Some(lxc) = svc.seed.persistent_container.lxc_container.get() else {
            return Ok(None);
        };
        let res = lxc.ip().await?;

        Ok(Some(res))
    } else {
        let Some(lxc) = context.seed.persistent_container.lxc_container.get() else {
            return Ok(None);
        };
        lxc.ip().await.map(Some)
    }
}
