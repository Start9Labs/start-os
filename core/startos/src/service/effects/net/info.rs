use std::net::Ipv4Addr;

use crate::service::effects::prelude::*;

pub async fn get_container_ip(context: EffectContext) -> Result<Ipv4Addr, Error> {
    let context = context.deref()?;
    Ok(context.seed.persistent_container.net_service.get_ip().await)
}
