use std::collections::BTreeMap;

use futures::TryStreamExt;
use rpc_toolkit::command;

use crate::context::RpcContext;
use crate::db::model::IpInfo;
use crate::net::net_utils::{iface_is_physical, list_interfaces};
use crate::util::display_none;
use crate::Error;

pub async fn init_ips() -> Result<BTreeMap<String, IpInfo>, Error> {
    let mut res = BTreeMap::new();
    let mut ifaces = list_interfaces();
    while let Some(iface) = ifaces.try_next().await? {
        if iface_is_physical(&iface).await {
            let ip_info = IpInfo::for_interface(&iface).await?;
            res.insert(iface, ip_info);
        }
    }
    Ok(res)
}

#[command(subcommands(update))]
pub async fn dhcp() -> Result<(), Error> {
    Ok(())
}

#[command(display(display_none))]
pub async fn update(#[context] ctx: RpcContext, #[arg] interface: String) -> Result<(), Error> {
    if iface_is_physical(&interface).await {
        crate::db::DatabaseModel::new()
            .server_info()
            .ip_info()
            .idx_model(&interface)
            .put(
                &mut ctx.db.handle(),
                &IpInfo::for_interface(&interface).await?,
            )
            .await?;
    }
    Ok(())
}
