use std::collections::{BTreeMap, BTreeSet};
use std::net::IpAddr;

use clap::Parser;
use futures::TryStreamExt;
use rpc_toolkit::{from_fn_async, HandlerExt, ParentHandler};
use serde::{Deserialize, Serialize};
use tokio::sync::RwLock;
use ts_rs::TS;

use crate::context::{CliContext, RpcContext};
use crate::db::model::public::IpInfo;
use crate::net::utils::{iface_is_physical, list_interfaces};
use crate::prelude::*;
use crate::Error;

lazy_static::lazy_static! {
    static ref CACHED_IPS: RwLock<BTreeSet<IpAddr>> = RwLock::new(BTreeSet::new());
}

async fn _ips() -> Result<BTreeSet<IpAddr>, Error> {
    Ok(init_ips()
        .await?
        .values()
        .flat_map(|i| {
            std::iter::empty()
                .chain(i.ipv4.map(IpAddr::from))
                .chain(i.ipv6.map(IpAddr::from))
        })
        .collect())
}

pub async fn ips() -> Result<BTreeSet<IpAddr>, Error> {
    let ips = CACHED_IPS.read().await.clone();
    if !ips.is_empty() {
        return Ok(ips);
    }
    let ips = _ips().await?;
    *CACHED_IPS.write().await = ips.clone();
    Ok(ips)
}

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

// #[command(subcommands(update))]
pub fn dhcp() -> ParentHandler {
    ParentHandler::new().subcommand(
        "update",
        from_fn_async::<_, _, (), Error, (RpcContext, UpdateParams)>(update)
            .no_display()
            .with_call_remote::<CliContext>(),
    )
}
#[derive(Deserialize, Serialize, Parser, TS)]
#[serde(rename_all = "camelCase")]
#[command(rename_all = "kebab-case")]
pub struct UpdateParams {
    interface: String,
}

pub async fn update(
    ctx: RpcContext,
    UpdateParams { interface }: UpdateParams,
) -> Result<(), Error> {
    if iface_is_physical(&interface).await {
        let ip_info = IpInfo::for_interface(&interface).await?;
        ctx.db
            .mutate(|db| {
                db.as_public_mut()
                    .as_server_info_mut()
                    .as_ip_info_mut()
                    .insert(&interface, &ip_info)
            })
            .await?;

        let mut cached = CACHED_IPS.write().await;
        if cached.is_empty() {
            *cached = _ips().await?;
        } else {
            cached.extend(
                std::iter::empty()
                    .chain(ip_info.ipv4.map(IpAddr::from))
                    .chain(ip_info.ipv6.map(IpAddr::from)),
            );
        }
    }
    Ok(())
}
