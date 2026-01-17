use clap::Parser;
use imbl_value::InternedString;
use patch_db::json_ptr::JsonPointer;
use rpc_toolkit::{Context, HandlerExt, ParentHandler, from_fn_async};
use serde::{Deserialize, Serialize};
use tokio::process::Command;
use ts_rs::TS;

use crate::GatewayId;
use crate::context::{CliContext, RpcContext};
use crate::db::model::public::{NetworkInterfaceInfo, NetworkInterfaceType};
use crate::net::host::all_hosts;
use crate::prelude::*;
use crate::util::Invoke;
use crate::util::io::{TmpDir, write_file_atomic};

pub fn tunnel_api<C: Context>() -> ParentHandler<C> {
    ParentHandler::new()
        .subcommand(
            "add",
            from_fn_async(add_tunnel)
                .with_about("about.add-new-tunnel")
                .with_call_remote::<CliContext>(),
        )
        .subcommand(
            "remove",
            from_fn_async(remove_tunnel)
                .no_display()
                .with_about("about.remove-tunnel")
                .with_call_remote::<CliContext>(),
        )
}

#[derive(Debug, Clone, Deserialize, Serialize, Parser, TS)]
#[ts(export)]
pub struct AddTunnelParams {
    name: InternedString,
    config: String,
    public: bool,
}

fn sanitize_config(config: &str) -> String {
    let mut res = String::with_capacity(config.len());
    for line in config.lines() {
        if line
            .trim()
            .strip_prefix("AllowedIPs")
            .map_or(false, |l| l.trim().starts_with("="))
        {
            res.push_str("AllowedIPs = 0.0.0.0/0, ::/0");
        } else {
            res.push_str(line);
        }
        res.push('\n');
    }
    res
}

pub async fn add_tunnel(
    ctx: RpcContext,
    AddTunnelParams {
        name,
        config,
        public,
    }: AddTunnelParams,
) -> Result<GatewayId, Error> {
    let ifaces = ctx.net_controller.net_iface.watcher.subscribe();
    let mut iface = GatewayId::from(InternedString::intern("wg0"));
    if !ifaces.send_if_modified(|i| {
        for id in 1..256 {
            if !i.contains_key(&iface) {
                i.insert(
                    iface.clone(),
                    NetworkInterfaceInfo {
                        name: Some(name),
                        public: Some(public),
                        secure: None,
                        ip_info: None,
                    },
                );
                return true;
            }
            iface = InternedString::from_display(&lazy_format!("wg{id}")).into();
        }
        false
    }) {
        return Err(Error::new(
            eyre!("too many wireguard interfaces"),
            ErrorKind::InvalidRequest,
        ));
    }

    let mut sub = ctx
        .db
        .subscribe(
            "/public/serverInfo/network/gateways"
                .parse::<JsonPointer>()
                .with_kind(ErrorKind::Database)?
                .join_end(iface.as_str())
                .join_end("ipInfo"),
        )
        .await;

    let tmpdir = TmpDir::new().await?;
    let conf = tmpdir.join(&iface).with_extension("conf");
    write_file_atomic(&conf, &sanitize_config(&config)).await?;
    Command::new("nmcli")
        .arg("connection")
        .arg("import")
        .arg("type")
        .arg("wireguard")
        .arg("file")
        .arg(&conf)
        .invoke(ErrorKind::Network)
        .await?;
    tmpdir.delete().await?;

    sub.recv().await;

    Ok(iface)
}

#[derive(Debug, Clone, Deserialize, Serialize, Parser, TS)]
#[ts(export)]
pub struct RemoveTunnelParams {
    id: GatewayId,
}
pub async fn remove_tunnel(
    ctx: RpcContext,
    RemoveTunnelParams { id }: RemoveTunnelParams,
) -> Result<(), Error> {
    let Some(existing) = ctx
        .db
        .peek()
        .await
        .into_public()
        .into_server_info()
        .into_network()
        .into_gateways()
        .into_idx(&id)
        .and_then(|e| e.into_ip_info().transpose())
    else {
        return Ok(());
    };

    if existing.as_deref().as_device_type().de()? != Some(NetworkInterfaceType::Wireguard) {
        return Err(Error::new(
            eyre!("network interface {id} is not a proxy"),
            ErrorKind::InvalidRequest,
        ));
    }

    ctx.db
        .mutate(|db| {
            for host in all_hosts(db) {
                let host = host?;
                host.as_public_domains_mut()
                    .mutate(|p| Ok(p.retain(|_, v| v.gateway != id)))?;
            }

            Ok(())
        })
        .await
        .result?;

    ctx.net_controller.net_iface.delete_iface(&id).await?;

    ctx.db
        .mutate(|db| {
            for host in all_hosts(db) {
                let host = host?;
                host.as_bindings_mut().mutate(|b| {
                    Ok(b.values_mut().for_each(|v| {
                        v.net.private_disabled.remove(&id);
                        v.net.public_enabled.remove(&id);
                    }))
                })?;
            }

            Ok(())
        })
        .await
        .result?;

    Ok(())
}
