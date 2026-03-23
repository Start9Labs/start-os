use std::time::Duration;

use clap::Parser;
use imbl_value::InternedString;
use patch_db::json_ptr::JsonPointer;
use rpc_toolkit::{Context, HandlerExt, ParentHandler, from_fn_async};
use serde::{Deserialize, Serialize};
use tokio::process::Command;
use ts_rs::TS;

use crate::GatewayId;
use crate::context::{CliContext, RpcContext};
use crate::db::model::public::{
    GatewayType, NetworkInfo, NetworkInterfaceInfo, NetworkInterfaceType,
};
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
#[group(skip)]
#[serde(rename_all = "camelCase")]
#[ts(export)]
pub struct AddTunnelParams {
    #[arg(help = "help.arg.tunnel-name")]
    name: InternedString,
    #[arg(help = "help.arg.wireguard-config")]
    config: String,
    #[arg(help = "help.arg.gateway-type")]
    #[serde(default, rename = "type")]
    gateway_type: Option<GatewayType>,
    #[arg(long, help = "help.arg.set-as-default-outbound")]
    set_as_default_outbound: bool,
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
        gateway_type,
        set_as_default_outbound,
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
                        secure: None,
                        ip_info: None,
                        gateway_type,
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

    if set_as_default_outbound {
        ctx.db
            .mutate(|db| {
                db.as_public_mut()
                    .as_server_info_mut()
                    .as_network_mut()
                    .as_default_outbound_mut()
                    .ser(&Some(iface.clone()))
            })
            .await
            .result?;
    }

    // Wait for the sync loop to fully commit gateway state (addresses, hosts)
    // to the database, with a 15-second timeout.
    if tokio::time::timeout(Duration::from_secs(15), async {
        let mut watch = ctx
            .db
            .watch("/public/serverInfo/network".parse::<JsonPointer>().unwrap())
            .await
            .typed::<NetworkInfo>();
        loop {
            if watch
                .peek()?
                .as_gateways()
                .as_idx(&iface)
                .and_then(|g| g.as_ip_info().transpose_ref())
                .is_some()
            {
                break;
            }
            watch.changed().await?;
        }
        Ok::<_, Error>(())
    })
    .await
    .is_err()
    {
        tracing::warn!(
            "{}",
            t!(
                "net.tunnel.timeout-waiting-for-add",
                gateway = iface.as_str()
            )
        );
    }

    Ok(iface)
}

#[derive(Debug, Clone, Deserialize, Serialize, Parser, TS)]
#[group(skip)]
#[ts(export)]
pub struct RemoveTunnelParams {
    #[arg(help = "help.arg.gateway-id")]
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
            let hostname = crate::hostname::ServerHostname::load(db.as_public().as_server_info())?;
            let gateways = db
                .as_public()
                .as_server_info()
                .as_network()
                .as_gateways()
                .de()?;
            let ports = db.as_private().as_available_ports().de()?;
            for host in all_hosts(db) {
                let host = host?;
                host.as_public_domains_mut()
                    .mutate(|p| Ok(p.retain(|_, v| v.gateway != id)))?;
                host.update_addresses(&hostname, &gateways, &ports)?;
            }

            Ok(())
        })
        .await
        .result?;

    ctx.net_controller.net_iface.delete_iface(&id).await?;

    ctx.db
        .mutate(|db| {
            let hostname = crate::hostname::ServerHostname::load(db.as_public().as_server_info())?;
            let gateways = db
                .as_public()
                .as_server_info()
                .as_network()
                .as_gateways()
                .de()?;
            let ports = db.as_private().as_available_ports().de()?;
            for host in all_hosts(db) {
                let host = host?;
                host.as_private_domains_mut().mutate(|d| {
                    for gateways in d.values_mut() {
                        gateways.remove(&id);
                    }
                    d.retain(|_, gateways| !gateways.is_empty());
                    Ok(())
                })?;
                host.update_addresses(&hostname, &gateways, &ports)?;
            }

            Ok(())
        })
        .await
        .result?;

    // Wait for the sync loop to fully commit gateway removal to the database,
    // with a 15-second timeout.
    if tokio::time::timeout(Duration::from_secs(15), async {
        let mut watch = ctx
            .db
            .watch("/public/serverInfo/network".parse::<JsonPointer>().unwrap())
            .await
            .typed::<NetworkInfo>();
        loop {
            if watch.peek()?.as_gateways().as_idx(&id).is_none() {
                break;
            }
            watch.changed().await?;
        }
        Ok::<_, Error>(())
    })
    .await
    .is_err()
    {
        tracing::warn!(
            "{}",
            t!(
                "net.tunnel.timeout-waiting-for-remove",
                gateway = id.as_str()
            )
        );
    }

    Ok(())
}
