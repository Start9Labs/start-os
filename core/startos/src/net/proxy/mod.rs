use std::io::Cursor;
use std::net::IpAddr;

use clap::Parser;
use const_format::formatcp;
use id_pool::IdPool;
use imbl_value::InternedString;
use rpc_toolkit::{from_fn_async, Context, HandlerExt, ParentHandler};
use serde::{Deserialize, Serialize};
use tokio::process::Command;
use ts_rs::TS;

use crate::context::{CliContext, RpcContext};
use crate::prelude::*;
use crate::util::io::{write_file, TmpDir};
use crate::util::serde::HandlerExtSerde;
use crate::util::Invoke;

const WIREGUARD_INSTALL_URL: &str = "https://raw.githubusercontent.com/start9labs/wireguard-vps-proxy-setup/master/wireguard-install.sh";

pub fn proxy_api<C: Context>() -> ParentHandler<C> {
    ParentHandler::new().subcommand(
        "create",
        from_fn_async(create_proxy)
            .no_display()
            .with_about("Create a new proxy")
            .with_call_remote::<CliContext>(),
    )
    // .subcommand(
    //     "edit",
    //     from_fn_async(edit_proxy)
    //         .no_display()
    //         .with_about("Change configuration of a proxy")
    //         .with_call_remote::<CliContext>(),
    // )
    // .subcommand(
    //     "list",
    //     from_fn_async(list_proxies)
    //         .with_display_serializable()
    //         .with_about("List proxies")
    //         .with_call_remote::<CliContext>(),
    // )
    // .subcommand(
    //     "remove",
    //     from_fn_async(remove_proxy)
    //         .no_display()
    //         .with_about("Remove an existing proxy")
    //         .with_call_remote::<CliContext>(),
    // )
    // .subcommand(
    //     "add-client",
    //     from_fn_async(add_client)
    //         .no_display()
    //         .with_about("Add VPN client")
    //         .with_call_remote::<CliContext>(),
    // )
    // .subcommand(
    //     "remove-client",
    //     from_fn_async(remove_client)
    //         .no_display()
    //         .with_about("Remove VPN client")
    //         .with_call_remote::<CliContext>(),
    // )
    // .subcommand(
    //     "list-clients",
    //     from_fn_async(list_clients)
    //         .no_display()
    //         .with_about("List VPN clients")
    //         .with_call_remote::<CliContext>(),
    // )
    // .subcommand(
    //     "set-outbound",
    //     from_fn_async(set_outbound)
    //         .no_display()
    //         .with_about("Set proxy to use for outbound connections")
    //         .with_call_remote::<CliContext>(),
    // )
    // .subcommand(
    //     "unset-outbound",
    //     from_fn_async(unset_outbound)
    //         .no_display()
    //         .with_about("Disable usage of a proxy for outbound connections")
    //         .with_call_remote::<CliContext>(),
    // )
}

#[derive(Debug, Deserialize, Serialize, Parser, TS)]
#[serde(rename_all = "camelCase")]
#[command(rename_all = "kebab-case")]
pub struct ProxyInfo {
    #[arg(long, short)]
    pub ip: IpAddr,
    #[arg(long, short)]
    pub public: bool,
}

pub async fn create_proxy(
    ctx: RpcContext,
    proxy @ ProxyInfo { ip, public }: ProxyInfo,
) -> Result<(), Error> {
    // TODO: install bash
    let id = ctx
        .db
        .peek()
        .await
        .as_public()
        .as_server_info()
        .as_network()
        .as_proxy()
        .keys()?
        .last()
        .map_or(Some(0), |k| k.checked_add(1))
        .ok_or_else(|| {
            Error::new(
                eyre!("proxy id cannot be greater than 255"),
                ErrorKind::Unknown, // TODO
            )
        })?;
    let conf = Command::new("ssh")
        .arg(format!("root@{ip}"))
        .arg("-oStrictHostKeyChecking=accept-new")
        .arg(format!("IP={ip}"))
        .arg(format!("WG_SUBNET=10.59.{id}.0"))
        .arg("bash")
        .input(Some(&mut Cursor::new(include_bytes!("./install-wg.sh"))))
        .invoke(ErrorKind::Unknown) // TODO
        .await?;
    ctx.db
        .mutate(|db| {
            db.as_public_mut()
                .as_server_info_mut()
                .as_network_mut()
                .as_proxy_mut()
                .insert(&id, &proxy)
        })
        .await
        .result?;
    let dir = TmpDir::new().await?;
    let iface = InternedString::from_display(&lazy_format!("wg-{id}"));
    let conf_path = dir.join(format!("{iface}.conf"));
    write_file(&conf_path, &conf).await?;
    Command::new("nmcli")
        .arg("connection")
        .arg("import")
        .arg("type")
        .arg("wireguard")
        .arg("file")
        .arg(conf_path)
        .invoke(ErrorKind::Network)
        .await?;
    dir.delete().await?;

    ctx.net_controller
        .net_iface
        .subscribe()
        .wait_for(|ifaces| ifaces.contains_key(&iface))
        .await;

    ctx.net_controller
        .net_iface
        .set_inbound(&iface, Some(public))
        .await?;

    Ok(())
}
