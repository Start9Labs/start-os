use std::net::{IpAddr, Ipv4Addr, SocketAddr};

use clap::Parser;
use imbl_value::InternedString;
use ipnet::Ipv4Net;
use rpc_toolkit::{from_fn_async, Context, Empty, HandlerArgs, HandlerExt, ParentHandler};
use serde::{Deserialize, Serialize};

use crate::context::CliContext;
use crate::prelude::*;
use crate::tunnel::context::TunnelContext;
use crate::tunnel::wg::{ClientConfig, WgConfig, WgSubnetClients, WgSubnetConfig};
use crate::util::serde::{display_serializable, HandlerExtSerde};

pub fn tunnel_api<C: Context>() -> ParentHandler<C> {
    ParentHandler::new()
        .subcommand(
            "db",
            super::db::db_api::<C>()
                .with_about("Commands to interact with the db i.e. dump and apply"),
        )
        .subcommand(
            "auth",
            super::auth::auth_api::<C>().with_about("Add or remove authorized clients"),
        )
        .subcommand(
            "subnet",
            subnet_api::<C>().with_about("Add, remove, or modify subnets"),
        )
    // .subcommand(
    //     "port-forward",
    //     ParentHandler::<C>::new()
    //         .subcommand(
    //             "add",
    //             from_fn_async(add_forward)
    //                 .with_metadata("sync_db", Value::Bool(true))
    //                 .no_display()
    //                 .with_about("Add a new port forward")
    //                 .with_call_remote::<CliContext>(),
    //         )
    //         .subcommand(
    //             "remove",
    //             from_fn_async(remove_forward)
    //                 .with_metadata("sync_db", Value::Bool(true))
    //                 .no_display()
    //                 .with_about("Remove a port forward")
    //                 .with_call_remote::<CliContext>(),
    //         ),
    // )
}

#[derive(Deserialize, Serialize, Parser)]
#[serde(rename_all = "camelCase")]
pub struct SubnetParams {
    subnet: Ipv4Net,
}

pub fn subnet_api<C: Context>() -> ParentHandler<C, SubnetParams> {
    ParentHandler::new()
        .subcommand(
            "add",
            from_fn_async(add_subnet)
                .with_metadata("sync_db", Value::Bool(true))
                .with_inherited(|a, _| a)
                .no_display()
                .with_about("Add a new subnet")
                .with_call_remote::<CliContext>(),
        )
        .subcommand(
            "remove",
            from_fn_async(remove_subnet)
                .with_metadata("sync_db", Value::Bool(true))
                .with_inherited(|a, _| a)
                .no_display()
                .with_about("Remove a subnet")
                .with_call_remote::<CliContext>(),
        )
        .subcommand(
            "add-device",
            from_fn_async(add_device)
                .with_metadata("sync_db", Value::Bool(true))
                .with_inherited(|a, _| a)
                .no_display()
                .with_about("Add a device to a subnet")
                .with_call_remote::<CliContext>(),
        )
        .subcommand(
            "remove-device",
            from_fn_async(remove_device)
                .with_metadata("sync_db", Value::Bool(true))
                .with_inherited(|a, _| a)
                .no_display()
                .with_about("Remove a device from a subnet")
                .with_call_remote::<CliContext>(),
        )
        .subcommand(
            "list-devices",
            from_fn_async(list_devices)
                .with_inherited(|a, _| a)
                .with_display_serializable()
                .with_custom_display_fn(|HandlerArgs { params, .. }, res| {
                    use prettytable::*;

                    if let Some(format) = params.format {
                        return display_serializable(format, res);
                    }

                    let mut table = Table::new();
                    table.add_row(row![bc => "NAME", "IP", "PUBLIC KEY"]);
                    for (ip, config) in res.clients.0 {
                        table.add_row(row![config.name, ip, config.key.verifying_key()]);
                    }

                    table.print_tty(false)?;

                    Ok(())
                })
                .with_about("List devices in a subnet")
                .with_call_remote::<CliContext>(),
        )
        .subcommand(
            "show-config",
            from_fn_async(show_config)
                .with_inherited(|a, _| a)
                .with_display_serializable()
                .with_custom_display_fn(|HandlerArgs { params, .. }, res| {
                    if let Some(format) = params.format {
                        return display_serializable(format, res);
                    }

                    println!("{}", res);

                    Ok(())
                })
                .with_about("Show the WireGuard configuration for a subnet")
                .with_call_remote::<CliContext>(),
        )
}

#[derive(Deserialize, Serialize, Parser)]
#[serde(rename_all = "camelCase")]
pub struct AddSubnetParams {
    name: InternedString,
}

pub async fn add_subnet(
    ctx: TunnelContext,
    AddSubnetParams { name }: AddSubnetParams,
    SubnetParams { subnet }: SubnetParams,
) -> Result<(), Error> {
    if subnet.addr().octets()[3] == 0
        || subnet.addr().octets()[3] == 255
        || subnet.prefix_len() > 24
    {
        return Err(Error::new(
            eyre!("invalid subnet"),
            ErrorKind::InvalidRequest,
        ));
    }
    let server = ctx
        .db
        .mutate(|db| {
            let map = db.as_wg_mut().as_subnets_mut();
            if !map.contains_key(&subnet)? {
                map.insert(&subnet, &WgSubnetConfig::new(name))?;
            }
            db.as_wg().de()
        })
        .await
        .result?;
    server.sync().await
}

pub async fn remove_subnet(
    ctx: TunnelContext,
    _: Empty,
    SubnetParams { subnet }: SubnetParams,
) -> Result<(), Error> {
    let server = ctx
        .db
        .mutate(|db| {
            db.as_wg_mut().as_subnets_mut().remove(&subnet)?;
            db.as_wg().de()
        })
        .await
        .result?;
    server.sync().await
}

#[derive(Deserialize, Serialize, Parser)]
#[serde(rename_all = "camelCase")]
pub struct AddDeviceParams {
    name: InternedString,
    ip: Option<Ipv4Addr>,
}

pub async fn add_device(
    ctx: TunnelContext,
    AddDeviceParams { name, ip }: AddDeviceParams,
    SubnetParams { subnet }: SubnetParams,
) -> Result<(), Error> {
    let config = WgConfig::generate(name);
    let server = ctx
        .db
        .mutate(|db| {
            db.as_wg_mut()
                .as_subnets_mut()
                .as_idx_mut(&subnet)
                .or_not_found(&subnet)?
                .as_clients_mut()
                .mutate(|WgSubnetClients(clients)| {
                    let ip = if let Some(ip) = ip {
                        ip
                    } else {
                        subnet
                            .hosts()
                            .find(|ip| !clients.contains_key(ip) && *ip != subnet.addr())
                            .ok_or_else(|| {
                                Error::new(
                                    eyre!("no available ips in subnet"),
                                    ErrorKind::InvalidRequest,
                                )
                            })?
                    };

                    if ip.octets()[3] == 0 || ip.octets()[3] == 255 {
                        return Err(Error::new(eyre!("invalid ip"), ErrorKind::InvalidRequest));
                    }
                    if !subnet.contains(&ip) {
                        return Err(Error::new(
                            eyre!("ip not in subnet"),
                            ErrorKind::InvalidRequest,
                        ));
                    }
                    clients.insert(ip, config).map_or(Ok(()), |_| {
                        Err(Error::new(
                            eyre!("ip already in use"),
                            ErrorKind::InvalidRequest,
                        ))
                    })
                })?;
            db.as_wg().de()
        })
        .await
        .result?;
    server.sync().await
}

#[derive(Deserialize, Serialize, Parser)]
#[serde(rename_all = "camelCase")]
pub struct RemoveDeviceParams {
    device: Ipv4Addr,
}

pub async fn remove_device(
    ctx: TunnelContext,
    RemoveDeviceParams { device }: RemoveDeviceParams,
    SubnetParams { subnet }: SubnetParams,
) -> Result<(), Error> {
    let server = ctx
        .db
        .mutate(|db| {
            db.as_wg_mut()
                .as_subnets_mut()
                .as_idx_mut(&subnet)
                .or_not_found(&subnet)?
                .as_clients_mut()
                .remove(&device)?
                .or_not_found(&device)?;
            db.as_wg().de()
        })
        .await
        .result?;
    server.sync().await
}

pub async fn list_devices(
    ctx: TunnelContext,
    _: Empty,
    SubnetParams { subnet }: SubnetParams,
) -> Result<WgSubnetConfig, Error> {
    ctx.db
        .peek()
        .await
        .as_wg()
        .as_subnets()
        .as_idx(&subnet)
        .or_not_found(&subnet)?
        .de()
}

#[derive(Deserialize, Serialize, Parser)]
#[serde(rename_all = "camelCase")]
pub struct ShowConfigParams {
    device: Ipv4Addr,
    wan_addr: Option<IpAddr>,
    #[serde(rename = "__ConnectInfo_local_addr")]
    #[arg(skip)]
    local_addr: Option<SocketAddr>,
}

pub async fn show_config(
    ctx: TunnelContext,
    ShowConfigParams {
        device,
        wan_addr,
        local_addr,
    }: ShowConfigParams,
    SubnetParams { subnet }: SubnetParams,
) -> Result<ClientConfig, Error> {
    let wg = ctx.db.peek().await.into_wg();
    let client = wg
        .as_subnets()
        .as_idx(&subnet)
        .or_not_found(&subnet)?
        .as_clients()
        .as_idx(&device)
        .or_not_found(&device)?
        .de()?;
    let wan_addr = if let Some(wan_addr) = wan_addr.or(local_addr.map(|a| a.ip())).filter(|ip| {
        !ip.is_loopback()
            && !match ip {
                IpAddr::V4(ipv4) => ipv4.is_private() || ipv4.is_link_local(),
                IpAddr::V6(ipv6) => ipv6.is_unique_local() || ipv6.is_unicast_link_local(),
            }
    }) {
        wan_addr
    } else {
        ctx.net_iface
            .ip_info()
            .into_iter()
            .find_map(|(_, info)| {
                info.public()
                    .then_some(info.ip_info)
                    .flatten()
                    .into_iter()
                    .find_map(|info| info.subnets.into_iter().next())
            })
            .or_not_found("a public IP address")?
            .addr()
    };
    Ok(client.client_config(
        device,
        wg.as_key().de()?.verifying_key(),
        (wan_addr, wg.as_port().de()?).into(),
    ))
}
