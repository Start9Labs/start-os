use std::net::{IpAddr, Ipv4Addr, SocketAddr, SocketAddrV4};

use clap::Parser;
use imbl_value::InternedString;
use ipnet::Ipv4Net;
use rpc_toolkit::{Context, Empty, HandlerArgs, HandlerExt, ParentHandler, from_fn_async};
use serde::{Deserialize, Serialize};
use ts_rs::TS;

use crate::context::CliContext;
use crate::db::model::public::NetworkInterfaceType;
use crate::net::forward::add_iptables_rule;
use crate::prelude::*;
use crate::tunnel::context::TunnelContext;
use crate::tunnel::db::PortForwardEntry;
use crate::tunnel::wg::{WIREGUARD_INTERFACE_NAME, WgConfig, WgSubnetClients, WgSubnetConfig};
use crate::util::serde::{HandlerExtSerde, display_serializable};

pub fn tunnel_api<C: Context>() -> ParentHandler<C> {
    ParentHandler::new()
        .subcommand("web", super::web::web_api::<C>())
        .subcommand(
            "db",
            super::db::db_api::<C>().with_about("about.commands-interact-with-db-dump-apply"),
        )
        .subcommand(
            "auth",
            super::auth::auth_api::<C>().with_about("about.add-or-remove-authorized-clients"),
        )
        .subcommand(
            "subnet",
            subnet_api::<C>().with_about("about.add-remove-or-modify-subnets"),
        )
        .subcommand(
            "device",
            device_api::<C>().with_about("about.add-remove-or-list-devices-in-subnets"),
        )
        .subcommand(
            "port-forward",
            ParentHandler::<C>::new()
                .subcommand(
                    "add",
                    from_fn_async(add_forward)
                        .with_metadata("sync_db", Value::Bool(true))
                        .no_display()
                        .with_about("about.add-new-port-forward")
                        .with_call_remote::<CliContext>(),
                )
                .subcommand(
                    "remove",
                    from_fn_async(remove_forward)
                        .with_metadata("sync_db", Value::Bool(true))
                        .no_display()
                        .with_about("about.remove-port-forward")
                        .with_call_remote::<CliContext>(),
                )
                .subcommand(
                    "update-label",
                    from_fn_async(update_forward_label)
                        .with_metadata("sync_db", Value::Bool(true))
                        .no_display()
                        .with_about("about.update-port-forward-label")
                        .with_call_remote::<CliContext>(),
                )
                .subcommand(
                    "set-enabled",
                    from_fn_async(set_forward_enabled)
                        .with_metadata("sync_db", Value::Bool(true))
                        .no_display()
                        .with_about("about.enable-or-disable-port-forward")
                        .with_call_remote::<CliContext>(),
                ),
        )
        .subcommand(
            "restart",
            from_fn_async(restart)
                .no_display()
                .with_about("about.restart-tunnel")
                .with_call_remote::<CliContext>(),
        )
        .subcommand(
            "update",
            ParentHandler::<C>::new()
                .subcommand(
                    "check",
                    from_fn_async(super::update::check_update)
                        .with_display_serializable()
                        .with_about("about.check-for-updates")
                        .with_call_remote::<CliContext>(),
                )
                .subcommand(
                    "apply",
                    from_fn_async(super::update::apply_update)
                        .with_display_serializable()
                        .with_about("about.apply-available-update")
                        .with_call_remote::<CliContext>(),
                ),
        )
}

pub async fn restart(ctx: TunnelContext) -> Result<(), Error> {
    ctx.shutdown.send(Some(true)).ok();
    Ok(())
}

#[derive(Deserialize, Serialize, Parser, TS)]
#[group(skip)]
#[serde(rename_all = "camelCase")]
pub struct SubnetParams {
    #[ts(type = "string")]
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
                .with_about("about.add-new-subnet")
                .with_call_remote::<CliContext>(),
        )
        .subcommand(
            "remove",
            from_fn_async(remove_subnet)
                .with_metadata("sync_db", Value::Bool(true))
                .with_inherited(|a, _| a)
                .no_display()
                .with_about("about.remove-subnet")
                .with_call_remote::<CliContext>(),
        )
}

pub fn device_api<C: Context>() -> ParentHandler<C> {
    ParentHandler::new()
        .subcommand(
            "add",
            from_fn_async(add_device)
                .with_metadata("sync_db", Value::Bool(true))
                .no_display()
                .with_about("about.add-device-to-subnet")
                .with_call_remote::<CliContext>(),
        )
        .subcommand(
            "remove",
            from_fn_async(remove_device)
                .with_metadata("sync_db", Value::Bool(true))
                .no_display()
                .with_about("about.remove-device-from-subnet")
                .with_call_remote::<CliContext>(),
        )
        .subcommand(
            "list",
            from_fn_async(list_devices)
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
                .with_about("about.list-devices-in-subnet")
                .with_call_remote::<CliContext>(),
        )
        .subcommand(
            "show-config",
            from_fn_async(show_config)
                .with_about("about.show-wireguard-configuration-for-device")
                .with_call_remote::<CliContext>(),
        )
}

#[derive(Deserialize, Serialize, Parser, TS)]
#[group(skip)]
#[serde(rename_all = "camelCase")]
pub struct AddSubnetParams {
    name: InternedString,
}

pub async fn add_subnet(
    ctx: TunnelContext,
    AddSubnetParams { name }: AddSubnetParams,
    SubnetParams { mut subnet }: SubnetParams,
) -> Result<(), Error> {
    if subnet.prefix_len() > 24 {
        return Err(Error::new(
            eyre!("invalid subnet"),
            ErrorKind::InvalidRequest,
        ));
    }
    let addr = subnet
        .hosts()
        .next()
        .ok_or_else(|| Error::new(eyre!("invalid subnet"), ErrorKind::InvalidRequest))?;
    subnet = Ipv4Net::new_assert(addr, subnet.prefix_len());
    let server = ctx
        .db
        .mutate(|db| {
            let map = db.as_wg_mut().as_subnets_mut();
            if let Some(s) = map
                .keys()?
                .into_iter()
                .find(|s| s != &subnet && (s.contains(&subnet) || subnet.contains(s)))
            {
                return Err(Error::new(
                    eyre!("{subnet} overlaps with existing subnet {s}"),
                    ErrorKind::InvalidRequest,
                ));
            }
            map.upsert(&subnet, || {
                Ok(WgSubnetConfig::new(InternedString::default()))
            })?
            .as_name_mut()
            .ser(&name)?;
            db.as_wg().de()
        })
        .await
        .result?;
    server.sync().await?;

    for iface in ctx.net_iface.peek(|i| {
        i.iter()
            .filter(|(_, info)| {
                info.ip_info.as_ref().map_or(false, |i| {
                    i.device_type != Some(NetworkInterfaceType::Loopback)
                })
            })
            .map(|(name, _)| name)
            .filter(|id| id.as_str() != WIREGUARD_INTERFACE_NAME)
            .cloned()
            .collect::<Vec<_>>()
    }) {
        add_iptables_rule(
            true,
            false,
            &[
                "POSTROUTING",
                "-s",
                &subnet.trunc().to_string(),
                "-o",
                iface.as_str(),
                "-j",
                "MASQUERADE",
            ],
        )
        .await?;
    }

    Ok(())
}

pub async fn remove_subnet(
    ctx: TunnelContext,
    _: Empty,
    SubnetParams { subnet }: SubnetParams,
) -> Result<(), Error> {
    let (server, keep) = ctx
        .db
        .mutate(|db| {
            db.as_wg_mut().as_subnets_mut().remove(&subnet)?;
            Ok((db.as_wg().de()?, db.gc_forwards()?))
        })
        .await
        .result?;
    server.sync().await?;
    ctx.gc_forwards(&keep).await?;

    for iface in ctx.net_iface.peek(|i| {
        i.iter()
            .filter(|(_, info)| {
                info.ip_info.as_ref().map_or(false, |i| {
                    i.device_type != Some(NetworkInterfaceType::Loopback)
                })
            })
            .map(|(name, _)| name)
            .filter(|id| id.as_str() != WIREGUARD_INTERFACE_NAME)
            .cloned()
            .collect::<Vec<_>>()
    }) {
        add_iptables_rule(
            true,
            true,
            &[
                "POSTROUTING",
                "-s",
                &subnet.trunc().to_string(),
                "-o",
                iface.as_str(),
                "-j",
                "MASQUERADE",
            ],
        )
        .await?;
    }

    Ok(())
}

#[derive(Deserialize, Serialize, Parser, TS)]
#[group(skip)]
#[serde(rename_all = "camelCase")]
pub struct AddDeviceParams {
    #[ts(type = "string")]
    subnet: Ipv4Net,
    name: InternedString,
    #[ts(type = "string | null")]
    ip: Option<Ipv4Addr>,
}

pub async fn add_device(
    ctx: TunnelContext,
    AddDeviceParams { subnet, name, ip }: AddDeviceParams,
) -> Result<(), Error> {
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
                    if ip == subnet.addr() {
                        return Err(Error::new(eyre!("invalid ip"), ErrorKind::InvalidRequest));
                    }
                    if !subnet.contains(&ip) {
                        return Err(Error::new(
                            eyre!("ip not in subnet"),
                            ErrorKind::InvalidRequest,
                        ));
                    }
                    let client = clients
                        .entry(ip)
                        .or_insert_with(|| WgConfig::generate(name.clone()));
                    client.name = name;

                    Ok(())
                })?;
            db.as_wg().de()
        })
        .await
        .result?;
    server.sync().await
}

#[derive(Deserialize, Serialize, Parser, TS)]
#[group(skip)]
#[serde(rename_all = "camelCase")]
pub struct RemoveDeviceParams {
    #[ts(type = "string")]
    subnet: Ipv4Net,
    #[ts(type = "string")]
    ip: Ipv4Addr,
}

pub async fn remove_device(
    ctx: TunnelContext,
    RemoveDeviceParams { subnet, ip }: RemoveDeviceParams,
) -> Result<(), Error> {
    let (server, keep) = ctx
        .db
        .mutate(|db| {
            db.as_wg_mut()
                .as_subnets_mut()
                .as_idx_mut(&subnet)
                .or_not_found(&subnet)?
                .as_clients_mut()
                .remove(&ip)?
                .or_not_found(&ip)?;
            Ok((db.as_wg().de()?, db.gc_forwards()?))
        })
        .await
        .result?;
    server.sync().await?;
    ctx.gc_forwards(&keep).await
}

#[derive(Deserialize, Serialize, Parser, TS)]
#[group(skip)]
#[serde(rename_all = "camelCase")]
pub struct ListDevicesParams {
    #[ts(type = "string")]
    subnet: Ipv4Net,
}

pub async fn list_devices(
    ctx: TunnelContext,
    ListDevicesParams { subnet }: ListDevicesParams,
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

#[derive(Deserialize, Serialize, Parser, TS)]
#[group(skip)]
#[serde(rename_all = "camelCase")]
pub struct ShowConfigParams {
    #[ts(type = "string")]
    subnet: Ipv4Net,
    #[ts(type = "string")]
    ip: Ipv4Addr,
    #[ts(type = "string | null")]
    wan_addr: Option<IpAddr>,
    #[serde(rename = "__ConnectInfo_local_addr")]
    #[arg(skip)]
    #[ts(skip)]
    local_addr: Option<SocketAddr>,
}

pub async fn show_config(
    ctx: TunnelContext,
    ShowConfigParams {
        subnet,
        ip,
        wan_addr,
        local_addr,
    }: ShowConfigParams,
) -> Result<String, Error> {
    let peek = ctx.db.peek().await;
    let wg = peek.as_wg();
    let client = wg
        .as_subnets()
        .as_idx(&subnet)
        .or_not_found(&subnet)?
        .as_clients()
        .as_idx(&ip)
        .or_not_found(&ip)?
        .de()?;
    let wan_addr = if let Some(wan_addr) = wan_addr.or(local_addr.map(|a| a.ip())).filter(|ip| {
        !ip.is_loopback()
            && !match ip {
                IpAddr::V4(ipv4) => ipv4.is_private() || ipv4.is_link_local(),
                IpAddr::V6(ipv6) => ipv6.is_unique_local() || ipv6.is_unicast_link_local(),
            }
    }) {
        wan_addr
    } else if let Some(webserver) = peek.as_webserver().as_listen().de()? {
        webserver.ip()
    } else {
        ctx.net_iface
            .peek(|i| {
                i.iter().find_map(|(_, info)| {
                    info.ip_info
                        .as_ref()
                        .and_then(|ip_info| ip_info.wan_ip)
                        .map(IpAddr::from)
                })
            })
            .or_not_found("a public IP address")?
    };
    Ok(client
        .client_config(
            ip,
            subnet,
            wg.as_key().de()?.verifying_key(),
            (wan_addr, wg.as_port().de()?).into(),
        )
        .to_string())
}

#[derive(Deserialize, Serialize, Parser, TS)]
#[group(skip)]
#[serde(rename_all = "camelCase")]
pub struct AddPortForwardParams {
    #[ts(type = "string")]
    source: SocketAddrV4,
    #[ts(type = "string")]
    target: SocketAddrV4,
    #[arg(long)]
    label: Option<String>,
}

pub async fn add_forward(
    ctx: TunnelContext,
    AddPortForwardParams {
        source,
        target,
        label,
    }: AddPortForwardParams,
) -> Result<(), Error> {
    let prefix = ctx
        .net_iface
        .peek(|i| {
            i.iter()
                .find_map(|(_, i)| {
                    i.ip_info.as_ref().and_then(|i| {
                        i.subnets
                            .iter()
                            .find(|s| s.contains(&IpAddr::from(*target.ip())))
                    })
                })
                .cloned()
        })
        .map(|s| s.prefix_len())
        .unwrap_or(32);
    let rc = ctx
        .forward
        .add_forward(source, target, prefix, None)
        .await?;
    ctx.active_forwards.mutate(|m| {
        m.insert(source, rc);
    });

    let entry = PortForwardEntry {
        target,
        label,
        enabled: true,
    };

    ctx.db
        .mutate(|db| {
            db.as_port_forwards_mut()
                .insert(&source, &entry)
                .and_then(|replaced| {
                    if replaced.is_some() {
                        Err(Error::new(
                            eyre!("Port forward from {source} already exists"),
                            ErrorKind::InvalidRequest,
                        ))
                    } else {
                        Ok(())
                    }
                })
        })
        .await
        .result?;

    Ok(())
}

#[derive(Deserialize, Serialize, Parser, TS)]
#[group(skip)]
#[serde(rename_all = "camelCase")]
pub struct RemovePortForwardParams {
    #[ts(type = "string")]
    source: SocketAddrV4,
}

pub async fn remove_forward(
    ctx: TunnelContext,
    RemovePortForwardParams { source, .. }: RemovePortForwardParams,
) -> Result<(), Error> {
    ctx.db
        .mutate(|db| db.as_port_forwards_mut().remove(&source))
        .await
        .result?;
    if let Some(rc) = ctx.active_forwards.mutate(|m| m.remove(&source)) {
        drop(rc);
        ctx.forward.gc().await?;
    }
    Ok(())
}

#[derive(Deserialize, Serialize, Parser, TS)]
#[group(skip)]
#[serde(rename_all = "camelCase")]
pub struct UpdatePortForwardLabelParams {
    #[ts(type = "string")]
    source: SocketAddrV4,
    label: Option<String>,
}

pub async fn update_forward_label(
    ctx: TunnelContext,
    UpdatePortForwardLabelParams { source, label }: UpdatePortForwardLabelParams,
) -> Result<(), Error> {
    ctx.db
        .mutate(|db| {
            db.as_port_forwards_mut().mutate(|pf| {
                let entry = pf.0.get_mut(&source).ok_or_else(|| {
                    Error::new(
                        eyre!("Port forward from {source} not found"),
                        ErrorKind::NotFound,
                    )
                })?;
                entry.label = label;
                Ok(())
            })
        })
        .await
        .result
}

#[derive(Deserialize, Serialize, Parser, TS)]
#[group(skip)]
#[serde(rename_all = "camelCase")]
pub struct SetPortForwardEnabledParams {
    #[ts(type = "string")]
    source: SocketAddrV4,
    #[arg(long)]
    enabled: bool,
}

pub async fn set_forward_enabled(
    ctx: TunnelContext,
    SetPortForwardEnabledParams { source, enabled }: SetPortForwardEnabledParams,
) -> Result<(), Error> {
    let target = ctx
        .db
        .mutate(|db| {
            db.as_port_forwards_mut().mutate(|pf| {
                let entry = pf.0.get_mut(&source).ok_or_else(|| {
                    Error::new(
                        eyre!("Port forward from {source} not found"),
                        ErrorKind::NotFound,
                    )
                })?;
                entry.enabled = enabled;
                Ok(entry.target)
            })
        })
        .await
        .result?;

    if enabled {
        let prefix = ctx
            .net_iface
            .peek(|i| {
                i.iter()
                    .find_map(|(_, i)| {
                        i.ip_info.as_ref().and_then(|i| {
                            i.subnets
                                .iter()
                                .find(|s| s.contains(&IpAddr::from(*target.ip())))
                        })
                    })
                    .cloned()
            })
            .map(|s| s.prefix_len())
            .unwrap_or(32);
        let rc = ctx
            .forward
            .add_forward(source, target, prefix, None)
            .await?;
        ctx.active_forwards.mutate(|m| {
            m.insert(source, rc);
        });
    } else {
        if let Some(rc) = ctx.active_forwards.mutate(|m| m.remove(&source)) {
            drop(rc);
            ctx.forward.gc().await?;
        }
    }

    Ok(())
}
