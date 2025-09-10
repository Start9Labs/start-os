use std::collections::{BTreeMap, BTreeSet};
use std::str::FromStr;

use clap::builder::ValueParserFactory;
use clap::Parser;
use imbl::OrdSet;
use models::{FromStrParser, GatewayId, HostId};
use rpc_toolkit::{from_fn_async, Context, Empty, HandlerArgs, HandlerExt, ParentHandler};
use serde::{Deserialize, Serialize};
use ts_rs::TS;

use crate::context::{CliContext, RpcContext};
use crate::db::model::public::NetworkInterfaceInfo;
use crate::net::forward::AvailablePorts;
use crate::net::gateway::InterfaceFilter;
use crate::net::host::HostApiKind;
use crate::net::vhost::AlpnInfo;
use crate::prelude::*;
use crate::util::serde::{display_serializable, HandlerExtSerde};

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Serialize, Deserialize, TS)]
#[ts(export)]
#[serde(rename_all = "camelCase")]
pub struct BindId {
    pub id: HostId,
    pub internal_port: u16,
}
impl ValueParserFactory for BindId {
    type Parser = FromStrParser<Self>;
    fn value_parser() -> Self::Parser {
        FromStrParser::new()
    }
}
impl FromStr for BindId {
    type Err = Error;
    fn from_str(s: &str) -> Result<Self, Self::Err> {
        let (id, port) = s
            .split_once(":")
            .ok_or_else(|| Error::new(eyre!("expected <id>:<port>"), ErrorKind::ParseUrl))?;
        Ok(Self {
            id: id.parse()?,
            internal_port: port.parse()?,
        })
    }
}

#[derive(Debug, Deserialize, Serialize, TS)]
#[serde(rename_all = "camelCase")]
#[ts(export)]
pub struct BindInfo {
    pub enabled: bool,
    pub options: BindOptions,
    pub net: NetInfo,
}

#[derive(Clone, Debug, Deserialize, Serialize, TS, PartialEq, Eq, PartialOrd, Ord)]
#[serde(rename_all = "camelCase")]
#[ts(export)]
pub struct NetInfo {
    #[ts(as = "BTreeSet::<GatewayId>")]
    #[serde(default)]
    pub private_disabled: OrdSet<GatewayId>,
    #[ts(as = "BTreeSet::<GatewayId>")]
    #[serde(default)]
    pub public_enabled: OrdSet<GatewayId>,
    pub assigned_port: Option<u16>,
    pub assigned_ssl_port: Option<u16>,
}
impl BindInfo {
    pub fn new(available_ports: &mut AvailablePorts, options: BindOptions) -> Result<Self, Error> {
        let mut assigned_port = None;
        let mut assigned_ssl_port = None;
        if options.add_ssl.is_some() {
            assigned_ssl_port = Some(available_ports.alloc()?);
        }
        if options
            .secure
            .map_or(true, |s| !(s.ssl && options.add_ssl.is_some()))
        {
            assigned_port = Some(available_ports.alloc()?);
        }

        Ok(Self {
            enabled: true,
            options,
            net: NetInfo {
                private_disabled: OrdSet::new(),
                public_enabled: OrdSet::new(),
                assigned_port,
                assigned_ssl_port,
            },
        })
    }
    pub fn update(
        self,
        available_ports: &mut AvailablePorts,
        options: BindOptions,
    ) -> Result<Self, Error> {
        let Self { net: mut lan, .. } = self;
        if options
            .secure
            .map_or(true, |s| !(s.ssl && options.add_ssl.is_some()))
        // doesn't make sense to have 2 listening ports, both with ssl
        {
            lan.assigned_port = if let Some(port) = lan.assigned_port.take() {
                Some(port)
            } else {
                Some(available_ports.alloc()?)
            };
        } else {
            if let Some(port) = lan.assigned_port.take() {
                available_ports.free([port]);
            }
        }
        if options.add_ssl.is_some() {
            lan.assigned_ssl_port = if let Some(port) = lan.assigned_ssl_port.take() {
                Some(port)
            } else {
                Some(available_ports.alloc()?)
            };
        } else {
            if let Some(port) = lan.assigned_ssl_port.take() {
                available_ports.free([port]);
            }
        }
        Ok(Self {
            enabled: true,
            options,
            net: lan,
        })
    }
    pub fn disable(&mut self) {
        self.enabled = false;
    }
}
impl InterfaceFilter for NetInfo {
    fn filter(&self, id: &GatewayId, info: &NetworkInterfaceInfo) -> bool {
        if info.public() {
            self.public_enabled.contains(id)
        } else {
            !self.private_disabled.contains(id)
        }
    }
}

#[derive(Debug, Clone, Copy, serde::Serialize, serde::Deserialize, TS)]
#[ts(export)]
#[serde(rename_all = "camelCase")]
pub struct Security {
    pub ssl: bool,
}

#[derive(Clone, Debug, Deserialize, Serialize, TS)]
#[serde(rename_all = "camelCase")]
#[ts(export)]
pub struct BindOptions {
    pub preferred_external_port: u16,
    pub add_ssl: Option<AddSslOptions>,
    pub secure: Option<Security>,
}

#[derive(Debug, Clone, Deserialize, Serialize, PartialEq, Eq, TS)]
#[serde(rename_all = "camelCase")]
#[ts(export)]
pub struct AddSslOptions {
    pub preferred_external_port: u16,
    // #[serde(default)]
    // pub add_x_forwarded_headers: bool, // TODO
    pub alpn: Option<AlpnInfo>,
}

pub fn binding<C: Context, Kind: HostApiKind>(
) -> ParentHandler<C, Kind::Params, Kind::InheritedParams> {
    ParentHandler::<C, Kind::Params, Kind::InheritedParams>::new()
        .subcommand(
            "list",
            from_fn_async(list_bindings::<Kind>)
                .with_inherited(Kind::inheritance)
                .with_display_serializable()
                .with_custom_display_fn(|HandlerArgs { params, .. }, res| {
                    use prettytable::*;

                    if let Some(format) = params.format {
                        return display_serializable(format, res);
                    }

                    let mut table = Table::new();
                    table.add_row(row![bc => "INTERNAL PORT", "ENABLED", "EXTERNAL PORT", "EXTERNAL SSL PORT"]);
                    for (internal, info) in res {
                        table.add_row(row![
                            internal,
                            info.enabled,
                            if let Some(port) = info.net.assigned_port {
                                port.to_string()
                            } else {
                                "N/A".to_owned()
                            },
                            if let Some(port) = info.net.assigned_ssl_port {
                                port.to_string()
                            } else {
                                "N/A".to_owned()
                            },
                        ]);
                    }

                    table.print_tty(false)?;

                    Ok(())
                })
                .with_about("List bindinges for this host")
                .with_call_remote::<CliContext>(),
        )
        .subcommand(
            "set-gateway-enabled",
            from_fn_async(set_gateway_enabled::<Kind>)
                .with_metadata("sync_db", Value::Bool(true))
                .with_inherited(Kind::inheritance)
                .no_display()
                .with_about("Set whether this gateway should be enabled for this binding")
                .with_call_remote::<CliContext>(),
        )
}

pub async fn list_bindings<Kind: HostApiKind>(
    ctx: RpcContext,
    _: Empty,
    inheritance: Kind::Inheritance,
) -> Result<BTreeMap<u16, BindInfo>, Error> {
    Kind::host_for(&inheritance, &mut ctx.db.peek().await)?
        .as_bindings()
        .de()
}

#[derive(Deserialize, Serialize, Parser, TS)]
#[serde(rename_all = "camelCase")]
#[ts(export)]
pub struct BindingGatewaySetEnabledParams {
    internal_port: u16,
    gateway: GatewayId,
    #[arg(long)]
    enabled: Option<bool>,
}

pub async fn set_gateway_enabled<Kind: HostApiKind>(
    ctx: RpcContext,
    BindingGatewaySetEnabledParams {
        internal_port,
        gateway,
        enabled,
    }: BindingGatewaySetEnabledParams,
    inheritance: Kind::Inheritance,
) -> Result<(), Error> {
    let enabled = enabled.unwrap_or(true);
    let gateway_public = ctx
        .net_controller
        .net_iface
        .watcher
        .ip_info()
        .get(&gateway)
        .or_not_found(&gateway)?
        .public();
    ctx.db
        .mutate(|db| {
            Kind::host_for(&inheritance, db)?
                .as_bindings_mut()
                .mutate(|b| {
                    let net = &mut b.get_mut(&internal_port).or_not_found(internal_port)?.net;
                    if gateway_public {
                        if enabled {
                            net.public_enabled.insert(gateway);
                        } else {
                            net.public_enabled.remove(&gateway);
                        }
                    } else {
                        if enabled {
                            net.private_disabled.remove(&gateway);
                        } else {
                            net.private_disabled.insert(gateway);
                        }
                    }
                    Ok(())
                })
        })
        .await
        .result?;
    Kind::sync_host(&ctx, inheritance).await
}
