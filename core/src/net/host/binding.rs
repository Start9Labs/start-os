use std::collections::{BTreeMap, BTreeSet};
use std::net::SocketAddr;
use std::str::FromStr;

use clap::Parser;
use clap::builder::ValueParserFactory;
use rpc_toolkit::{Context, Empty, HandlerArgs, HandlerExt, ParentHandler, from_fn_async};
use serde::{Deserialize, Serialize};
use ts_rs::TS;

use crate::HostId;
use crate::context::{CliContext, RpcContext};
use crate::db::prelude::Map;
use crate::net::forward::AvailablePorts;
use crate::net::host::HostApiKind;
use crate::net::service_interface::{HostnameInfo, HostnameMetadata};
use crate::net::vhost::AlpnInfo;
use crate::prelude::*;
use crate::util::FromStrParser;
use crate::util::serde::{HandlerExtSerde, display_serializable};

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

#[derive(Debug, Default, Clone, Deserialize, Serialize, TS, HasModel)]
#[serde(rename_all = "camelCase")]
#[ts(export)]
#[model = "Model<Self>"]
pub struct DerivedAddressInfo {
    /// User override: enable these addresses (only for public IP & port)
    pub enabled: BTreeSet<SocketAddr>,
    /// User override: disable these addresses (only for domains and private IP & port)
    pub disabled: BTreeSet<(InternedString, u16)>,
    /// COMPUTED: NetServiceData::update — all possible addresses for this binding
    pub available: BTreeSet<HostnameInfo>,
}

impl DerivedAddressInfo {
    /// Returns addresses that are currently enabled after applying overrides.
    /// Default: public IPs are disabled, everything else is enabled.
    /// Explicit `enabled`/`disabled` overrides take precedence.
    pub fn enabled(&self) -> BTreeSet<&HostnameInfo> {
        self.available
            .iter()
            .filter(|h| {
                if h.public && h.metadata.is_ip() {
                    // Public IPs: disabled by default, explicitly enabled via SocketAddr
                    h.to_socket_addr().map_or(
                        true, // should never happen, but would rather see them if it does
                        |sa| self.enabled.contains(&sa),
                    )
                } else {
                    !self
                        .disabled
                        .contains(&(h.hostname.clone(), h.port.unwrap_or_default())) // disablable addresses will always have a port
                }
            })
            .collect()
    }
}

#[derive(Debug, Default, Deserialize, Serialize, HasModel, TS)]
#[model = "Model<Self>"]
#[ts(export)]
pub struct Bindings(pub BTreeMap<u16, BindInfo>);

impl Map for Bindings {
    type Key = u16;
    type Value = BindInfo;
    fn key_str(key: &Self::Key) -> Result<impl AsRef<str>, Error> {
        Self::key_string(key)
    }
    fn key_string(key: &Self::Key) -> Result<InternedString, Error> {
        Ok(InternedString::from_display(key))
    }
}

impl std::ops::Deref for Bindings {
    type Target = BTreeMap<u16, BindInfo>;
    fn deref(&self) -> &Self::Target {
        &self.0
    }
}

impl std::ops::DerefMut for Bindings {
    fn deref_mut(&mut self) -> &mut Self::Target {
        &mut self.0
    }
}

#[derive(Debug, Deserialize, Serialize, HasModel, TS)]
#[serde(rename_all = "camelCase")]
#[model = "Model<Self>"]
#[ts(export)]
pub struct BindInfo {
    pub enabled: bool,
    pub options: BindOptions,
    pub net: NetInfo,
    pub addresses: DerivedAddressInfo,
}

#[derive(Clone, Debug, Deserialize, Serialize, TS, PartialEq, Eq, PartialOrd, Ord)]
#[serde(rename_all = "camelCase")]
#[ts(export)]
pub struct NetInfo {
    pub assigned_port: Option<u16>,
    pub assigned_ssl_port: Option<u16>,
}
impl BindInfo {
    pub fn new(available_ports: &mut AvailablePorts, options: BindOptions) -> Result<Self, Error> {
        let mut assigned_port = None;
        let mut assigned_ssl_port = None;
        if let Some(ssl) = &options.add_ssl {
            assigned_ssl_port = available_ports
                .try_alloc(ssl.preferred_external_port, true)
                .or_else(|| Some(available_ports.alloc(true).ok()?));
        }
        if options
            .secure
            .map_or(true, |s| !(s.ssl && options.add_ssl.is_some()))
        {
            assigned_port = available_ports
                .try_alloc(options.preferred_external_port, false)
                .or_else(|| Some(available_ports.alloc(false).ok()?));
        }

        Ok(Self {
            enabled: true,
            options,
            net: NetInfo {
                assigned_port,
                assigned_ssl_port,
            },
            addresses: DerivedAddressInfo::default(),
        })
    }
    pub fn update(
        self,
        available_ports: &mut AvailablePorts,
        options: BindOptions,
    ) -> Result<Self, Error> {
        let Self {
            net: mut lan,
            addresses,
            ..
        } = self;
        if options
            .secure
            .map_or(true, |s| !(s.ssl && options.add_ssl.is_some()))
        // doesn't make sense to have 2 listening ports, both with ssl
        {
            lan.assigned_port = if let Some(port) = lan.assigned_port.take() {
                Some(port)
            } else if let Some(port) =
                available_ports.try_alloc(options.preferred_external_port, false)
            {
                Some(port)
            } else {
                Some(available_ports.alloc(false)?)
            };
        } else {
            if let Some(port) = lan.assigned_port.take() {
                available_ports.free([port]);
            }
        }
        if let Some(ssl) = &options.add_ssl {
            lan.assigned_ssl_port = if let Some(port) = lan.assigned_ssl_port.take() {
                Some(port)
            } else if let Some(port) = available_ports.try_alloc(ssl.preferred_external_port, true)
            {
                Some(port)
            } else {
                Some(available_ports.alloc(true)?)
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
            addresses,
        })
    }
    pub fn disable(&mut self) {
        self.enabled = false;
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
    #[serde(default)]
    pub add_x_forwarded_headers: bool, // TODO
    pub alpn: Option<AlpnInfo>,
}

pub fn binding<C: Context, Kind: HostApiKind>()
-> ParentHandler<C, Kind::Params, Kind::InheritedParams> {
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
                    for (internal, info) in res.iter() {
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
                .with_about("about.list-bindings-for-host")
                .with_call_remote::<CliContext>(),
        )
        .subcommand(
            "set-address-enabled",
            from_fn_async(set_address_enabled::<Kind>)
                .with_metadata("sync_db", Value::Bool(true))
                .with_inherited(Kind::inheritance)
                .no_display()
                .with_about("about.set-address-enabled-for-binding")
                .with_call_remote::<CliContext>(),
        )
}

pub async fn list_bindings<Kind: HostApiKind>(
    ctx: RpcContext,
    _: Empty,
    inheritance: Kind::Inheritance,
) -> Result<Bindings, Error> {
    Kind::host_for(&inheritance, &mut ctx.db.peek().await)?
        .as_bindings()
        .de()
}

#[derive(Deserialize, Serialize, Parser, TS)]
#[group(skip)]
#[serde(rename_all = "camelCase")]
#[ts(export)]
pub struct BindingSetAddressEnabledParams {
    #[arg(help = "help.arg.internal-port")]
    internal_port: u16,
    #[arg(long, help = "help.arg.address")]
    address: String,
    #[arg(long, help = "help.arg.binding-enabled")]
    enabled: Option<bool>,
}

pub async fn set_address_enabled<Kind: HostApiKind>(
    ctx: RpcContext,
    BindingSetAddressEnabledParams {
        internal_port,
        address,
        enabled,
    }: BindingSetAddressEnabledParams,
    inheritance: Kind::Inheritance,
) -> Result<(), Error> {
    let enabled = enabled.unwrap_or(true);
    let address: HostnameInfo =
        serde_json::from_str(&address).with_kind(ErrorKind::Deserialization)?;
    ctx.db
        .mutate(|db| {
            Kind::host_for(&inheritance, db)?
                .as_bindings_mut()
                .mutate(|b| {
                    let bind = b.get_mut(&internal_port).or_not_found(internal_port)?;
                    if address.public && address.metadata.is_ip() {
                        // Public IPs: toggle via SocketAddr in `enabled` set
                        let sa = address.to_socket_addr().ok_or_else(|| {
                            Error::new(
                                eyre!("cannot convert address to socket addr"),
                                ErrorKind::InvalidRequest,
                            )
                        })?;
                        if enabled {
                            bind.addresses.enabled.insert(sa);
                        } else {
                            bind.addresses.enabled.remove(&sa);
                        }
                        // Non-SSL Ipv4: cascade to PublicDomains on same gateway
                        if !address.ssl {
                            if let HostnameMetadata::Ipv4 { gateway } =
                                &address.metadata
                            {
                                let port = sa.port();
                                for a in &bind.addresses.available {
                                    if a.ssl {
                                        continue;
                                    }
                                    if let HostnameMetadata::PublicDomain {
                                        gateway: gw,
                                    } = &a.metadata
                                    {
                                        if gw == gateway
                                            && a.port.unwrap_or(80) == port
                                        {
                                            let k = (
                                                a.hostname.clone(),
                                                a.port.unwrap_or(80),
                                            );
                                            if enabled {
                                                bind.addresses
                                                    .disabled
                                                    .remove(&k);
                                            } else {
                                                bind.addresses
                                                    .disabled
                                                    .insert(k);
                                            }
                                        }
                                    }
                                }
                            }
                        }
                    } else {
                        // Domains and private IPs: toggle via (host, port) in `disabled` set
                        let port = address.port.unwrap_or(if address.ssl { 443 } else { 80 });
                        let key = (address.hostname.clone(), port);
                        if enabled {
                            bind.addresses.disabled.remove(&key);
                        } else {
                            bind.addresses.disabled.insert(key);
                        }
                        // Non-SSL PublicDomain: cascade to Ipv4 + other PublicDomains on same gateway
                        if !address.ssl {
                            if let HostnameMetadata::PublicDomain { gateway } =
                                &address.metadata
                            {
                                for a in &bind.addresses.available {
                                    if a.ssl {
                                        continue;
                                    }
                                    match &a.metadata {
                                        HostnameMetadata::Ipv4 { gateway: gw }
                                            if a.public
                                                && gw == gateway =>
                                        {
                                            if let Some(sa) =
                                                a.to_socket_addr()
                                            {
                                                if sa.port() == port {
                                                    if enabled {
                                                        bind.addresses
                                                            .enabled
                                                            .insert(sa);
                                                    } else {
                                                        bind.addresses
                                                            .enabled
                                                            .remove(&sa);
                                                    }
                                                }
                                            }
                                        }
                                        HostnameMetadata::PublicDomain {
                                            gateway: gw,
                                        } if gw == gateway => {
                                            let dp = a.port.unwrap_or(80);
                                            if dp == port {
                                                let k = (
                                                    a.hostname.clone(),
                                                    dp,
                                                );
                                                if enabled {
                                                    bind.addresses
                                                        .disabled
                                                        .remove(&k);
                                                } else {
                                                    bind.addresses
                                                        .disabled
                                                        .insert(k);
                                                }
                                            }
                                        }
                                        _ => {}
                                    }
                                }
                            }
                        }
                    }
                    Ok(())
                })
        })
        .await
        .result?;
    Ok(())
}
