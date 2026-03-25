use std::collections::{BTreeMap, BTreeSet, HashMap};
use std::future::Future;
use std::net::{IpAddr, Ipv4Addr, Ipv6Addr, SocketAddr};
use std::sync::Arc;
use std::task::Poll;
use std::time::{Duration, Instant};

use clap::Parser;
use futures::{FutureExt, Stream, StreamExt, TryStreamExt};
use imbl::{OrdMap, OrdSet};
use imbl_value::{InternedString, Value};
use ipnet::IpNet;
use itertools::Itertools;
use nix::net::if_::if_nametoindex;
use patch_db::json_ptr::JsonPointer;
use rpc_toolkit::{Context, HandlerArgs, HandlerExt, ParentHandler, from_fn_async};
use serde::{Deserialize, Serialize};
use tokio::io::{AsyncBufReadExt, BufReader};
use tokio::net::TcpListener;
use tokio::process::Command;
use tokio::sync::oneshot;
use ts_rs::TS;
use url::Url;
use visit_rs::{Visit, VisitFields};
use zbus::proxy::{PropertyChanged, PropertyStream, SignalStream};
use zbus::zvariant::{
    DeserializeDict, Dict, OwnedObjectPath, OwnedValue, Type as ZType, Value as ZValue,
};
use zbus::{Connection, proxy};

use crate::context::{CliContext, RpcContext};
use crate::db::model::Database;
use crate::db::model::public::{IpInfo, NetworkInterfaceInfo, NetworkInterfaceType};
use crate::net::forward::START9_BRIDGE_IFACE;
use crate::net::gateway::device::DeviceProxy;
use crate::net::host::all_hosts;
use crate::net::utils::find_wifi_iface;
use crate::net::web_server::{Accept, AcceptStream, MetadataVisitor, TcpMetadata};
use crate::prelude::*;
use crate::util::Invoke;
use crate::util::collections::OrdMapIterMut;
use crate::util::future::{NonDetachingJoinHandle, Until};
use crate::util::io::open_file;
use crate::util::serde::{HandlerExtSerde, display_serializable};
use crate::util::sync::Watch;
use crate::{GatewayId, PackageId};

pub fn gateway_api<C: Context>() -> ParentHandler<C> {
    ParentHandler::new()
        .subcommand(
            "list",
            from_fn_async(list_interfaces)
                .with_display_serializable()
                .with_custom_display_fn(|HandlerArgs { params, .. }, res| {
                    use prettytable::*;

                    if let Some(format) = params.format {
                        return display_serializable(format, res);
                    }

                    let mut table = Table::new();
                    table.add_row(row![bc => "INTERFACE", "TYPE", "ADDRESSES", "WAN IP"]);
                    for (iface, info) in res {
                        table.add_row(row![
                            iface,
                            info.ip_info
                                .as_ref()
                                .and_then(|ip_info| ip_info.device_type)
                                .map_or_else(|| "UNKNOWN".to_owned(), |ty| format!("{ty:?}")),
                            info.ip_info.as_ref().map_or_else(
                                || "<DISCONNECTED>".to_owned(),
                                |ip_info| ip_info
                                    .subnets
                                    .iter()
                                    .map(|ipnet| match ipnet.addr() {
                                        IpAddr::V4(ip) => format!("{ip}/{}", ipnet.prefix_len()),
                                        IpAddr::V6(ip) => format!(
                                            "[{ip}%{}]/{}",
                                            ip_info.scope_id,
                                            ipnet.prefix_len()
                                        ),
                                    })
                                    .join(", ")
                            ),
                            info.ip_info
                                .as_ref()
                                .and_then(|ip_info| ip_info.wan_ip)
                                .map_or_else(|| "N/A".to_owned(), |ip| ip.to_string())
                        ]);
                    }

                    table.print_tty(false)?;

                    Ok(())
                })
                .with_about("about.show-gateways-startos-can-listen-on")
                .with_call_remote::<CliContext>(),
        )
        .subcommand(
            "forget",
            from_fn_async(forget_iface)
                .with_metadata("sync_db", Value::Bool(true))
                .no_display()
                .with_about("about.forget-disconnected-gateway")
                .with_call_remote::<CliContext>(),
        )
        .subcommand(
            "set-name",
            from_fn_async(set_name)
                .with_metadata("sync_db", Value::Bool(true))
                .no_display()
                .with_about("about.rename-gateway")
                .with_call_remote::<CliContext>(),
        )
        .subcommand(
            "check-port",
            from_fn_async(check_port)
                .with_display_serializable()
                .with_about("about.check-port-reachability")
                .with_call_remote::<CliContext>(),
        )
        .subcommand(
            "check-dns",
            from_fn_async(check_dns)
                .with_display_serializable()
                .with_about("about.check-dns-configuration")
                .with_call_remote::<CliContext>(),
        )
        .subcommand(
            "set-default-outbound",
            from_fn_async(set_default_outbound)
                .with_metadata("sync_db", Value::Bool(true))
                .no_display()
                .with_about("about.set-default-outbound-gateway")
                .with_call_remote::<CliContext>(),
        )
}

async fn list_interfaces(
    ctx: RpcContext,
) -> Result<OrdMap<GatewayId, NetworkInterfaceInfo>, Error> {
    Ok(ctx.net_controller.net_iface.watcher.ip_info())
}

#[derive(Debug, Clone, Deserialize, Serialize, Parser, TS)]
#[group(skip)]
#[ts(export)]
struct ForgetGatewayParams {
    #[arg(help = "help.arg.gateway-id")]
    gateway: GatewayId,
}

async fn forget_iface(
    ctx: RpcContext,
    ForgetGatewayParams { gateway }: ForgetGatewayParams,
) -> Result<(), Error> {
    ctx.net_controller.net_iface.forget(&gateway).await
}

#[derive(Debug, Clone, Deserialize, Serialize, Parser, TS)]
#[group(skip)]
#[ts(export)]
struct RenameGatewayParams {
    #[arg(help = "help.arg.gateway-id")]
    id: GatewayId,
    #[arg(help = "help.arg.gateway-name")]
    name: InternedString,
}

async fn set_name(
    ctx: RpcContext,
    RenameGatewayParams { id, name }: RenameGatewayParams,
) -> Result<(), Error> {
    ctx.net_controller.net_iface.set_name(&id, name).await
}

#[derive(Debug, Clone, Deserialize, Serialize, Parser, TS)]
#[group(skip)]
#[serde(rename_all = "camelCase")]
#[ts(export)]
pub struct CheckPortParams {
    #[arg(help = "help.arg.port")]
    pub port: u16,
    #[arg(help = "help.arg.gateway-id")]
    pub gateway: GatewayId,
}

#[derive(Debug, Clone, Deserialize, Serialize, TS)]
#[serde(rename_all = "camelCase")]
#[ts(export)]
pub struct CheckPortRes {
    pub ip: Ipv4Addr,
    pub port: u16,
    pub open_externally: bool,
    pub open_internally: bool,
    pub hairpinning: bool,
}

#[derive(Debug, Clone, Deserialize, Serialize)]
#[serde(rename_all = "camelCase")]
pub struct IfconfigPortRes {
    pub ip: Ipv4Addr,
    pub port: u16,
    pub reachable: bool,
}

pub async fn check_port(
    ctx: RpcContext,
    CheckPortParams { port, gateway }: CheckPortParams,
) -> Result<CheckPortRes, Error> {
    let db = ctx.db.peek().await;
    let base_urls = db.as_public().as_server_info().as_echoip_urls().de()?;
    let gateways = db
        .as_public()
        .as_server_info()
        .as_network()
        .as_gateways()
        .de()?;
    let gw_info = gateways
        .get(&gateway)
        .ok_or_else(|| Error::new(eyre!("unknown gateway: {gateway}"), ErrorKind::NotFound))?;
    let ip_info = gw_info.ip_info.as_ref().ok_or_else(|| {
        Error::new(
            eyre!("gateway {gateway} has no IP info"),
            ErrorKind::NotFound,
        )
    })?;

    let internal_ips = ip_info
        .subnets
        .iter()
        .map(|i| i.addr())
        .filter(|a| a.is_ipv4())
        .map(|a| SocketAddr::new(a, port))
        .collect::<Vec<_>>();

    let open_internally = tokio::time::timeout(
        Duration::from_secs(5),
        tokio::net::TcpStream::connect(&*internal_ips),
    )
    .await
    .map_or(false, |r| r.is_ok());

    let client = reqwest::Client::builder();
    #[cfg(target_os = "linux")]
    let client = client
        .interface(gateway.as_str())
        .local_address(IpAddr::V4(Ipv4Addr::UNSPECIFIED));
    let client = client.build()?;

    let mut res = None;
    for base_url in base_urls {
        let url = base_url
            .join(&format!("/port/{port}"))
            .with_kind(ErrorKind::ParseUrl)?;
        res = Some(
            async {
                client
                    .get(url)
                    .timeout(Duration::from_secs(5))
                    .send()
                    .await?
                    .error_for_status()?
                    .json()
                    .await
            }
            .await,
        );
        if res.as_ref().map_or(false, |r| r.is_ok()) {
            break;
        }
    }
    let Some(IfconfigPortRes {
        ip,
        port,
        reachable: open_externally,
    }) = res.transpose()?
    else {
        return Err(Error::new(
            eyre!("{}", t!("net.gateway.no-configured-echoip-urls")),
            ErrorKind::Network,
        ));
    };

    let hairpinning = tokio::time::timeout(
        Duration::from_secs(5),
        tokio::net::TcpStream::connect(SocketAddr::new(ip.into(), port)),
    )
    .await
    .map_or(false, |r| r.is_ok());

    Ok(CheckPortRes {
        ip,
        port,
        open_externally,
        open_internally,
        hairpinning,
    })
}

#[derive(Debug, Clone, Deserialize, Serialize, Parser, TS)]
#[group(skip)]
#[serde(rename_all = "camelCase")]
#[ts(export)]
pub struct CheckDnsParams {
    #[arg(help = "help.arg.gateway-id")]
    pub gateway: GatewayId,
}

pub async fn check_dns(
    ctx: RpcContext,
    CheckDnsParams { gateway }: CheckDnsParams,
) -> Result<bool, Error> {
    use hickory_server::proto::xfer::Protocol;
    use hickory_server::resolver::Resolver;
    use hickory_server::resolver::config::{NameServerConfig, ResolverConfig, ResolverOpts};
    use hickory_server::resolver::name_server::TokioConnectionProvider;

    let ip_info = ctx.net_controller.net_iface.watcher.ip_info();
    let gw_info = ip_info
        .get(&gateway)
        .ok_or_else(|| Error::new(eyre!("unknown gateway: {gateway}"), ErrorKind::NotFound))?;
    let gw_ip_info = gw_info.ip_info.as_ref().ok_or_else(|| {
        Error::new(
            eyre!("gateway {gateway} has no IP info"),
            ErrorKind::NotFound,
        )
    })?;

    for dns_ip in &gw_ip_info.dns_servers {
        // Case 1: DHCP DNS == server IP → immediate success
        if gw_ip_info.subnets.iter().any(|s| s.addr() == *dns_ip) {
            return Ok(true);
        }

        // Case 2: DHCP DNS is on LAN but not the server → TXT challenge check
        if gw_ip_info.subnets.iter().any(|s| s.contains(dns_ip)) {
            let nonce = rand::random::<u64>();
            let challenge_domain = InternedString::intern(format!("_dns-check-{nonce}.startos"));
            let challenge_value =
                InternedString::intern(crate::rpc_continuations::Guid::new().as_ref());

            let _guard = ctx
                .net_controller
                .dns
                .add_challenge(challenge_domain.clone(), challenge_value.clone())?;

            let mut config = ResolverConfig::new();
            config.add_name_server(NameServerConfig::new(
                SocketAddr::new(*dns_ip, 53),
                Protocol::Udp,
            ));
            config.add_name_server(NameServerConfig::new(
                SocketAddr::new(*dns_ip, 53),
                Protocol::Tcp,
            ));
            let mut opts = ResolverOpts::default();
            opts.timeout = Duration::from_secs(5);
            opts.attempts = 1;

            let resolver =
                Resolver::builder_with_config(config, TokioConnectionProvider::default())
                    .with_options(opts)
                    .build();
            let txt_lookup = resolver.txt_lookup(&*challenge_domain).await;

            return Ok(match txt_lookup {
                Ok(lookup) => lookup.iter().any(|txt| {
                    txt.iter()
                        .any(|data| data.as_ref() == challenge_value.as_bytes())
                }),
                Err(_) => false,
            });
        }
    }

    // Case 3: No DNS servers in subnet → failure
    Ok(false)
}

#[derive(Debug, Clone, Deserialize, Serialize, Parser, TS)]
#[group(skip)]
#[serde(rename_all = "camelCase")]
#[ts(export)]
struct SetDefaultOutboundParams {
    #[arg(help = "help.arg.gateway-id")]
    gateway: Option<GatewayId>,
}

async fn set_default_outbound(
    ctx: RpcContext,
    SetDefaultOutboundParams { gateway }: SetDefaultOutboundParams,
) -> Result<(), Error> {
    if let Some(ref gw) = gateway {
        let ip_info = ctx.net_controller.net_iface.watcher.ip_info();
        let info = ip_info
            .get(gw)
            .ok_or_else(|| Error::new(eyre!("unknown gateway: {gw}"), ErrorKind::NotFound))?;
        ensure_code!(
            info.ip_info.is_some(),
            ErrorKind::InvalidRequest,
            "gateway {gw} is not connected"
        );
    }
    ctx.db
        .mutate(|db| {
            db.as_public_mut()
                .as_server_info_mut()
                .as_network_mut()
                .as_default_outbound_mut()
                .ser(&gateway)
        })
        .await
        .result
}

#[derive(Debug, Clone, Deserialize, Serialize, Parser, TS)]
#[group(skip)]
#[serde(rename_all = "camelCase")]
#[ts(export)]
pub struct SetOutboundGatewayParams {
    #[arg(help = "help.arg.package-id")]
    package: PackageId,
    #[arg(help = "help.arg.gateway-id")]
    gateway: Option<GatewayId>,
}

pub async fn set_outbound_gateway(
    ctx: RpcContext,
    SetOutboundGatewayParams { package, gateway }: SetOutboundGatewayParams,
) -> Result<(), Error> {
    if let Some(ref gw) = gateway {
        let ip_info = ctx.net_controller.net_iface.watcher.ip_info();
        let info = ip_info
            .get(gw)
            .ok_or_else(|| Error::new(eyre!("unknown gateway: {gw}"), ErrorKind::NotFound))?;
        ensure_code!(
            info.ip_info.is_some(),
            ErrorKind::InvalidRequest,
            "gateway {gw} is not connected"
        );
    }
    ctx.db
        .mutate(|db| {
            db.as_public_mut()
                .as_package_data_mut()
                .as_idx_mut(&package)
                .or_not_found(&package)?
                .as_outbound_gateway_mut()
                .ser(&gateway)
        })
        .await
        .result
}

#[proxy(
    interface = "org.freedesktop.NetworkManager",
    default_service = "org.freedesktop.NetworkManager",
    default_path = "/org/freedesktop/NetworkManager"
)]
trait NetworkManager {
    fn get_device_by_ip_iface(&self, iface: &str) -> Result<OwnedObjectPath, Error>;

    #[zbus(property)]
    fn all_devices(&self) -> Result<Vec<OwnedObjectPath>, Error>;

    #[zbus(signal)]
    fn device_added(&self) -> Result<(), Error>;

    #[zbus(signal)]
    fn device_removed(&self) -> Result<(), Error>;

    #[zbus(signal)]
    fn state_changed(&self) -> Result<(), Error>;
}

mod active_connection {
    use zbus::proxy;
    use zbus::zvariant::OwnedObjectPath;

    use crate::prelude::*;

    #[proxy(
        interface = "org.freedesktop.NetworkManager.Connection.Active",
        default_service = "org.freedesktop.NetworkManager"
    )]
    pub trait ActiveConnection {
        #[zbus(property)]
        fn connection(&self) -> Result<OwnedObjectPath, Error>;

        #[zbus(property)]
        fn id(&self) -> Result<String, Error>;

        #[zbus(property)]
        fn state_flags(&self) -> Result<u32, Error>;

        #[zbus(property, name = "Type")]
        fn connection_type(&self) -> Result<String, Error>;

        #[zbus(signal)]
        fn state_changed(&self) -> Result<(), Error>;

        #[zbus(property)]
        fn dhcp4_config(&self) -> Result<OwnedObjectPath, Error>;
    }
}

#[proxy(
    interface = "org.freedesktop.NetworkManager.Settings.Connection",
    default_service = "org.freedesktop.NetworkManager"
)]
trait ConnectionSettings {
    fn delete(&self) -> Result<(), Error>;

    fn get_settings(&self) -> Result<HashMap<String, HashMap<String, OwnedValue>>, Error>;

    fn update2(
        &self,
        settings: HashMap<String, HashMap<String, OwnedValue>>,
        flags: u32,
        args: HashMap<String, ZValue<'_>>,
    ) -> Result<HashMap<String, OwnedValue>, Error>;

    #[zbus(signal)]
    fn updated(&self) -> Result<(), Error>;
}

#[proxy(
    interface = "org.freedesktop.NetworkManager.IP4Config",
    default_service = "org.freedesktop.NetworkManager"
)]
trait Ip4Config {
    #[zbus(property)]
    fn address_data(&self) -> Result<Vec<AddressData>, Error>;

    #[zbus(property)]
    fn route_data(&self) -> Result<Vec<RouteData>, Error>;

    #[zbus(property)]
    fn gateway(&self) -> Result<String, Error>;

    #[zbus(property)]
    fn nameserver_data(&self) -> Result<Vec<NameserverData>, Error>;
}

#[proxy(
    interface = "org.freedesktop.NetworkManager.IP6Config",
    default_service = "org.freedesktop.NetworkManager"
)]
trait Ip6Config {
    #[zbus(property)]
    fn address_data(&self) -> Result<Vec<AddressData>, Error>;

    #[zbus(property)]
    fn gateway(&self) -> Result<String, Error>;

    #[zbus(property)]
    fn nameserver_data(&self) -> Result<Vec<NameserverData>, Error>;
}

#[derive(Clone, Debug, DeserializeDict, ZValue, ZType)]
#[zvariant(signature = "dict")]
struct AddressData {
    address: String,
    prefix: u32,
}
impl TryFrom<AddressData> for IpNet {
    type Error = Error;
    fn try_from(value: AddressData) -> Result<Self, Self::Error> {
        IpNet::new(value.address.parse()?, value.prefix as u8).with_kind(ErrorKind::ParseNetAddress)
    }
}

#[derive(Clone, Debug, DeserializeDict, ZValue, ZType)]
#[zvariant(signature = "dict")]
struct RouteData {
    dest: String,
    prefix: u32,
    table: Option<u32>,
}

#[derive(Clone, Debug, DeserializeDict, ZValue, ZType)]
#[zvariant(signature = "dict")]
struct NameserverData {
    address: String,
}

#[proxy(
    interface = "org.freedesktop.NetworkManager.DHCP4Config",
    default_service = "org.freedesktop.NetworkManager"
)]
trait Dhcp4Config {
    #[zbus(property)]
    fn options(&self) -> Result<Dhcp4Options, Error>;
}

#[derive(Clone, Debug, DeserializeDict, ZType)]
#[zvariant(signature = "dict")]
struct Dhcp4Options {
    ntp_servers: Option<String>,
    domain_name_servers: Option<String>,
}
impl TryFrom<OwnedValue> for Dhcp4Options {
    type Error = zbus::Error;
    fn try_from(value: OwnedValue) -> Result<Self, Self::Error> {
        let dict = value.downcast_ref::<Dict>()?;
        Ok(Self {
            ntp_servers: dict.get::<_, String>(&zbus::zvariant::Str::from_static("ntp_servers"))?,
            domain_name_servers: dict
                .get::<_, String>(&zbus::zvariant::Str::from_static("domain_name_servers"))?,
        })
    }
}

mod device {
    use zbus::proxy;
    use zbus::zvariant::OwnedObjectPath;

    use crate::prelude::*;

    #[proxy(
        interface = "org.freedesktop.NetworkManager.Device",
        default_service = "org.freedesktop.NetworkManager"
    )]
    pub trait Device {
        fn delete(&self) -> Result<(), Error>;

        #[zbus(property)]
        fn ip_interface(&self) -> Result<String, Error>;

        #[zbus(property)]
        fn managed(&self) -> Result<bool, Error>;

        #[zbus(property)]
        fn active_connection(&self) -> Result<OwnedObjectPath, Error>;

        #[zbus(property)]
        fn ip4_config(&self) -> Result<OwnedObjectPath, Error>;

        #[zbus(property)]
        fn ip6_config(&self) -> Result<OwnedObjectPath, Error>;

        #[zbus(property, name = "State")]
        fn _state(&self) -> Result<u32, Error>;

        #[zbus(property)]
        fn device_type(&self) -> Result<u32, Error>;

        #[zbus(signal)]
        fn state_changed(&self) -> Result<(), Error>;
    }
}

trait StubStream<'a> {
    fn stub(self) -> impl Stream<Item = Result<(), Error>> + 'a;
}
impl<'a, T> StubStream<'a> for PropertyStream<'a, T>
where
    T: Unpin + TryFrom<OwnedValue> + std::fmt::Debug + 'a,
    T::Error: Into<zbus::Error>,
{
    fn stub(self) -> impl Stream<Item = Result<(), Error>> + 'a {
        StreamExt::then(self, |d| async move {
            PropertyChanged::get(&d).await.map(|_| ())
        })
        .map_err(Error::from)
    }
}
impl<'a> StubStream<'a> for SignalStream<'a> {
    fn stub(self) -> impl Stream<Item = Result<(), Error>> + 'a {
        self.map(|_| Ok(()))
    }
}

#[instrument(skip_all)]
async fn watcher(
    watch_ip_info: Watch<OrdMap<GatewayId, NetworkInterfaceInfo>>,
    watch_activation: Watch<BTreeMap<GatewayId, bool>>,
    db: Option<TypedPatchDb<Database>>,
) {
    loop {
        let res: Result<(), Error> = async {
            Command::new("systemctl")
                .arg("start")
                .arg("NetworkManager")
                .invoke(ErrorKind::Network)
                .await?;

            let connection = Connection::system().await?;

            let netman_proxy = NetworkManagerProxy::new(&connection).await?;

            let mut until = Until::new()
                .with_stream(netman_proxy.receive_all_devices_changed().await.stub())
                .with_stream(
                    netman_proxy
                        .receive_device_added()
                        .await?
                        .into_inner()
                        .stub(),
                )
                .with_stream(
                    netman_proxy
                        .receive_device_removed()
                        .await?
                        .into_inner()
                        .stub(),
                )
                .with_stream(
                    netman_proxy
                        .receive_state_changed()
                        .await?
                        .into_inner()
                        .stub(),
                );

            loop {
                until
                    .run(async {
                        let devices = netman_proxy.all_devices().await?;
                        ensure_code!(
                            !devices.is_empty(),
                            ErrorKind::Network,
                            "{}",
                            t!("net.gateway.no-devices-returned")
                        );
                        let mut ifaces = BTreeSet::new();
                        let mut jobs = Vec::new();
                        for device in devices {
                            use futures::future::Either;

                            let device_proxy =
                                device::DeviceProxy::new(&connection, device.clone()).await?;
                            let iface = InternedString::intern(device_proxy.ip_interface().await?);
                            if iface.is_empty() {
                                continue;
                            }
                            let iface: GatewayId = iface.into();
                            if watch_activation.peek(|a| a.contains_key(&iface)) {
                                jobs.push(Either::Left(watch_activated(
                                    &connection,
                                    device_proxy.clone(),
                                    iface.clone(),
                                    &watch_activation,
                                )));
                            }

                            jobs.push(Either::Right(watch_ip(
                                &connection,
                                device_proxy.clone(),
                                iface.clone(),
                                &watch_ip_info,
                                db.as_ref(),
                            )));
                            ifaces.insert(iface);
                        }

                        watch_ip_info.send_if_modified(|m| {
                            let mut changed = false;
                            for (iface, info) in OrdMapIterMut::from(m) {
                                if !ifaces.contains(iface) {
                                    info.ip_info = None;
                                    changed = true;
                                }
                            }
                            changed
                        });
                        for result in futures::future::join_all(jobs).await {
                            result.log_err();
                        }

                        Ok::<_, Error>(())
                    })
                    .await?;
            }
        }
        .await;
        if let Err(e) = res {
            tracing::error!("{e}");
            tracing::debug!("{e:?}");
            tokio::time::sleep(Duration::from_secs(1)).await;
        }
    }
}

async fn get_wan_ipv4(iface: &str, base_url: &Url) -> Result<Option<Ipv4Addr>, Error> {
    let client = reqwest::Client::builder();
    #[cfg(target_os = "linux")]
    let client = client
        .interface(iface)
        .local_address(IpAddr::V4(Ipv4Addr::UNSPECIFIED));
    let url = base_url.join("/ip").with_kind(ErrorKind::ParseUrl)?;
    let text = client
        .build()?
        .get(url)
        .timeout(Duration::from_secs(5))
        .send()
        .await?
        .error_for_status()?
        .text()
        .await?;
    let trimmed = text.trim();
    if trimmed.is_empty() {
        return Ok(None);
    }
    Ok(Some(trimmed.parse()?))
}

struct PolicyRoutingCleanup {
    table_id: u32,
    iface: String,
}
impl Drop for PolicyRoutingCleanup {
    fn drop(&mut self) {
        let table_str = self.table_id.to_string();
        let iface = std::mem::take(&mut self.iface);
        tokio::spawn(async move {
            Command::new("ip")
                .arg("rule")
                .arg("del")
                .arg("fwmark")
                .arg(&table_str)
                .arg("lookup")
                .arg(&table_str)
                .arg("priority")
                .arg("50")
                .invoke(ErrorKind::Network)
                .await
                .ok();
            Command::new("ip")
                .arg("route")
                .arg("flush")
                .arg("table")
                .arg(&table_str)
                .invoke(ErrorKind::Network)
                .await
                .ok();
            Command::new("iptables")
                .arg("-t")
                .arg("mangle")
                .arg("-D")
                .arg("PREROUTING")
                .arg("-i")
                .arg(&iface)
                .arg("-m")
                .arg("conntrack")
                .arg("--ctstate")
                .arg("NEW")
                .arg("-j")
                .arg("CONNMARK")
                .arg("--set-mark")
                .arg(&table_str)
                .invoke(ErrorKind::Network)
                .await
                .ok();
        });
    }
}

#[instrument(skip(connection, device_proxy, write_to, db))]
async fn watch_ip(
    connection: &Connection,
    device_proxy: device::DeviceProxy<'_>,
    iface: GatewayId,
    write_to: &Watch<OrdMap<GatewayId, NetworkInterfaceInfo>>,
    db: Option<&TypedPatchDb<Database>>,
) -> Result<(), Error> {
    let mut until = Until::new()
        .with_stream(
            device_proxy
                .receive_active_connection_changed()
                .await
                .stub(),
        )
        .with_stream(
            device_proxy
                .receive_state_changed()
                .await?
                .into_inner()
                .stub(),
        )
        .with_stream(device_proxy.receive_ip4_config_changed().await.stub())
        .with_stream(device_proxy.receive_ip6_config_changed().await.stub())
        .with_async_fn(|| {
            async {
                tokio::time::sleep(Duration::from_secs(600)).await;
                Ok(())
            }
            .fuse()
        });

    let mut echoip_ratelimit_state: BTreeMap<Url, Instant> = BTreeMap::new();

    loop {
        until
            .run(async {
                let ip4_config = device_proxy.ip4_config().await?;
                let ip6_config = device_proxy.ip6_config().await?;

                let managed = device_proxy.managed().await?;
                if !managed {
                    return Ok(());
                }
                let dac = device_proxy.active_connection().await?;
                if &*dac == "/" {
                    return Ok(());
                }

                let active_connection_proxy =
                    active_connection::ActiveConnectionProxy::new(&connection, dac).await?;

                let settings_proxy = ConnectionSettingsProxy::new(
                    &connection,
                    active_connection_proxy.connection().await?,
                )
                .await?;

                let mut until = Until::new()
                    .with_stream(
                        active_connection_proxy
                            .receive_state_changed()
                            .await?
                            .into_inner()
                            .stub(),
                    )
                    .with_stream(
                        active_connection_proxy
                            .receive_dhcp4_config_changed()
                            .await
                            .stub(),
                    )
                    .with_stream(settings_proxy.receive_updated().await?.into_inner().stub());

                loop {
                    until
                        .run(async {
                            let device_type = match device_proxy.device_type().await? {
                                1 => Some(NetworkInterfaceType::Ethernet),
                                2 => Some(NetworkInterfaceType::Wireless),
                                13 => Some(NetworkInterfaceType::Bridge),
                                29 => Some(NetworkInterfaceType::Wireguard),
                                32 => Some(NetworkInterfaceType::Loopback),
                                _ => None,
                            };

                            let name = InternedString::from(active_connection_proxy.id().await?);

                            let dhcp4_config = active_connection_proxy.dhcp4_config().await?;
                            let ip4_proxy =
                                Ip4ConfigProxy::new(&connection, ip4_config.clone()).await?;
                            let ip6_proxy =
                                Ip6ConfigProxy::new(&connection, ip6_config.clone()).await?;
                            let mut until = Until::new()
                                .with_stream(ip4_proxy.receive_address_data_changed().await.stub())
                                .with_stream(ip4_proxy.receive_route_data_changed().await.stub())
                                .with_stream(ip4_proxy.receive_gateway_changed().await.stub())
                                .with_stream(
                                    ip4_proxy.receive_nameserver_data_changed().await.stub(),
                                )
                                .with_stream(ip6_proxy.receive_address_data_changed().await.stub())
                                .with_stream(ip6_proxy.receive_gateway_changed().await.stub())
                                .with_stream(
                                    ip6_proxy.receive_nameserver_data_changed().await.stub(),
                                );

                            let dhcp4_proxy = if &*dhcp4_config != "/" {
                                let dhcp4_proxy =
                                    Dhcp4ConfigProxy::new(&connection, dhcp4_config).await?;
                                until = until.with_stream(
                                    dhcp4_proxy.receive_options_changed().await.stub(),
                                );
                                Some(dhcp4_proxy)
                            } else {
                                None
                            };

                            // Policy routing: track per-interface table for cleanup on scope exit
                            let policy_table_id = if !matches!(
                                device_type,
                                Some(NetworkInterfaceType::Bridge | NetworkInterfaceType::Loopback)
                            ) {
                                if_nametoindex(iface.as_str())
                                    .map(|idx| 1000 + idx)
                                    .log_err()
                            } else {
                                None
                            };
                            let policy_guard: Option<PolicyRoutingCleanup> =
                                policy_table_id.map(|t| PolicyRoutingCleanup {
                                    table_id: t,
                                    iface: iface.as_str().to_owned(),
                                });

                            loop {
                                until
                                    .run(poll_ip_info(
                                        &ip4_proxy,
                                        &ip6_proxy,
                                        &dhcp4_proxy,
                                        &policy_guard,
                                        &iface,
                                        &mut echoip_ratelimit_state,
                                        db,
                                        write_to,
                                        device_type,
                                        &name,
                                    ))
                                    .await?;
                            }
                        })
                        .await?;
                }
            })
            .await?;
    }
}

async fn apply_policy_routing(
    guard: &PolicyRoutingCleanup,
    iface: &GatewayId,
    lan_ip: &OrdSet<IpAddr>,
) -> Result<(), Error> {
    let table_id = guard.table_id;
    let table_str = table_id.to_string();

    let ipv4_gateway: Option<Ipv4Addr> = lan_ip
        .iter()
        .find_map(|ip| match ip {
            IpAddr::V4(v4) => Some(v4),
            _ => None,
        })
        .copied();

    // Rebuild per-interface routing table using `ip route replace` to avoid
    // the connectivity gap that a flush+add cycle would create.  We replace
    // every desired route in-place (each replace is atomic in the kernel),
    // then delete any stale routes that are no longer in the desired set.

    // Collect the set of desired non-default route prefixes (the first
    // whitespace-delimited token of each `ip route show` line is the
    // destination prefix, e.g. "192.168.1.0/24" or "10.0.0.0/8").
    let mut desired_prefixes = BTreeSet::<String>::new();

    if let Ok(main_routes) = Command::new("ip")
        .arg("route")
        .arg("show")
        .arg("table")
        .arg("main")
        .invoke(ErrorKind::Network)
        .await
        .and_then(|b| String::from_utf8(b).with_kind(ErrorKind::Utf8))
    {
        for line in main_routes.lines() {
            let line = line.trim();
            if line.is_empty() || line.starts_with("default") {
                continue;
            }
            if let Some(prefix) = line.split_whitespace().next() {
                desired_prefixes.insert(prefix.to_owned());
            }
            let mut cmd = Command::new("ip");
            cmd.arg("route").arg("replace");
            for part in line.split_whitespace() {
                // Skip status flags that appear in route output but
                // are not valid for `ip route replace`.
                if part == "linkdown" || part == "dead" {
                    continue;
                }
                cmd.arg(part);
            }
            cmd.arg("table").arg(&table_str);
            if let Err(e) = cmd.invoke(ErrorKind::Network).await {
                // Transient interfaces (podman, wg-quick, etc.) may
                // vanish between reading the main table and replaying
                // the route — demote to debug to avoid log noise.
                if e.source.to_string().contains("No such file or directory") {
                    tracing::trace!("ip route replace (transient device): {e}");
                } else {
                    tracing::error!("{e}");
                    tracing::debug!("{e:?}");
                }
            }
        }
    }

    // Replace the default route via this interface's gateway.
    {
        let mut cmd = Command::new("ip");
        cmd.arg("route").arg("replace").arg("default");
        if let Some(gw) = ipv4_gateway {
            cmd.arg("via").arg(gw.to_string());
        }
        cmd.arg("dev")
            .arg(iface.as_str())
            .arg("table")
            .arg(&table_str);
        if ipv4_gateway.is_none() {
            cmd.arg("scope").arg("link");
        }
        cmd.invoke(ErrorKind::Network).await.log_err();
    }

    // Delete stale routes: any non-default route in the per-interface table
    // whose prefix is not in the desired set.
    if let Ok(existing_routes) = Command::new("ip")
        .arg("route")
        .arg("show")
        .arg("table")
        .arg(&table_str)
        .invoke(ErrorKind::Network)
        .await
        .and_then(|b| String::from_utf8(b).with_kind(ErrorKind::Utf8))
    {
        for line in existing_routes.lines() {
            let line = line.trim();
            if line.is_empty() || line.starts_with("default") {
                continue;
            }
            let Some(prefix) = line.split_whitespace().next() else {
                continue;
            };
            if desired_prefixes.contains(prefix) {
                continue;
            }
            Command::new("ip")
                .arg("route")
                .arg("del")
                .arg(prefix)
                .arg("table")
                .arg(&table_str)
                .invoke(ErrorKind::Network)
                .await
                .log_err();
        }
    }

    // Ensure global CONNMARK restore rules in mangle PREROUTING (forwarded
    // packets) and OUTPUT (locally-generated replies). Both are needed:
    // PREROUTING handles DNAT-forwarded traffic, OUTPUT handles replies from
    // locally-bound listeners (e.g. vhost). The `-m mark --mark 0` condition
    // ensures we only restore when the packet has no existing fwmark,
    // preserving marks set by WireGuard on encapsulation packets.
    for chain in ["PREROUTING", "OUTPUT"] {
        if Command::new("iptables")
            .arg("-t")
            .arg("mangle")
            .arg("-C")
            .arg(chain)
            .arg("-m")
            .arg("mark")
            .arg("--mark")
            .arg("0")
            .arg("-j")
            .arg("CONNMARK")
            .arg("--restore-mark")
            .invoke(ErrorKind::Network)
            .await
            .is_err()
        {
            Command::new("iptables")
                .arg("-t")
                .arg("mangle")
                .arg("-I")
                .arg(chain)
                .arg("1")
                .arg("-m")
                .arg("mark")
                .arg("--mark")
                .arg("0")
                .arg("-j")
                .arg("CONNMARK")
                .arg("--restore-mark")
                .invoke(ErrorKind::Network)
                .await
                .log_err();
        }
    }

    // Mark NEW connections arriving on this interface with its routing
    // table ID via conntrack mark
    if Command::new("iptables")
        .arg("-t")
        .arg("mangle")
        .arg("-C")
        .arg("PREROUTING")
        .arg("-i")
        .arg(iface.as_str())
        .arg("-m")
        .arg("conntrack")
        .arg("--ctstate")
        .arg("NEW")
        .arg("-j")
        .arg("CONNMARK")
        .arg("--set-mark")
        .arg(&table_str)
        .invoke(ErrorKind::Network)
        .await
        .is_err()
    {
        Command::new("iptables")
            .arg("-t")
            .arg("mangle")
            .arg("-A")
            .arg("PREROUTING")
            .arg("-i")
            .arg(iface.as_str())
            .arg("-m")
            .arg("conntrack")
            .arg("--ctstate")
            .arg("NEW")
            .arg("-j")
            .arg("CONNMARK")
            .arg("--set-mark")
            .arg(&table_str)
            .invoke(ErrorKind::Network)
            .await
            .log_err();
    }

    // Ensure fwmark-based ip rule for this interface's table
    let rules_output = String::from_utf8(
        Command::new("ip")
            .arg("rule")
            .arg("list")
            .invoke(ErrorKind::Network)
            .await?,
    )?;
    if !rules_output
        .lines()
        .any(|l| l.contains("fwmark") && l.contains(&format!("lookup {table_id}")))
    {
        Command::new("ip")
            .arg("rule")
            .arg("add")
            .arg("fwmark")
            .arg(&table_str)
            .arg("lookup")
            .arg(&table_str)
            .arg("priority")
            .arg("50")
            .invoke(ErrorKind::Network)
            .await
            .log_err();
    }

    Ok(())
}

async fn poll_ip_info(
    ip4_proxy: &Ip4ConfigProxy<'_>,
    ip6_proxy: &Ip6ConfigProxy<'_>,
    dhcp4_proxy: &Option<Dhcp4ConfigProxy<'_>>,
    policy_guard: &Option<PolicyRoutingCleanup>,
    iface: &GatewayId,
    echoip_ratelimit_state: &mut BTreeMap<Url, Instant>,
    db: Option<&TypedPatchDb<Database>>,
    write_to: &Watch<OrdMap<GatewayId, NetworkInterfaceInfo>>,
    device_type: Option<NetworkInterfaceType>,
    name: &InternedString,
) -> Result<(), Error> {
    let addresses = ip4_proxy
        .address_data()
        .await?
        .into_iter()
        .chain(ip6_proxy.address_data().await?)
        .collect_vec();
    let lan_ip: OrdSet<IpAddr> = [
        Some(ip4_proxy.gateway().await?)
            .filter(|g| !g.is_empty())
            .and_then(|g| g.parse::<IpAddr>().log_err()),
        Some(ip6_proxy.gateway().await?)
            .filter(|g| !g.is_empty())
            .and_then(|g| g.parse::<IpAddr>().log_err()),
    ]
    .into_iter()
    .filter_map(|a| a)
    .collect();
    let mut ntp_servers = OrdSet::new();
    let mut dns_servers = OrdSet::new();
    if let Some(dhcp4_proxy) = dhcp4_proxy {
        let dhcp = dhcp4_proxy.options().await?;
        if let Some(ntp) = dhcp.ntp_servers {
            ntp_servers.extend(ntp.split_whitespace().map(InternedString::intern));
        }
        if let Some(dns) = dhcp.domain_name_servers {
            dns_servers.extend(
                dns.split_ascii_whitespace()
                    .filter_map(|s| s.parse::<IpAddr>().log_err())
                    .collect::<Vec<_>>(),
            );
        }
    }
    let scope_id = if_nametoindex(iface.as_str()).with_kind(ErrorKind::Network)?;
    let subnets: OrdSet<IpNet> = addresses.into_iter().map(IpNet::try_from).try_collect()?;

    // Policy routing: ensure replies exit the same interface they arrived on,
    // eliminating the need for MASQUERADE.
    if let Some(guard) = policy_guard {
        apply_policy_routing(guard, iface, &lan_ip).await?;
    }

    let echoip_urls = if let Some(db) = db {
        db.peek()
            .await
            .as_public()
            .as_server_info()
            .as_echoip_urls()
            .de()
            .unwrap_or_else(|_| crate::db::model::public::default_echoip_urls())
    } else {
        crate::db::model::public::default_echoip_urls()
    };
    let mut wan_ip = None;
    let mut err = None;
    for echoip_url in echoip_urls {
        if echoip_ratelimit_state
            .get(&echoip_url)
            .map_or(true, |i| i.elapsed() > Duration::from_secs(300))
            && !subnets.is_empty()
            && !matches!(
                device_type,
                Some(NetworkInterfaceType::Bridge | NetworkInterfaceType::Loopback)
            )
        {
            match get_wan_ipv4(iface.as_str(), &echoip_url).await {
                Ok(a) => {
                    wan_ip = a;
                }
                Err(e) => {
                    err = Some(e);
                }
            };
            echoip_ratelimit_state.insert(echoip_url, Instant::now());
            if wan_ip.is_some() {
                break;
            }
        };
    }
    if wan_ip.is_none()
        && let Some(e) = err
    {
        tracing::error!(
            "{}",
            t!(
                "net.gateway.failed-to-determine-wan-ip",
                iface = iface.to_string(),
                error = e.to_string()
            )
        );
        tracing::debug!("{e:?}");
    }
    let mut ip_info = IpInfo {
        name: name.clone(),
        scope_id,
        device_type,
        subnets,
        lan_ip,
        wan_ip,
        ntp_servers,
        dns_servers,
    };

    write_to.send_if_modified(|m: &mut OrdMap<GatewayId, NetworkInterfaceInfo>| {
        let (name, secure, gateway_type, prev_wan_ip) =
            m.get(iface).map_or((None, None, None, None), |i| {
                (
                    i.name.clone(),
                    i.secure,
                    i.gateway_type,
                    i.ip_info.as_ref().and_then(|i| i.wan_ip),
                )
            });
        ip_info.wan_ip = ip_info.wan_ip.or(prev_wan_ip);
        let ip_info = Arc::new(ip_info);
        m.insert(
            iface.clone(),
            NetworkInterfaceInfo {
                name,
                secure,
                ip_info: Some(ip_info.clone()),
                gateway_type,
            },
        )
        .filter(|old| &old.ip_info == &Some(ip_info))
        .is_none()
    });

    Ok(())
}

#[instrument(skip(_connection, device_proxy, watch_activation))]
async fn watch_activated(
    _connection: &Connection,
    device_proxy: device::DeviceProxy<'_>,
    iface: GatewayId,
    watch_activation: &Watch<BTreeMap<GatewayId, bool>>,
) -> Result<(), Error> {
    let mut until = Until::new()
        .with_stream(
            device_proxy
                .receive_active_connection_changed()
                .await
                .stub(),
        )
        .with_stream(
            device_proxy
                .receive_state_changed()
                .await?
                .into_inner()
                .stub(),
        );

    loop {
        until
            .run(async {
                let activated = device_proxy._state().await? == 100;
                watch_activation.send_if_modified(|a| {
                    a.get_mut(&iface)
                        .map_or(false, |a| std::mem::replace(a, activated) != activated)
                });
                Ok(())
            })
            .await?;
    }
}

pub struct NetworkInterfaceWatcher {
    activated: Watch<BTreeMap<GatewayId, bool>>,
    ip_info: Watch<OrdMap<GatewayId, NetworkInterfaceInfo>>,
    _watcher: NonDetachingJoinHandle<()>,
}
impl NetworkInterfaceWatcher {
    pub fn new(
        seed: impl Future<Output = OrdMap<GatewayId, NetworkInterfaceInfo>> + Send + Sync + 'static,
        watch_activated: impl IntoIterator<Item = GatewayId>,
        db: TypedPatchDb<Database>,
    ) -> Self {
        let ip_info = Watch::new(OrdMap::new());
        let activated = Watch::new(watch_activated.into_iter().map(|k| (k, false)).collect());
        Self {
            activated: activated.clone(),
            ip_info: ip_info.clone(),
            _watcher: tokio::spawn(async move {
                let seed = seed.await;
                if !seed.is_empty() {
                    ip_info.send_replace(seed);
                }
                watcher(ip_info, activated, Some(db)).await
            })
            .into(),
        }
    }

    pub fn activated(&self) -> Watch<BTreeMap<GatewayId, bool>> {
        self.activated.clone_unseen()
    }

    pub fn wait_for_activated(
        &self,
        interface: GatewayId,
    ) -> impl Future<Output = ()> + Send + Sync + 'static {
        let mut activated = self.activated();
        async move {
            activated
                .wait_for(|a| a.get(&interface).copied().unwrap_or(false))
                .await;
        }
    }

    pub fn subscribe(&self) -> Watch<OrdMap<GatewayId, NetworkInterfaceInfo>> {
        self.ip_info.clone_unseen()
    }

    pub fn ip_info(&self) -> OrdMap<GatewayId, NetworkInterfaceInfo> {
        self.ip_info.read()
    }
}

pub struct NetworkInterfaceController {
    db: TypedPatchDb<Database>,
    pub watcher: NetworkInterfaceWatcher,
    _sync: NonDetachingJoinHandle<()>,
}
impl NetworkInterfaceController {
    async fn sync(
        db: &TypedPatchDb<Database>,
        info: &OrdMap<GatewayId, NetworkInterfaceInfo>,
    ) -> Result<(), Error> {
        tracing::debug!("syncronizing {info:?} to db");

        let wifi_iface = find_wifi_iface()
            .await
            .ok()
            .and_then(|a| a)
            .map(InternedString::from)
            .map(GatewayId::from);

        db.mutate(|db| {
            let network = db.as_public_mut().as_server_info_mut().as_network_mut();
            network.as_gateways_mut().ser(info)?;
            network.as_wifi_mut().as_interface_mut().ser(&wifi_iface)?;
            let hostname = crate::hostname::ServerHostname::load(db.as_public().as_server_info())?;
            let ports = db.as_private().as_available_ports().de()?;
            for host in all_hosts(db) {
                host?.update_addresses(&hostname, info, &ports)?;
            }
            Ok(())
        })
        .await
        .result?;

        let ntp: BTreeSet<_> = info
            .values()
            .filter_map(|i| i.ip_info.as_ref())
            .flat_map(|i| &i.ntp_servers)
            .cloned()
            .collect();
        let prev_ntp = tokio_stream::wrappers::LinesStream::new(
            BufReader::new(open_file("/etc/systemd/timesyncd.conf").await?).lines(),
        )
        .try_filter_map(|l| async move {
            Ok(l.strip_prefix("NTP=").map(|s| {
                s.split_whitespace()
                    .map(InternedString::intern)
                    .collect::<BTreeSet<_>>()
            }))
        })
        .boxed()
        .try_next()
        .await?
        .unwrap_or_default();
        if ntp != prev_ntp {
            // sed -i '/\(^\|#\)NTP=/c\NTP='"${servers}" /etc/systemd/timesyncd.conf
            Command::new("sed")
                .arg("-i")
                .arg(
                    [r#"/\(^\|#\)NTP=/c\NTP="#]
                        .into_iter()
                        .chain(Itertools::intersperse(
                            {
                                fn to_str(ntp: &InternedString) -> &str {
                                    &*ntp
                                }
                                ntp.iter().map(to_str)
                            },
                            " ",
                        ))
                        .join(""),
                )
                .arg("/etc/systemd/timesyncd.conf")
                .invoke(ErrorKind::Filesystem)
                .await?;
            Command::new("systemctl")
                .arg("restart")
                .arg("systemd-timesyncd")
                .invoke(ErrorKind::Systemd)
                .await?;
        }

        Ok(())
    }

    async fn apply_default_outbound(
        default_outbound: &Option<GatewayId>,
        ip_info: &OrdMap<GatewayId, NetworkInterfaceInfo>,
    ) {
        // 1. Snapshot existing rules at priorities 74 and 75.
        //    Priority 74: fwmark-based exemptions (WireGuard encap packets)
        //    Priority 75: catch-all routing to the chosen gateway's table
        let (existing_74, existing_75) = match async {
            let output = String::from_utf8(
                Command::new("ip")
                    .arg("rule")
                    .arg("show")
                    .invoke(ErrorKind::Network)
                    .await?,
            )?;
            let mut fwmarks_74 = BTreeSet::<u32>::new();
            let mut tables_75 = BTreeSet::<u32>::new();
            for line in output.lines() {
                let line = line.trim();
                if let Some(rest) = line.strip_prefix("74:") {
                    if let Some(pos) = rest.find("fwmark ") {
                        let after = &rest[pos + 7..];
                        let token = after.split_whitespace().next().unwrap_or("");
                        if let Ok(v) =
                            u32::from_str_radix(token.strip_prefix("0x").unwrap_or(token), 16)
                        {
                            fwmarks_74.insert(v);
                        }
                    }
                } else if let Some(rest) = line.strip_prefix("75:") {
                    if let Some(pos) = rest.find("lookup ") {
                        let after = &rest[pos + 7..];
                        let token = after.split_whitespace().next().unwrap_or("");
                        if let Ok(v) = token.parse::<u32>() {
                            tables_75.insert(v);
                        }
                    }
                }
            }
            Ok::<_, Error>((fwmarks_74, tables_75))
        }
        .await
        {
            Ok(v) => v,
            Err(e) => {
                tracing::error!("failed to snapshot outbound rules: {e}");
                (BTreeSet::new(), BTreeSet::new())
            }
        };

        // 2. Compute desired rules
        let mut desired_74 = BTreeSet::<u32>::new();
        let mut desired_75 = BTreeSet::<u32>::new();

        if let Some(gw_id) = default_outbound {
            let connected = ip_info
                .get(gw_id)
                .map_or(false, |info| info.ip_info.is_some());
            if !connected {
                if ip_info.contains_key(gw_id) {
                    tracing::warn!("default outbound gateway {gw_id} is not connected");
                } else {
                    tracing::warn!("default outbound gateway {gw_id} not found in ip_info");
                }
            } else {
                match if_nametoindex(gw_id.as_str()) {
                    Ok(idx) => {
                        let table_id = 1000 + idx;
                        desired_75.insert(table_id);

                        // Exempt ALL active WireGuard interfaces' encapsulation packets.
                        // Our priority-75 catch-all would otherwise swallow their encap
                        // traffic before NM's fwmark rules at priority 31610 can route
                        // it correctly.
                        for (iface_id, iface_info) in ip_info {
                            let Some(ref ip) = iface_info.ip_info else {
                                continue;
                            };
                            if ip.device_type != Some(NetworkInterfaceType::Wireguard) {
                                continue;
                            }
                            match Command::new("wg")
                                .arg("show")
                                .arg(iface_id.as_str())
                                .arg("fwmark")
                                .invoke(ErrorKind::Network)
                                .await
                            {
                                Ok(output) => {
                                    let fwmark_hex =
                                        String::from_utf8_lossy(&output).trim().to_owned();
                                    if fwmark_hex.is_empty() || fwmark_hex == "off" {
                                        continue;
                                    }
                                    match u32::from_str_radix(
                                        fwmark_hex.strip_prefix("0x").unwrap_or(&fwmark_hex),
                                        16,
                                    ) {
                                        Ok(v) => {
                                            desired_74.insert(v);
                                        }
                                        Err(e) => {
                                            tracing::error!(
                                                "failed to parse WireGuard fwmark '{fwmark_hex}' for {iface_id}: {e}"
                                            );
                                        }
                                    }
                                }
                                Err(e) => {
                                    tracing::error!(
                                        "failed to read WireGuard fwmark for {iface_id}: {e}"
                                    );
                                }
                            }
                        }
                    }
                    Err(e) => {
                        tracing::error!("failed to get ifindex for {gw_id}: {e}");
                    }
                }
            }
        }

        // 3. Add rules in desired set but not in existing set
        for fwmark in desired_74.difference(&existing_74) {
            Command::new("ip")
                .arg("rule")
                .arg("add")
                .arg("fwmark")
                .arg(fwmark.to_string())
                .arg("lookup")
                .arg("main")
                .arg("priority")
                .arg("74")
                .invoke(ErrorKind::Network)
                .await
                .log_err();
        }
        for table in desired_75.difference(&existing_75) {
            Command::new("ip")
                .arg("rule")
                .arg("add")
                .arg("table")
                .arg(table.to_string())
                .arg("priority")
                .arg("75")
                .invoke(ErrorKind::Network)
                .await
                .log_err();
        }

        // 4. Delete rules in existing set but not in desired set
        for fwmark in existing_74.difference(&desired_74) {
            Command::new("ip")
                .arg("rule")
                .arg("del")
                .arg("fwmark")
                .arg(fwmark.to_string())
                .arg("lookup")
                .arg("main")
                .arg("priority")
                .arg("74")
                .invoke(ErrorKind::Network)
                .await
                .log_err();
        }
        for table in existing_75.difference(&desired_75) {
            Command::new("ip")
                .arg("rule")
                .arg("del")
                .arg("table")
                .arg(table.to_string())
                .arg("priority")
                .arg("75")
                .invoke(ErrorKind::Network)
                .await
                .log_err();
        }
    }

    pub fn new(db: TypedPatchDb<Database>) -> Self {
        let (seeded_send, seeded) = oneshot::channel();
        let watcher = NetworkInterfaceWatcher::new(
            {
                let db = db.clone();
                async move {
                    let info = match db
                        .peek()
                        .await
                        .as_public()
                        .as_server_info()
                        .as_network()
                        .as_gateways()
                        .de()
                    {
                        Ok(mut info) => {
                            for (_, info) in OrdMapIterMut::from(&mut info) {
                                info.ip_info = None;
                            }
                            info
                        }
                        Err(e) => {
                            tracing::error!(
                                "{}",
                                t!(
                                    "net.gateway.error-loading-interface-info",
                                    error = e.to_string()
                                )
                            );
                            tracing::debug!("{e:?}");
                            OrdMap::new()
                        }
                    };
                    let _ = seeded_send.send(info.clone());
                    info
                }
            },
            [InternedString::from_static(START9_BRIDGE_IFACE).into()],
            db.clone(),
        );
        let mut ip_info_watch = watcher.subscribe();
        ip_info_watch.mark_seen();
        Self {
            db: db.clone(),
            watcher,
            _sync: tokio::spawn(async move {
                let res: Result<(), Error> = async {
                    let mut ip_info = seeded.await.ok();
                    let mut outbound_sub = db
                        .subscribe(
                            "/public/serverInfo/network/defaultOutbound"
                                .parse::<JsonPointer<_, _>>()
                                .unwrap(),
                        )
                        .await;
                    loop {
                        if let Err(e) = async {
                            if let Some(ref ip_info) = ip_info {
                                Self::sync(&db, ip_info).boxed().await?;
                            }
                            if let Some(ref ip_info) = ip_info {
                                let default_outbound: Option<GatewayId> = db
                                    .peek()
                                    .await
                                    .as_public()
                                    .as_server_info()
                                    .as_network()
                                    .as_default_outbound()
                                    .de()?;
                                Self::apply_default_outbound(&default_outbound, ip_info).await;
                            }

                            Ok::<_, Error>(())
                        }
                        .await
                        {
                            tracing::error!(
                                "{}",
                                t!("net.gateway.error-syncing-ip-info", error = e.to_string())
                            );
                            tracing::debug!("{e:?}");
                        }

                        tokio::select! {
                            _ = ip_info_watch.changed() => {
                                ip_info = Some(ip_info_watch.read());
                            }
                            _ = outbound_sub.recv() => {}
                        }
                    }
                }
                .await;
                if let Err(e) = res {
                    tracing::error!(
                        "{}",
                        t!("net.gateway.error-syncing-ip-info", error = e.to_string())
                    );
                    tracing::debug!("{e:?}");
                }
            })
            .into(),
        }
    }

    pub async fn forget(&self, interface: &GatewayId) -> Result<(), Error> {
        let mut sub = self
            .db
            .subscribe(
                "/public/serverInfo/network/gateways"
                    .parse::<JsonPointer<_, _>>()
                    .with_kind(ErrorKind::Database)?,
            )
            .await;
        let mut err = None;
        let changed = self.watcher.ip_info.send_if_modified(|ip_info| {
            if ip_info
                .get(interface)
                .map_or(false, |i| i.ip_info.is_some())
            {
                err = Some(Error::new(
                    eyre!("{}", t!("net.gateway.cannot-forget-connected-interface")),
                    ErrorKind::InvalidRequest,
                ));
                return false;
            }
            ip_info.remove(interface).is_some()
        });
        if let Some(e) = err {
            return Err(e);
        }
        if changed {
            sub.recv().await;
        }
        Ok(())
    }

    pub async fn delete_iface(&self, interface: &GatewayId) -> Result<(), Error> {
        let Some(has_ip_info) = self
            .watcher
            .ip_info
            .peek(|ifaces| ifaces.get(interface).map(|i| i.ip_info.is_some()))
        else {
            return self.forget(interface).await;
        };

        if has_ip_info {
            let mut ip_info = self.watcher.ip_info.clone_unseen();

            let connection = Connection::system().await?;

            let netman_proxy = NetworkManagerProxy::new(&connection).await?;

            let device = Some(
                netman_proxy
                    .get_device_by_ip_iface(interface.as_str())
                    .await?,
            )
            .filter(|o| &**o != "/")
            .or_not_found(lazy_format!("{interface} in NetworkManager"))?;

            let device_proxy = DeviceProxy::new(&connection, device).await?;

            let ac = device_proxy.active_connection().await?;

            if &*ac == "/" {
                return Err(Error::new(
                    eyre!("{}", t!("net.gateway.cannot-delete-without-connection")),
                    ErrorKind::InvalidRequest,
                ));
            }

            let ac_proxy = active_connection::ActiveConnectionProxy::new(&connection, ac).await?;

            let settings =
                ConnectionSettingsProxy::new(&connection, ac_proxy.connection().await?).await?;

            settings.delete().await?;

            ip_info
                .wait_for(|ifaces| ifaces.get(interface).map_or(true, |i| i.ip_info.is_none()))
                .await;
        }

        self.forget(interface).await?;

        Ok(())
    }

    pub async fn set_name(&self, interface: &GatewayId, name: InternedString) -> Result<(), Error> {
        let mut sub = self
            .db
            .subscribe(
                "/public/serverInfo/network/gateways"
                    .parse::<JsonPointer<_, _>>()
                    .with_kind(ErrorKind::Database)?
                    .join_end(interface.as_str())
                    .join_end("name"),
            )
            .await;
        let changed = self.watcher.ip_info.send_if_modified(|i| {
            i.get_mut(interface)
                .map(|i| {
                    if i.name.as_ref() != Some(&name) {
                        i.name = Some(name);
                        true
                    } else {
                        false
                    }
                })
                .unwrap_or(false)
        });
        if changed {
            sub.recv().await;
        }

        Ok(())
    }
}

pub fn lookup_info_by_addr(
    ip_info: &OrdMap<GatewayId, NetworkInterfaceInfo>,
    addr: SocketAddr,
) -> Option<(&GatewayId, &NetworkInterfaceInfo)> {
    ip_info.iter().find(|(_, i)| {
        i.ip_info
            .as_ref()
            .map_or(false, |i| i.subnets.iter().any(|i| i.addr() == addr.ip()))
    })
}

#[derive(Clone, Debug)]
pub struct GatewayInfo {
    pub id: GatewayId,
    pub info: NetworkInterfaceInfo,
}
impl<V: MetadataVisitor> Visit<V> for GatewayInfo {
    fn visit(&self, visitor: &mut V) -> <V as visit_rs::Visitor>::Result {
        visitor.visit(self)
    }
}

/// Metadata for connections accepted by WildcardListener or VHostBindListener.
#[derive(Clone, Debug, VisitFields)]
pub struct NetworkInterfaceListenerAcceptMetadata {
    pub inner: TcpMetadata,
    pub info: GatewayInfo,
}
impl<V: MetadataVisitor> Visit<V> for NetworkInterfaceListenerAcceptMetadata {
    fn visit(&self, visitor: &mut V) -> V::Result {
        self.visit_fields(visitor).collect()
    }
}

/// A simple TCP listener on 0.0.0.0:port that looks up GatewayInfo from the
/// connection's local address on each accepted connection.
pub struct WildcardListener {
    listener: TcpListener,
    ip_info: Watch<OrdMap<GatewayId, NetworkInterfaceInfo>>,
    /// Handle to the self-contained watcher task started in `new()`.
    /// Dropped (and thus aborted) when `set_ip_info` replaces the ip_info source.
    _watcher: Option<NonDetachingJoinHandle<()>>,
}
impl WildcardListener {
    pub fn new(port: u16) -> Result<Self, Error> {
        let listener = TcpListener::from_std(
            mio::net::TcpListener::bind(SocketAddr::new(IpAddr::V6(Ipv6Addr::UNSPECIFIED), port))
                .with_kind(ErrorKind::Network)?
                .into(),
        )
        .with_kind(ErrorKind::Network)?;
        let ip_info = Watch::new(OrdMap::new());
        let watcher_handle =
            tokio::spawn(watcher(ip_info.clone(), Watch::new(BTreeMap::new()), None)).into();
        Ok(Self {
            listener,
            ip_info,
            _watcher: Some(watcher_handle),
        })
    }

    /// Replace the ip_info source with the one from the NetworkInterfaceController.
    /// Aborts the self-contained watcher task.
    pub fn set_ip_info(&mut self, ip_info: Watch<OrdMap<GatewayId, NetworkInterfaceInfo>>) {
        self.ip_info = ip_info;
        self._watcher = None;
    }
}
impl Accept for WildcardListener {
    type Metadata = NetworkInterfaceListenerAcceptMetadata;
    fn poll_accept(
        &mut self,
        cx: &mut std::task::Context<'_>,
    ) -> Poll<Result<(Self::Metadata, AcceptStream), Error>> {
        if let Poll::Ready((stream, peer_addr)) = TcpListener::poll_accept(&self.listener, cx)? {
            if let Err(e) = socket2::SockRef::from(&stream).set_keepalive(true) {
                tracing::error!("Failed to set tcp keepalive: {e}");
                tracing::debug!("{e:?}");
            }
            let local_addr = stream.local_addr()?;
            let info = self
                .ip_info
                .peek(|ip_info| {
                    lookup_info_by_addr(ip_info, local_addr).map(|(id, info)| GatewayInfo {
                        id: id.clone(),
                        info: info.clone(),
                    })
                })
                .unwrap_or_else(|| GatewayInfo {
                    id: InternedString::from_static("").into(),
                    info: NetworkInterfaceInfo::default(),
                });
            return Poll::Ready(Ok((
                NetworkInterfaceListenerAcceptMetadata {
                    inner: TcpMetadata {
                        local_addr,
                        peer_addr,
                    },
                    info,
                },
                Box::pin(stream),
            )));
        }
        Poll::Pending
    }
}
