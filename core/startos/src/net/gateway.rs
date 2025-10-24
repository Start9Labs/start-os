use std::any::Any;
use std::collections::{BTreeMap, BTreeSet, HashMap};
use std::future::Future;
use std::net::{IpAddr, Ipv4Addr, SocketAddr, SocketAddrV6};
use std::sync::{Arc, Weak};
use std::task::{ready, Poll};
use std::time::Duration;

use clap::Parser;
use futures::future::Either;
use futures::{FutureExt, Stream, StreamExt, TryStreamExt};
use helpers::NonDetachingJoinHandle;
use imbl::{OrdMap, OrdSet};
use imbl_value::InternedString;
use ipnet::IpNet;
use itertools::Itertools;
use models::GatewayId;
use nix::net::if_::if_nametoindex;
use patch_db::json_ptr::JsonPointer;
use rpc_toolkit::{from_fn_async, Context, HandlerArgs, HandlerExt, ParentHandler};
use serde::{Deserialize, Serialize};
use tokio::io::{AsyncBufReadExt, BufReader};
use tokio::net::TcpListener;
use tokio::process::Command;
use tokio::sync::oneshot;
use ts_rs::TS;
use visit_rs::{Visit, VisitFields};
use zbus::proxy::{PropertyChanged, PropertyStream, SignalStream};
use zbus::zvariant::{
    DeserializeDict, Dict, OwnedObjectPath, OwnedValue, Type as ZType, Value as ZValue,
};
use zbus::{proxy, Connection};

use crate::context::{CliContext, RpcContext};
use crate::db::model::public::{IpInfo, NetworkInterfaceInfo, NetworkInterfaceType};
use crate::db::model::Database;
use crate::net::forward::START9_BRIDGE_IFACE;
use crate::net::gateway::device::DeviceProxy;
use crate::net::utils::ipv6_is_link_local;
use crate::net::web_server::{Accept, AcceptStream, Acceptor, MetadataVisitor};
use crate::prelude::*;
use crate::util::collections::OrdMapIterMut;
use crate::util::future::Until;
use crate::util::io::open_file;
use crate::util::serde::{display_serializable, HandlerExtSerde};
use crate::util::sync::{SyncMutex, Watch};
use crate::util::Invoke;

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
                    table.add_row(row![bc => "INTERFACE", "TYPE", "PUBLIC", "ADDRESSES", "WAN IP"]);
                    for (iface, info) in res {
                        table.add_row(row![
                            iface,
                            info.ip_info.as_ref()
                                .and_then(|ip_info| ip_info.device_type)
                                .map_or_else(|| "UNKNOWN".to_owned(), |ty| format!("{ty:?}")),
                            info.public(),
                            info.ip_info.as_ref().map_or_else(
                                || "<DISCONNECTED>".to_owned(),
                                |ip_info| ip_info.subnets
                                    .iter()
                                    .map(|ipnet| match ipnet.addr() {
                                        IpAddr::V4(ip) => format!("{ip}/{}", ipnet.prefix_len()),
                                        IpAddr::V6(ip) => format!(
                                            "[{ip}%{}]/{}",
                                            ip_info.scope_id,
                                            ipnet.prefix_len()
                                        ),
                                    })
                                    .join(", ")),
                            info.ip_info.as_ref()
                                .and_then(|ip_info| ip_info.wan_ip)
                                .map_or_else(|| "N/A".to_owned(), |ip| ip.to_string())
                        ]);
                    }

                    table.print_tty(false)?;

                    Ok(())
                })
                .with_about("Show gateways StartOS can listen on")
                .with_call_remote::<CliContext>(),
        )
        .subcommand(
            "set-public",
            from_fn_async(set_public)
                .with_metadata("sync_db", Value::Bool(true))
                .no_display()
                .with_about("Indicate whether this gateway has inbound access from the WAN")
                .with_call_remote::<CliContext>(),
        ).subcommand(
            "unset-public",
            from_fn_async(unset_public)
                .with_metadata("sync_db", Value::Bool(true))
                .no_display()
                .with_about("Allow this gateway to infer whether it has inbound access from the WAN based on its IPv4 address")
                .with_call_remote::<CliContext>(),
        ).subcommand("forget",
            from_fn_async(forget_iface)
                .with_metadata("sync_db", Value::Bool(true))
                .no_display()
                .with_about("Forget a disconnected gateway")
                .with_call_remote::<CliContext>()
        ).subcommand("set-name",
        from_fn_async(set_name)
            .with_metadata("sync_db", Value::Bool(true))
            .no_display()
            .with_about("Rename a gateway")
            .with_call_remote::<CliContext>()
    )
}

async fn list_interfaces(
    ctx: RpcContext,
) -> Result<OrdMap<GatewayId, NetworkInterfaceInfo>, Error> {
    Ok(ctx.net_controller.net_iface.watcher.ip_info())
}

#[derive(Debug, Clone, Deserialize, Serialize, Parser, TS)]
#[ts(export)]
struct NetworkInterfaceSetPublicParams {
    gateway: GatewayId,
    public: Option<bool>,
}

async fn set_public(
    ctx: RpcContext,
    NetworkInterfaceSetPublicParams { gateway, public }: NetworkInterfaceSetPublicParams,
) -> Result<(), Error> {
    ctx.net_controller
        .net_iface
        .set_public(&gateway, Some(public.unwrap_or(true)))
        .await
}

#[derive(Debug, Clone, Deserialize, Serialize, Parser, TS)]
#[ts(export)]
struct UnsetPublicParams {
    gateway: GatewayId,
}

async fn unset_public(
    ctx: RpcContext,
    UnsetPublicParams { gateway }: UnsetPublicParams,
) -> Result<(), Error> {
    ctx.net_controller
        .net_iface
        .set_public(&gateway, None)
        .await
}

#[derive(Debug, Clone, Deserialize, Serialize, Parser, TS)]
#[ts(export)]
struct ForgetGatewayParams {
    gateway: GatewayId,
}

async fn forget_iface(
    ctx: RpcContext,
    ForgetGatewayParams { gateway }: ForgetGatewayParams,
) -> Result<(), Error> {
    ctx.net_controller.net_iface.forget(&gateway).await
}

#[derive(Debug, Clone, Deserialize, Serialize, Parser, TS)]
#[ts(export)]
struct RenameGatewayParams {
    id: GatewayId,
    name: InternedString,
}

async fn set_name(
    ctx: RpcContext,
    RenameGatewayParams { id, name }: RenameGatewayParams,
) -> Result<(), Error> {
    ctx.net_controller.net_iface.set_name(&id, name).await
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
) {
    loop {
        let res: Result<(), Error> = async {
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
                        futures::future::try_join_all(jobs).await?;

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

async fn get_wan_ipv4(iface: &str) -> Result<Option<Ipv4Addr>, Error> {
    let client = reqwest::Client::builder();
    #[cfg(target_os = "linux")]
    let client = client.interface(iface);
    Ok(client
        .build()?
        .get("https://ip4only.me/api/")
        .timeout(Duration::from_secs(10))
        .send()
        .await?
        .error_for_status()?
        .text()
        .await?
        .split(",")
        .skip(1)
        .next()
        .filter(|s| !s.is_empty())
        .map(|s| s.parse())
        .transpose()?)
}

#[instrument(skip(connection, device_proxy, write_to))]
async fn watch_ip(
    connection: &Connection,
    device_proxy: device::DeviceProxy<'_>,
    iface: GatewayId,
    write_to: &Watch<OrdMap<GatewayId, NetworkInterfaceInfo>>,
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
                tokio::time::sleep(Duration::from_secs(300)).await;
                Ok(())
            }
            .fuse()
        });

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

                            if device_type == Some(NetworkInterfaceType::Loopback) {
                                return Ok(());
                            }

                            let name = InternedString::from(active_connection_proxy.id().await?);

                            let dhcp4_config = active_connection_proxy.dhcp4_config().await?;
                            let ip4_proxy =
                                Ip4ConfigProxy::new(&connection, ip4_config.clone()).await?;
                            let ip6_proxy =
                                Ip6ConfigProxy::new(&connection, ip6_config.clone()).await?;
                            let mut until = Until::new()
                                .with_stream(ip4_proxy.receive_address_data_changed().await.stub())
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

                            loop {
                                until
                                    .run(async {
                                        let addresses = ip4_proxy
                                            .address_data()
                                            .await?
                                            .into_iter()
                                            .chain(ip6_proxy.address_data().await?)
                                            .collect_vec();
                                        let lan_ip = [
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
                                        if let Some(dhcp4_proxy) = &dhcp4_proxy {
                                            let dhcp = dhcp4_proxy.options().await?;
                                            if let Some(ntp) = dhcp.ntp_servers {
                                                ntp_servers.extend(
                                                    ntp.split_whitespace()
                                                        .map(InternedString::intern),
                                                );
                                            }
                                            if let Some(dns) = dhcp.domain_name_servers {
                                                dns_servers.extend(
                                                    dns.split_ascii_whitespace()
                                                        .filter_map(|s| {
                                                            s.parse::<IpAddr>().log_err()
                                                        })
                                                        .collect::<Vec<_>>(),
                                                );
                                            }
                                        }
                                        let scope_id = if_nametoindex(iface.as_str())
                                            .with_kind(ErrorKind::Network)?;
                                        let subnets: OrdSet<IpNet> = addresses
                                            .into_iter()
                                            .map(IpNet::try_from)
                                            .try_collect()?;
                                        let wan_ip = if !subnets.is_empty() {
                                            match get_wan_ipv4(iface.as_str()).await {
                                                Ok(a) => a,
                                                Err(e) => {
                                                    tracing::error!(
                                                    "Failed to determine WAN IP for {iface}: {e}"
                                                );
                                                    tracing::debug!("{e:?}");
                                                    None
                                                }
                                            }
                                        } else {
                                            None
                                        };
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

                                        write_to.send_if_modified(
                                            |m: &mut OrdMap<GatewayId, NetworkInterfaceInfo>| {
                                                let (name, public, secure, prev_wan_ip) = m
                                                    .get(&iface)
                                                    .map_or((None, None, None, None), |i| {
                                                        (
                                                            i.name.clone(),
                                                            i.public,
                                                            i.secure,
                                                            i.ip_info
                                                                .as_ref()
                                                                .and_then(|i| i.wan_ip),
                                                        )
                                                    });
                                                ip_info.wan_ip = ip_info.wan_ip.or(prev_wan_ip);
                                                let ip_info = Arc::new(ip_info);
                                                m.insert(
                                                    iface.clone(),
                                                    NetworkInterfaceInfo {
                                                        name,
                                                        public,
                                                        secure,
                                                        ip_info: Some(ip_info.clone()),
                                                    },
                                                )
                                                .filter(|old| &old.ip_info == &Some(ip_info))
                                                .is_none()
                                            },
                                        );

                                        Ok::<_, Error>(())
                                    })
                                    .await?;
                            }
                        })
                        .await?;
                }
            })
            .await?;
    }
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
    listeners: SyncMutex<BTreeMap<u16, Weak<()>>>,
}
impl NetworkInterfaceWatcher {
    pub fn new(
        seed: impl Future<Output = OrdMap<GatewayId, NetworkInterfaceInfo>> + Send + Sync + 'static,
        watch_activated: impl IntoIterator<Item = GatewayId>,
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
                watcher(ip_info, activated).await
            })
            .into(),
            listeners: SyncMutex::new(BTreeMap::new()),
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

    pub fn bind<B: Bind>(&self, bind: B, port: u16) -> Result<NetworkInterfaceListener<B>, Error> {
        let arc = Arc::new(());
        self.listeners.mutate(|l| {
            if l.get(&port).filter(|w| w.strong_count() > 0).is_some() {
                return Err(Error::new(
                    std::io::Error::from_raw_os_error(libc::EADDRINUSE),
                    ErrorKind::Network,
                ));
            }
            l.insert(port, Arc::downgrade(&arc));
            Ok(())
        })?;
        let ip_info = self.ip_info.clone_unseen();
        Ok(NetworkInterfaceListener {
            _arc: arc,
            ip_info,
            listeners: ListenerMap::new(bind, port),
        })
    }

    pub fn upgrade_listener<B: Bind>(
        &self,
        SelfContainedNetworkInterfaceListener {
            mut listener,
            ..
        }: SelfContainedNetworkInterfaceListener<B>,
    ) -> Result<NetworkInterfaceListener<B>, Error> {
        let port = listener.listeners.port;
        let arc = &listener._arc;
        self.listeners.mutate(|l| {
            if l.get(&port).filter(|w| w.strong_count() > 0).is_some() {
                return Err(Error::new(
                    std::io::Error::from_raw_os_error(libc::EADDRINUSE),
                    ErrorKind::Network,
                ));
            }
            l.insert(port, Arc::downgrade(arc));
            Ok(())
        })?;
        let ip_info = self.ip_info.clone_unseen();
        ip_info.mark_changed();
        listener.change_ip_info_source(ip_info);
        Ok(listener)
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

        db.mutate(|db| {
            db.as_public_mut()
                .as_server_info_mut()
                .as_network_mut()
                .as_gateways_mut()
                .ser(info)
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
                            tracing::error!("Error loading network interface info: {e}");
                            tracing::debug!("{e:?}");
                            OrdMap::new()
                        }
                    };
                    let _ = seeded_send.send(info.clone());
                    info
                }
            },
            [InternedString::from_static(START9_BRIDGE_IFACE).into()],
        );
        let mut ip_info_watch = watcher.subscribe();
        ip_info_watch.mark_seen();
        Self {
            db: db.clone(),
            watcher,
            _sync: tokio::spawn(async move {
                let res: Result<(), Error> = async {
                    let mut ip_info = seeded.await.ok();
                    loop {
                        if let Err(e) = async {
                            if let Some(ip_info) = ip_info {
                                Self::sync(&db, &ip_info).boxed().await?;
                            }

                            Ok::<_, Error>(())
                        }
                        .await
                        {
                            tracing::error!("Error syncing ip info to db: {e}");
                            tracing::debug!("{e:?}");
                        }

                        let _ = ip_info_watch.changed().await;
                        ip_info = Some(ip_info_watch.read());
                    }
                }
                .await;
                if let Err(e) = res {
                    tracing::error!("Error syncing ip info to db: {e}");
                    tracing::debug!("{e:?}");
                }
            })
            .into(),
        }
    }

    pub async fn set_public(
        &self,
        interface: &GatewayId,
        public: Option<bool>,
    ) -> Result<(), Error> {
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
            let prev = std::mem::replace(
                &mut match ip_info.get_mut(interface).or_not_found(interface) {
                    Ok(a) => a,
                    Err(e) => {
                        err = Some(e);
                        return false;
                    }
                }
                .public,
                public,
            );
            prev != public
        });
        if let Some(e) = err {
            return Err(e);
        }
        if changed {
            sub.recv().await;
        }
        Ok(())
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
                    eyre!("Cannot forget currently connected interface"),
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
                    eyre!("Cannot delete device without active connection"),
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

pub trait InterfaceFilter: Any + Clone + std::fmt::Debug + Eq + Ord + Send + Sync {
    fn filter(&self, id: &GatewayId, info: &NetworkInterfaceInfo) -> bool;
    fn eq(&self, other: &dyn Any) -> bool {
        Some(self) == other.downcast_ref::<Self>()
    }
    fn cmp(&self, other: &dyn Any) -> std::cmp::Ordering {
        match (self as &dyn Any).type_id().cmp(&other.type_id()) {
            std::cmp::Ordering::Equal => {
                std::cmp::Ord::cmp(self, other.downcast_ref::<Self>().unwrap())
            }
            ord => ord,
        }
    }
    fn as_any(&self) -> &dyn Any {
        self
    }
    fn into_dyn(self) -> DynInterfaceFilter {
        DynInterfaceFilter::new(self)
    }
}

impl InterfaceFilter for bool {
    fn filter(&self, _: &GatewayId, _: &NetworkInterfaceInfo) -> bool {
        *self
    }
}

#[derive(Clone, Debug, PartialEq, Eq, PartialOrd, Ord)]
pub struct TypeFilter(pub NetworkInterfaceType);
impl InterfaceFilter for TypeFilter {
    fn filter(&self, _: &GatewayId, info: &NetworkInterfaceInfo) -> bool {
        info.ip_info.as_ref().and_then(|i| i.device_type) == Some(self.0)
    }
}

#[derive(Clone, Debug, PartialEq, Eq, PartialOrd, Ord)]
pub struct IdFilter(pub GatewayId);
impl InterfaceFilter for IdFilter {
    fn filter(&self, id: &GatewayId, _: &NetworkInterfaceInfo) -> bool {
        id == &self.0
    }
}

#[derive(Clone, Copy, Debug, PartialEq, Eq, PartialOrd, Ord)]
pub struct PublicFilter {
    pub public: bool,
}
impl InterfaceFilter for PublicFilter {
    fn filter(&self, _: &GatewayId, info: &NetworkInterfaceInfo) -> bool {
        self.public == info.public()
    }
}

#[derive(Clone, Copy, Debug, PartialEq, Eq, PartialOrd, Ord)]
pub struct SecureFilter {
    pub secure: bool,
}
impl InterfaceFilter for SecureFilter {
    fn filter(&self, _: &GatewayId, info: &NetworkInterfaceInfo) -> bool {
        self.secure || info.secure()
    }
}

#[derive(Clone, Copy, Debug, PartialEq, Eq, PartialOrd, Ord)]
pub struct AndFilter<A, B>(pub A, pub B);
impl<A: InterfaceFilter, B: InterfaceFilter> InterfaceFilter for AndFilter<A, B> {
    fn filter(&self, id: &GatewayId, info: &NetworkInterfaceInfo) -> bool {
        self.0.filter(id, info) && self.1.filter(id, info)
    }
}

#[derive(Clone, Copy, Debug, PartialEq, Eq, PartialOrd, Ord)]
pub struct OrFilter<A, B>(pub A, pub B);
impl<A: InterfaceFilter, B: InterfaceFilter> InterfaceFilter for OrFilter<A, B> {
    fn filter(&self, id: &GatewayId, info: &NetworkInterfaceInfo) -> bool {
        self.0.filter(id, info) || self.1.filter(id, info)
    }
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord)]
pub struct AnyFilter(pub BTreeSet<DynInterfaceFilter>);
impl InterfaceFilter for AnyFilter {
    fn filter(&self, id: &GatewayId, info: &NetworkInterfaceInfo) -> bool {
        self.0.iter().any(|f| InterfaceFilter::filter(f, id, info))
    }
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord)]
pub struct AllFilter(pub BTreeSet<DynInterfaceFilter>);
impl InterfaceFilter for AllFilter {
    fn filter(&self, id: &GatewayId, info: &NetworkInterfaceInfo) -> bool {
        self.0.iter().all(|f| InterfaceFilter::filter(f, id, info))
    }
}

pub trait DynInterfaceFilterT: std::fmt::Debug + Any + Send + Sync {
    fn filter(&self, id: &GatewayId, info: &NetworkInterfaceInfo) -> bool;
    fn eq(&self, other: &dyn Any) -> bool;
    fn cmp(&self, other: &dyn Any) -> std::cmp::Ordering;
    fn as_any(&self) -> &dyn Any;
}
impl<T: InterfaceFilter> DynInterfaceFilterT for T {
    fn filter(&self, id: &GatewayId, info: &NetworkInterfaceInfo) -> bool {
        InterfaceFilter::filter(self, id, info)
    }
    fn eq(&self, other: &dyn Any) -> bool {
        InterfaceFilter::eq(self, other)
    }
    fn cmp(&self, other: &dyn Any) -> std::cmp::Ordering {
        InterfaceFilter::cmp(self, other)
    }
    fn as_any(&self) -> &dyn Any {
        InterfaceFilter::as_any(self)
    }
}

#[test]
fn test_interface_filter_eq() {
    let dyn_t = true.into_dyn();
    assert!(DynInterfaceFilterT::eq(
        &dyn_t,
        DynInterfaceFilterT::as_any(&true),
    ))
}

#[derive(Clone, Debug)]
pub struct DynInterfaceFilter(Arc<dyn DynInterfaceFilterT>);
impl InterfaceFilter for DynInterfaceFilter {
    fn filter(&self, id: &GatewayId, info: &NetworkInterfaceInfo) -> bool {
        self.0.filter(id, info)
    }
    fn eq(&self, other: &dyn Any) -> bool {
        self.0.eq(other)
    }
    fn cmp(&self, other: &dyn Any) -> std::cmp::Ordering {
        self.0.cmp(other)
    }
    fn as_any(&self) -> &dyn Any {
        self.0.as_any()
    }
    fn into_dyn(self) -> DynInterfaceFilter {
        self
    }
}
impl DynInterfaceFilter {
    fn new<T: InterfaceFilter>(value: T) -> Self {
        Self(Arc::new(value))
    }
}
impl PartialEq for DynInterfaceFilter {
    fn eq(&self, other: &Self) -> bool {
        DynInterfaceFilterT::eq(&*self.0, DynInterfaceFilterT::as_any(&*other.0))
    }
}
impl Eq for DynInterfaceFilter {}
impl PartialOrd for DynInterfaceFilter {
    fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
        Some(self.0.cmp(other.0.as_any()))
    }
}
impl Ord for DynInterfaceFilter {
    fn cmp(&self, other: &Self) -> std::cmp::Ordering {
        self.0.cmp(other.0.as_any())
    }
}

struct ListenerMap<B: Bind> {
    prev_filter: DynInterfaceFilter,
    bind: B,
    port: u16,
    listeners: BTreeMap<SocketAddr, B::Accept>,
}
impl<B: Bind> ListenerMap<B> {
    fn new(bind: B, port: u16) -> Self {
        Self {
            prev_filter: false.into_dyn(),
            bind,
            port,
            listeners: BTreeMap::new(),
        }
    }

    #[instrument(skip(self))]
    fn update(
        &mut self,
        ip_info: &OrdMap<GatewayId, NetworkInterfaceInfo>,
        filter: &impl InterfaceFilter,
    ) -> Result<(), Error> {
        let mut keep = BTreeSet::<SocketAddr>::new();
        for (_, info) in ip_info
            .iter()
            .filter(|(id, info)| filter.filter(*id, *info))
        {
            if let Some(ip_info) = &info.ip_info {
                for ipnet in &ip_info.subnets {
                    let addr = match ipnet.addr() {
                        IpAddr::V6(ip6) => SocketAddrV6::new(
                            ip6,
                            self.port,
                            0,
                            if ipv6_is_link_local(ip6) {
                                ip_info.scope_id
                            } else {
                                0
                            },
                        )
                        .into(),
                        ip => SocketAddr::new(ip, self.port),
                    };
                    keep.insert(addr);
                    if !self.listeners.contains_key(&addr) {
                        self.listeners.insert(addr, self.bind.bind(addr)?);
                    }
                }
            }
        }
        self.listeners.retain(|key, _| keep.contains(key));
        self.prev_filter = filter.clone().into_dyn();
        Ok(())
    }
    fn poll_accept(
        &mut self,
        cx: &mut std::task::Context<'_>,
    ) -> Poll<Result<(SocketAddr, <B::Accept as Accept>::Metadata, AcceptStream), Error>> {
        for (addr, listener) in self.listeners.iter_mut() {
            if let Poll::Ready((metadata, stream)) = listener.poll_accept(cx)? {
                return Poll::Ready(Ok((*addr, metadata, stream)));
            }
        }
        Poll::Pending
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

pub trait Bind {
    type Accept: Accept;
    fn bind(&mut self, addr: SocketAddr) -> Result<Self::Accept, Error>;
}

#[derive(Clone, Copy, Default)]
pub struct BindTcp;
impl Bind for BindTcp {
    type Accept = TcpListener;
    fn bind(&mut self, addr: SocketAddr) -> Result<Self::Accept, Error> {
        TcpListener::from_std(
            mio::net::TcpListener::bind(addr)
                .with_kind(ErrorKind::Network)?
                .into(),
        )
        .with_kind(ErrorKind::Network)
    }
}

pub trait FromGatewayInfo {
    fn from_gateway_info(id: &GatewayId, info: &NetworkInterfaceInfo) -> Self;
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
impl FromGatewayInfo for GatewayInfo {
    fn from_gateway_info(id: &GatewayId, info: &NetworkInterfaceInfo) -> Self {
        Self {
            id: id.clone(),
            info: info.clone(),
        }
    }
}

pub struct NetworkInterfaceListener<B: Bind = BindTcp> {
    pub ip_info: Watch<OrdMap<GatewayId, NetworkInterfaceInfo>>,
    listeners: ListenerMap<B>,
    _arc: Arc<()>,
}
impl<B: Bind> NetworkInterfaceListener<B> {
    pub(super) fn new(
        mut ip_info: Watch<OrdMap<GatewayId, NetworkInterfaceInfo>>,
        bind: B,
        port: u16,
    ) -> Self {
        ip_info.mark_unseen();
        Self {
            ip_info,
            listeners: ListenerMap::new(bind, port),
            _arc: Arc::new(()),
        }
    }

    pub fn port(&self) -> u16 {
        self.listeners.port
    }

    #[cfg_attr(feature = "unstable", inline(never))]
    pub fn poll_accept<M: FromGatewayInfo>(
        &mut self,
        cx: &mut std::task::Context<'_>,
        filter: &impl InterfaceFilter,
    ) -> Poll<Result<(M, <B::Accept as Accept>::Metadata, AcceptStream), Error>> {
        while self.ip_info.poll_changed(cx).is_ready()
            || !DynInterfaceFilterT::eq(&self.listeners.prev_filter, filter.as_any())
        {
            self.ip_info
                .peek_and_mark_seen(|ip_info| self.listeners.update(ip_info, filter))?;
        }
        let (addr, inner, stream) = ready!(self.listeners.poll_accept(cx)?);
        Poll::Ready(Ok((
            self.ip_info
                .peek(|ip_info| {
                    lookup_info_by_addr(ip_info, addr)
                        .map(|(id, info)| M::from_gateway_info(id, info))
                })
                .or_not_found(lazy_format!("gateway for {addr}"))?,
            inner,
            stream,
        )))
    }

    pub fn change_ip_info_source(
        &mut self,
        mut ip_info: Watch<OrdMap<GatewayId, NetworkInterfaceInfo>>,
    ) {
        ip_info.mark_unseen();
        self.ip_info = ip_info;
    }

    pub async fn accept<M: FromGatewayInfo>(
        &mut self,
        filter: &impl InterfaceFilter,
    ) -> Result<(M, <B::Accept as Accept>::Metadata, AcceptStream), Error> {
        futures::future::poll_fn(|cx| self.poll_accept(cx, filter)).await
    }

    pub fn check_filter(&self) -> impl FnOnce(SocketAddr, &DynInterfaceFilter) -> bool + 'static {
        let ip_info = self.ip_info.clone();
        move |addr, filter| {
            ip_info.peek(|i| {
                lookup_info_by_addr(i, addr).map_or(false, |(id, info)| {
                    InterfaceFilter::filter(filter, id, info)
                })
            })
        }
    }
}

#[derive(VisitFields)]
pub struct NetworkInterfaceListenerAcceptMetadata<B: Bind> {
    pub inner: <B::Accept as Accept>::Metadata,
    pub info: GatewayInfo,
}
impl<B, V> Visit<V> for NetworkInterfaceListenerAcceptMetadata<B>
where
    B: Bind,
    <B::Accept as Accept>::Metadata: Visit<V> + Clone + Send + Sync + 'static,
    V: MetadataVisitor,
{
    fn visit(&self, visitor: &mut V) -> V::Result {
        self.visit_fields(visitor).collect()
    }
}

impl<B: Bind> Accept for NetworkInterfaceListener<B> {
    type Metadata = NetworkInterfaceListenerAcceptMetadata<B>;
    fn poll_accept(
        &mut self,
        cx: &mut std::task::Context<'_>,
    ) -> Poll<Result<(Self::Metadata, AcceptStream), Error>> {
        NetworkInterfaceListener::poll_accept(self, cx, &true).map(|res| {
            res.map(|(info, inner, stream)| {
                (
                    NetworkInterfaceListenerAcceptMetadata { inner, info },
                    stream,
                )
            })
        })
    }
}

pub struct SelfContainedNetworkInterfaceListener<B: Bind = BindTcp> {
    _watch_thread: NonDetachingJoinHandle<()>,
    listener: NetworkInterfaceListener<B>,
}
impl<B: Bind> SelfContainedNetworkInterfaceListener<B> {
    pub fn bind(bind: B, port: u16) -> Self {
        let ip_info = Watch::new(OrdMap::new());
        let _watch_thread =
            tokio::spawn(watcher(ip_info.clone(), Watch::new(BTreeMap::new()))).into();
        Self {
            _watch_thread,
            listener: NetworkInterfaceListener::new(ip_info, bind, port),
        }
    }
}
impl<B: Bind> Accept for SelfContainedNetworkInterfaceListener<B> {
    type Metadata = <NetworkInterfaceListener<B> as Accept>::Metadata;
    fn poll_accept(
        &mut self,
        cx: &mut std::task::Context<'_>,
    ) -> std::task::Poll<Result<(Self::Metadata, AcceptStream), Error>> {
        Accept::poll_accept(&mut self.listener, cx)
    }
}

pub type UpgradableListener<B = BindTcp> =
    Option<Either<SelfContainedNetworkInterfaceListener<B>, NetworkInterfaceListener<B>>>;

impl<B> Acceptor<UpgradableListener<B>>
where
    B: Bind + Send + Sync + 'static,
    B::Accept: Send + Sync,
{
    pub fn bind_upgradable(listener: SelfContainedNetworkInterfaceListener<B>) -> Self {
        Self::new(Some(Either::Left(listener)))
    }
}
