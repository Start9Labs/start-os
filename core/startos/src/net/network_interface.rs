use std::collections::{BTreeMap, BTreeSet};
use std::future::Future;
use std::net::{IpAddr, Ipv4Addr, SocketAddr, SocketAddrV6};
use std::pin::Pin;
use std::sync::{Arc, Weak};
use std::task::Poll;
use std::time::Duration;

use clap::Parser;
use futures::{Stream, StreamExt, TryStreamExt};
use getifaddrs::if_nametoindex;
use helpers::NonDetachingJoinHandle;
use imbl_value::InternedString;
use ipnet::IpNet;
use itertools::Itertools;
use patch_db::json_ptr::JsonPointer;
use rpc_toolkit::{from_fn_async, Context, HandlerArgs, HandlerExt, ParentHandler};
use serde::{Deserialize, Serialize};
use tokio::net::{TcpListener, TcpStream};
use tokio::sync::watch;
use ts_rs::TS;
use zbus::proxy::{PropertyChanged, PropertyStream, SignalStream};
use zbus::zvariant::{
    DeserializeDict, OwnedObjectPath, OwnedValue, Type as ZType, Value as ZValue,
};
use zbus::{proxy, Connection};

use crate::context::{CliContext, RpcContext};
use crate::db::model::public::{IpInfo, NetworkInterfaceInfo};
use crate::db::model::Database;
use crate::net::network_interface::active_connection::ActiveConnectionProxy;
use crate::prelude::*;
use crate::util::future::Until;
use crate::util::serde::{display_serializable, HandlerExtSerde};
use crate::util::sync::SyncMutex;

pub fn network_interface_api<C: Context>() -> ParentHandler<C> {
    ParentHandler::new()
        .subcommand(
            "list",
            from_fn_async(list_interfaces)
                .with_display_serializable()
                .with_custom_display_fn(|HandlerArgs { params, .. }, res| {
                    use prettytable::*;

                    if let Some(format) = params.format {
                        return Ok(display_serializable(format, res));
                    }

                    let mut table = Table::new();
                    table.add_row(row![bc => "INTERFACE", "PUBLIC", "ADDRESSES"]);
                    for (iface, info) in res {
                        table.add_row(row![
                            iface,
                            info.public(),
                            info.ip_info.map_or_else(
                                || "<DISCONNECTED>".to_owned(),
                                |ip_info| ip_info.subnets
                                    .into_iter()
                                    .map(|ipnet| match ipnet.addr() {
                                        IpAddr::V4(ip) => format!("{ip}/{}", ipnet.prefix_len()),
                                        IpAddr::V6(ip) => format!(
                                            "[{ip}%{}]/{}",
                                            ip_info.scope_id,
                                            ipnet.prefix_len()
                                        ),
                                    })
                                    .join(", "))
                        ]);
                    }

                    table.print_tty(false).unwrap();

                    Ok(())
                })
                .with_about("Show network interfaces StartOS can listen on")
                .with_call_remote::<CliContext>(),
        )
        .subcommand(
            "set-public",
            from_fn_async(set_public)
                .with_metadata("sync_db", Value::Bool(true))
                .no_display()
                .with_about("Indicate whether this interface is publicly addressable")
                .with_call_remote::<CliContext>(),
        ).subcommand(
            "unset-public",
            from_fn_async(unset_public)
                .with_metadata("sync_db", Value::Bool(true))
                .no_display()
                .with_about("Allow this interface to infer whether it is publicly addressable based on its IPv4 address")
                .with_call_remote::<CliContext>(),
        )
}

async fn list_interfaces(
    ctx: RpcContext,
) -> Result<BTreeMap<InternedString, NetworkInterfaceInfo>, Error> {
    Ok(ctx.net_controller.net_iface.ip_info.borrow().clone())
}

#[derive(Debug, Clone, Deserialize, Serialize, Parser, TS)]
#[ts(export)]
struct SetPublicParams {
    #[ts(type = "string")]
    interface: InternedString,
    public: Option<bool>,
}

async fn set_public(
    ctx: RpcContext,
    SetPublicParams { interface, public }: SetPublicParams,
) -> Result<(), Error> {
    ctx.net_controller
        .net_iface
        .set_public(&interface, Some(public.unwrap_or(true)))
        .await
}

#[derive(Debug, Clone, Deserialize, Serialize, Parser, TS)]
#[ts(export)]
struct UnsetPublicParams {
    #[ts(type = "string")]
    interface: InternedString,
}

async fn unset_public(
    ctx: RpcContext,
    UnsetPublicParams { interface }: UnsetPublicParams,
) -> Result<(), Error> {
    ctx.net_controller
        .net_iface
        .set_public(&interface, None)
        .await
}

#[proxy(
    interface = "org.freedesktop.NetworkManager",
    default_service = "org.freedesktop.NetworkManager",
    default_path = "/org/freedesktop/NetworkManager"
)]
trait NetworkManager {
    #[zbus(property)]
    fn devices(&self) -> Result<Vec<OwnedObjectPath>, Error>;

    #[zbus(signal)]
    fn device_added(&self) -> Result<(), Error>;

    #[zbus(signal)]
    fn device_removed(&self) -> Result<(), Error>;
}

mod active_connection {
    use zbus::proxy;

    use crate::prelude::*;

    #[proxy(
        interface = "org.freedesktop.NetworkManager.Connection.Active",
        default_service = "org.freedesktop.NetworkManager"
    )]
    pub trait ActiveConnection {
        #[zbus(property)]
        fn state_flags(&self) -> Result<u32, Error>;

        #[zbus(property, name = "Type")]
        fn connection_type(&self) -> Result<String, Error>;

        #[zbus(signal)]
        fn state_changed(&self) -> Result<(), Error>;
    }
}

#[proxy(
    interface = "org.freedesktop.NetworkManager.IP4Config",
    default_service = "org.freedesktop.NetworkManager"
)]
trait Ip4Config {
    #[zbus(property)]
    fn address_data(&self) -> Result<Vec<AddressData>, Error>;
}

#[proxy(
    interface = "org.freedesktop.NetworkManager.IP6Config",
    default_service = "org.freedesktop.NetworkManager"
)]
trait Ip6Config {
    #[zbus(property)]
    fn address_data(&self) -> Result<Vec<AddressData>, Error>;
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

#[proxy(
    interface = "org.freedesktop.NetworkManager.Device",
    default_service = "org.freedesktop.NetworkManager"
)]
trait Device {
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

    #[zbus(signal)]
    fn state_changed(&self) -> Result<(), Error>;
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
async fn watcher(write_to: watch::Sender<BTreeMap<InternedString, NetworkInterfaceInfo>>) {
    loop {
        let res: Result<(), Error> = async {
            let connection = Connection::system().await?;

            let netman_proxy = NetworkManagerProxy::new(&connection).await?;

            let mut until = Until::new()
                .with_stream(netman_proxy.receive_devices_changed().await.stub())
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
                );

            loop {
                let devices = netman_proxy.devices().await?;
                until
                    .run(async {
                        let mut ifaces = BTreeSet::new();
                        let mut jobs = Vec::new();
                        for device in devices {
                            let device_proxy =
                                DeviceProxy::new(&connection, device.clone()).await?;
                            let iface = InternedString::intern(device_proxy.ip_interface().await?);
                            if iface.is_empty() {
                                continue;
                            }
                            let managed = device_proxy.managed().await?;
                            if !managed {
                                continue;
                            }
                            let dac = device_proxy.active_connection().await?;
                            if &*dac == "/" {
                                continue;
                            }
                            let ac_proxy =
                                active_connection::ActiveConnectionProxy::new(&connection, dac)
                                    .await?;
                            let external = ac_proxy.state_flags().await? & 0x80 != 0;
                            if external && iface != "lo" {
                                continue;
                            }
                            jobs.push(watch_ip(
                                &connection,
                                ac_proxy,
                                device_proxy.clone(),
                                iface.clone(),
                                &write_to,
                            ));
                            ifaces.insert(iface);
                        }

                        write_to.send_if_modified(|m| {
                            let mut changed = false;
                            for (iface, info) in m {
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
        }
    }
}

async fn get_wan_ipv4(local_addr: Ipv4Addr) -> Result<Option<Ipv4Addr>, Error> {
    Ok(reqwest::Client::builder()
        .local_address(Some(IpAddr::V4(local_addr)))
        .build()?
        .get("http://ip4only.me/api/")
        .timeout(Duration::from_secs(10))
        .send()
        .await?
        .error_for_status()?
        .text()
        .await?
        .split(",")
        .skip(1)
        .next()
        .filter(|s| s.is_empty())
        .map(|s| s.parse())
        .transpose()?)
}

#[instrument(skip_all)]
async fn watch_ip(
    connection: &Connection,
    active_connection_proxy: ActiveConnectionProxy<'_>,
    device_proxy: DeviceProxy<'_>,
    iface: InternedString,
    write_to: &watch::Sender<BTreeMap<InternedString, NetworkInterfaceInfo>>,
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
        .with_stream(
            active_connection_proxy
                .receive_state_changed()
                .await?
                .into_inner()
                .stub(),
        )
        .with_stream(device_proxy.receive_ip4_config_changed().await.stub())
        .with_stream(device_proxy.receive_ip6_config_changed().await.stub());

    loop {
        let ip4_config = device_proxy.ip4_config().await?;
        let ip6_config = device_proxy.ip6_config().await?;
        until
            .run(async {
                let ip4_proxy = Ip4ConfigProxy::new(&connection, ip4_config).await?;
                let ip6_proxy = Ip6ConfigProxy::new(&connection, ip6_config).await?;
                let mut until = Until::new()
                    .with_stream(ip4_proxy.receive_address_data_changed().await.stub())
                    .with_stream(ip6_proxy.receive_address_data_changed().await.stub());

                loop {
                    let addresses = ip4_proxy
                        .address_data()
                        .await?
                        .into_iter()
                        .chain(ip6_proxy.address_data().await?)
                        .collect_vec();
                    if iface == "enp1s0" {
                        dbg!(&addresses);
                    }
                    until
                        .run(async {
                            let scope_id = if_nametoindex(&*iface).with_kind(ErrorKind::Network)?;
                            let subnets: BTreeSet<IpNet> =
                                addresses.into_iter().map(TryInto::try_into).try_collect()?;
                            let ip_info = if !subnets.is_empty() {
                                let wan_ip = if let Some(local_addr) =
                                    subnets.iter().find_map(|s| match s {
                                        IpNet::V4(net)
                                            if !net.addr().is_loopback()
                                                && !net.addr().is_link_local() =>
                                        {
                                            Some(net)
                                        }
                                        _ => None,
                                    }) {
                                    match get_wan_ipv4(local_addr.addr()).await {
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
                                Some(IpInfo {
                                    scope_id,
                                    subnets,
                                    wan_ip,
                                })
                            } else {
                                None
                            };

                            write_to.send_if_modified(|m| {
                                let public = m.get(&iface).map_or(None, |i| i.public);
                                m.insert(
                                    iface.clone(),
                                    dbg!(NetworkInterfaceInfo {
                                        public,
                                        ip_info: ip_info.clone(),
                                    }),
                                )
                                .filter(|old| &dbg!(old).ip_info == &ip_info)
                                .is_none()
                            });

                            Ok::<_, Error>(())
                        })
                        .await?;
                }
            })
            .await?;
    }
}

pub struct NetworkInterfaceController {
    db: TypedPatchDb<Database>,
    ip_info: watch::Sender<BTreeMap<InternedString, NetworkInterfaceInfo>>,
    _watcher: NonDetachingJoinHandle<()>,
    listeners: SyncMutex<BTreeMap<u16, Weak<()>>>,
}
impl NetworkInterfaceController {
    async fn sync(
        db: &TypedPatchDb<Database>,
        info: &BTreeMap<InternedString, NetworkInterfaceInfo>,
    ) -> Result<(), Error> {
        db.mutate(|db| {
            db.as_public_mut()
                .as_server_info_mut()
                .as_network_interfaces_mut()
                .ser(info)
        })
        .await?;
        Ok(())
    }
    pub fn new(db: TypedPatchDb<Database>) -> Self {
        let (write_to, mut read_from) = watch::channel(BTreeMap::new());
        Self {
            db: db.clone(),
            ip_info: write_to.clone(),
            _watcher: tokio::spawn(async move {
                match db
                    .peek()
                    .await
                    .as_public()
                    .as_server_info()
                    .as_network_interfaces()
                    .de()
                {
                    Ok(mut info) => {
                        for info in info.values_mut() {
                            info.ip_info = None;
                        }
                        write_to.send_replace(info);
                    }
                    Err(e) => {
                        tracing::error!("Error loading network interface info: {e}");
                        tracing::debug!("{e:?}");
                    }
                };
                tokio::join!(watcher(write_to), async {
                    loop {
                        if let Err(e) = async {
                            let ip_info = read_from.borrow().clone();
                            Self::sync(&db, &ip_info).await?;

                            read_from.changed().await.map_err(|_| {
                                Error::new(
                                    eyre!("NetworkManager watch thread exited"),
                                    ErrorKind::Network,
                                )
                            })?;

                            Ok::<_, Error>(())
                        }
                        .await
                        {
                            tracing::error!("Error syncing ip info to db: {e}");
                            tracing::debug!("{e:?}");
                        }
                    }
                });
            })
            .into(),
            listeners: SyncMutex::new(BTreeMap::new()),
        }
    }

    pub fn bind(&self, port: u16) -> Result<NetworkInterfaceListener, Error> {
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
        Ok(NetworkInterfaceListener {
            _arc: arc,
            ip_info: self.ip_info.subscribe(),
            listeners: ListenerMap::new(port),
            needs_update: true,
        })
    }

    pub async fn set_public(
        &self,
        interface: &InternedString,
        public: Option<bool>,
    ) -> Result<(), Error> {
        let mut sub = self
            .db
            .subscribe(
                "/public/serverInfo/networkInterfaces"
                    .parse::<JsonPointer<_, _>>()
                    .with_kind(ErrorKind::Database)?,
            )
            .await;
        let mut err = None;
        let changed = self.ip_info.send_if_modified(|ip_info| {
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
}

struct ListenerMap {
    port: u16,
    listeners: BTreeMap<(IpAddr, u32), (TcpListener, bool)>,
}
impl ListenerMap {
    fn new(port: u16) -> Self {
        Self {
            port,
            listeners: BTreeMap::new(),
        }
    }
    async fn update(
        &mut self,
        ip_info: &BTreeMap<InternedString, NetworkInterfaceInfo>,
        public: bool,
    ) -> Result<(), Error> {
        let mut keep = BTreeSet::<(IpAddr, u32)>::new();
        for info in ip_info.values() {
            if public || !info.public() {
                if let Some(ip_info) = &info.ip_info {
                    for ipnet in &ip_info.subnets {
                        let key = (ipnet.addr(), ip_info.scope_id);
                        keep.insert(key);
                        if let Some((_, is_public)) = self.listeners.get_mut(&key) {
                            *is_public = info.public();
                            continue;
                        }
                        self.listeners.insert(
                            key,
                            (
                                TcpListener::bind(match ipnet.addr() {
                                    IpAddr::V6(ip6) => {
                                        SocketAddrV6::new(ip6, self.port, 0, ip_info.scope_id)
                                            .into()
                                    }
                                    ip => SocketAddr::new(ip, self.port),
                                })
                                .await?,
                                info.public(),
                            ),
                        );
                    }
                }
            }
        }
        self.listeners.retain(|key, _| keep.contains(key));
        Ok(())
    }
    fn accept(&mut self) -> ListenerMapFut {
        ListenerMapFut(&mut self.listeners)
    }
}
#[pin_project::pin_project]
struct ListenerMapFut<'a>(&'a mut BTreeMap<(IpAddr, u32), (TcpListener, bool)>);
impl<'a> Future for ListenerMapFut<'a> {
    type Output = Result<(IpAddr, bool, TcpStream, SocketAddr), Error>;
    fn poll(self: Pin<&mut Self>, cx: &mut std::task::Context<'_>) -> Poll<Self::Output> {
        let this = self.project();
        for ((ip, _), listener) in this.0.iter() {
            if let Poll::Ready((stream, addr)) = listener.0.poll_accept(cx)? {
                return Poll::Ready(Ok((*ip, listener.1, stream, addr)));
            }
        }
        Poll::Pending
    }
}

pub struct NetworkInterfaceListener {
    needs_update: bool,
    ip_info: watch::Receiver<BTreeMap<InternedString, NetworkInterfaceInfo>>,
    listeners: ListenerMap,
    _arc: Arc<()>,
}
impl NetworkInterfaceListener {
    pub async fn accept(&mut self, public: bool) -> Result<Accepted, Error> {
        loop {
            if self.needs_update {
                let ip_info = self.ip_info.borrow().clone();
                self.listeners.update(&ip_info, public).await?;
                self.needs_update = false;
            }
            tokio::select! {
                accepted = self.listeners.accept() => {
                    let (ip, is_public, stream, peer) = accepted?;
                    return Ok(Accepted {
                        stream,
                        peer,
                        is_public,
                        bind: (ip, self.listeners.port).into(),
                    })
                },
                res = self.ip_info.changed() => {
                    res.map_err(|_| Error::new(
                        eyre!("NetworkManager watch thread exited"),
                        ErrorKind::Network,
                    ))?;
                    self.needs_update = true;
                }
            }
        }
    }
}

pub struct Accepted {
    pub stream: TcpStream,
    pub peer: SocketAddr,
    pub is_public: bool,
    pub bind: SocketAddr,
}

// async fn _ips() -> Result<BTreeSet<IpAddr>, Error> {
//     Ok(init_ips()
//         .await?
//         .values()
//         .flat_map(|i| {
//             std::iter::empty()
//                 .chain(i.ipv4.map(IpAddr::from))
//                 .chain(i.ipv6.map(IpAddr::from))
//         })
//         .collect())
// }

// pub async fn ips() -> Result<BTreeSet<IpAddr>, Error> {
//     let ips = CACHED_IPS.read().await.clone();
//     if !ips.is_empty() {
//         return Ok(ips);
//     }
//     let ips = _ips().await?;
//     *CACHED_IPS.write().await = ips.clone();
//     Ok(ips)
// }

// pub async fn init_ips() -> Result<BTreeMap<String, IpInfo>, Error> {
//     let mut res = BTreeMap::new();
//     let mut ifaces = list_interfaces();
//     while let Some(iface) = ifaces.try_next().await? {
//         if iface_is_physical(&iface).await {
//             let ip_info = IpInfo::for_interface(&iface).await?;
//             res.insert(iface, ip_info);
//         }
//     }
//     Ok(res)
// }

// // #[command(subcommands(update))]
// pub fn dhcp<C: Context>() -> ParentHandler<C> {
//     ParentHandler::new().subcommand(
//         "update",
//         from_fn_async::<_, _, (), Error, (RpcContext, UpdateParams)>(update)
//             .no_display()
//             .with_about("Update IP assigned by dhcp")
//             .with_call_remote::<CliContext>(),
//     )
// }
// #[derive(Deserialize, Serialize, Parser, TS)]
// #[serde(rename_all = "camelCase")]
// #[command(rename_all = "kebab-case")]
// pub struct UpdateParams {
//     interface: String,
// }

// pub async fn update(
//     ctx: RpcContext,
//     UpdateParams { interface }: UpdateParams,
// ) -> Result<(), Error> {
//     if iface_is_physical(&interface).await {
//         let ip_info = IpInfo::for_interface(&interface).await?;
//         ctx.db
//             .mutate(|db| {
//                 db.as_public_mut()
//                     .as_server_info_mut()
//                     .as_ip_info_mut()
//                     .insert(&interface, &ip_info)
//             })
//             .await?;

//         let mut cached = CACHED_IPS.write().await;
//         if cached.is_empty() {
//             *cached = _ips().await?;
//         } else {
//             cached.extend(
//                 std::iter::empty()
//                     .chain(ip_info.ipv4.map(IpAddr::from))
//                     .chain(ip_info.ipv6.map(IpAddr::from)),
//             );
//         }
//     }
//     Ok(())
// }
