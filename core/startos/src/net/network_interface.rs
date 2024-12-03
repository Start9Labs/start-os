use std::collections::{BTreeMap, BTreeSet};
use std::future::Future;
use std::net::{IpAddr, SocketAddr, SocketAddrV6};
use std::pin::Pin;
use std::sync::{Arc, Weak};
use std::task::Poll;

use futures::future::pending;
use futures::TryFutureExt;
use getifaddrs::if_nametoindex;
use helpers::NonDetachingJoinHandle;
use imbl_value::InternedString;
use ipnet::IpNet;
use itertools::Itertools;
use tokio::net::{TcpListener, TcpStream};
use tokio::sync::watch;
use tokio_stream::StreamExt;
use zbus::proxy::PropertyStream;
use zbus::zvariant::{
    DeserializeDict, OwnedObjectPath, OwnedValue, Type as ZType, Value as ZValue,
};
use zbus::{proxy, Connection};

use crate::db::model::public::{IpInfo, NetworkInterfaceInfo};
use crate::db::model::Database;
use crate::prelude::*;
use crate::util::sync::SyncMutex;

#[proxy(
    interface = "org.freedesktop.NetworkManager",
    default_service = "org.freedesktop.NetworkManager",
    default_path = "/org/freedesktop/NetworkManager"
)]
trait NetworkManager {
    #[zbus(property)]
    fn devices(&self) -> Result<Vec<OwnedObjectPath>, Error>;
}

#[proxy(
    interface = "org.freedesktop.NetworkManager.Connection.Active",
    default_service = "org.freedesktop.NetworkManager"
)]
trait ActiveConnection {
    #[zbus(property)]
    fn state_flags(&self) -> Result<u32, Error>;
    #[zbus(property, name = "Type")]
    fn connection_type(&self) -> Result<String, Error>;
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
impl TryFrom<Vec<AddressData>> for IpInfo {
    type Error = Error;
    fn try_from(value: Vec<AddressData>) -> Result<Self, Self::Error> {
        value
            .into_iter()
            .map(|a| {
                IpNet::new(a.address.parse()?, a.prefix as u8).with_kind(ErrorKind::ParseNetAddress)
            })
            .filter_ok(|ipnet| !ipnet.addr().is_unspecified() && !ipnet.addr().is_multicast())
            .collect::<Result<_, _>>()
            .map(Self)
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
}

struct WatchPropertyStream<'a, T> {
    stream: PropertyStream<'a, T>,
    last: T,
}
impl<'a, T> WatchPropertyStream<'a, T>
where
    T: Unpin + TryFrom<OwnedValue>,
    T::Error: Into<zbus::Error>,
{
    fn new(stream: PropertyStream<'a, T>, first: T) -> Self {
        Self {
            stream,
            last: first,
        }
    }
    async fn until_changed<Fut: Future<Output = Result<(), Error>>>(
        &mut self,
        fut: Fut,
    ) -> Result<(), Error> {
        let next = self.stream.next();
        tokio::select! {
            changed = next => {
                self.last = changed.ok_or_else(|| Error::new(eyre!("stream is empty"), ErrorKind::DBus))?.get().await?;
                Ok(())
            },
            res = fut.and_then(|_| pending()) => {
                res
            }
        }
    }
}

#[instrument(skip_all)]
async fn watcher(write_to: watch::Sender<BTreeMap<InternedString, NetworkInterfaceInfo>>) {
    loop {
        let res: Result<(), Error> = async {
            let connection = Connection::system().await?;
            let netman_proxy = NetworkManagerProxy::new(&connection).await?;

            let mut devices_sub = WatchPropertyStream::new(
                netman_proxy.receive_devices_changed().await,
                netman_proxy.devices().await?,
            );

            loop {
                let devices = devices_sub.last.clone();
                devices_sub
                    .until_changed(async {
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
                            let ac_proxy = ActiveConnectionProxy::new(&connection, dac).await?;
                            let external = ac_proxy.state_flags().await? & 0x80 != 0;
                            if external && iface != "lo" {
                                continue;
                            }
                            jobs.push(watch_ip(
                                &connection,
                                device_proxy.clone(),
                                iface.clone(),
                                &write_to,
                            ));
                            ifaces.insert(iface);
                        }

                        write_to.send_if_modified(|m| {
                            let mut changed = false;
                            m.retain(|i, _| {
                                if ifaces.contains(i) {
                                    true
                                } else {
                                    changed |= true;
                                    false
                                }
                            });
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

#[instrument(skip_all)]
async fn watch_ip(
    connection: &Connection,
    device_proxy: DeviceProxy<'_>,
    iface: InternedString,
    write_to: &watch::Sender<BTreeMap<InternedString, NetworkInterfaceInfo>>,
) -> Result<(), Error> {
    let mut ip4_config_sub = WatchPropertyStream::new(
        device_proxy.receive_ip4_config_changed().await,
        device_proxy.ip4_config().await?,
    );
    let mut ip6_config_sub = WatchPropertyStream::new(
        device_proxy.receive_ip6_config_changed().await,
        device_proxy.ip6_config().await?,
    );

    loop {
        let ip4_config = ip4_config_sub.last.clone();
        let ip6_config = ip6_config_sub.last.clone();
        ip4_config_sub
            .until_changed(ip6_config_sub.until_changed(async {
                let ip4_proxy = Ip4ConfigProxy::new(&connection, ip4_config).await?;
                let mut address4_sub = WatchPropertyStream::new(
                    ip4_proxy.receive_address_data_changed().await,
                    ip4_proxy.address_data().await?,
                );
                let ip6_proxy = Ip6ConfigProxy::new(&connection, ip6_config).await?;
                let mut address6_sub = WatchPropertyStream::new(
                    ip6_proxy.receive_address_data_changed().await,
                    ip6_proxy.address_data().await?,
                );

                loop {
                    let addresses = address4_sub
                        .last
                        .iter()
                        .cloned()
                        .chain(address6_sub.last.iter().cloned())
                        .collect_vec();
                    address4_sub
                        .until_changed(address6_sub.until_changed(async {
                            let ip_info: IpInfo = addresses.try_into()?;
                            let scope_id =
                                Some(if_nametoindex(&*iface).with_kind(ErrorKind::Network)?);

                            write_to.send_if_modified(|m| {
                                let public = m.get(&iface).map_or(false, |i| i.public);
                                m.insert(
                                    iface.clone(),
                                    NetworkInterfaceInfo {
                                        public,
                                        scope_id,
                                        ip_info: ip_info.clone(),
                                    },
                                )
                                .filter(|old| &old.ip_info == &ip_info && old.scope_id == scope_id)
                                .is_none()
                            });

                            Ok::<_, Error>(())
                        }))
                        .await?;
                }
            }))
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
        ip_info: &BTreeMap<InternedString, NetworkInterfaceInfo>,
    ) -> Result<(), Error> {
        db.mutate(|db| {
            let ifaces_model = db
                .as_public_mut()
                .as_server_info_mut()
                .as_network_interfaces_mut();
            let mut keep = BTreeSet::new();
            for (iface, ip_info) in dbg!(ip_info) {
                keep.insert(iface);
                ifaces_model
                    .upsert(&iface, || Ok(NetworkInterfaceInfo::default()))?
                    .ser(&ip_info)?;
            }
            for iface in ifaces_model.keys()? {
                if !keep.contains(&&iface) {
                    if let Some(info) = ifaces_model.as_idx_mut(&iface) {
                        info.as_ip_info_mut().ser(&IpInfo::default())?;
                    }
                }
            }
            Ok(())
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
                    Ok(info) => {
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
                            Self::sync(&db, dbg!(&ip_info)).await?;

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
            if public || !info.public {
                for ipnet in &info.ip_info.0 {
                    let scope_id = info.scope_id.unwrap_or(0);
                    let key = (ipnet.addr(), scope_id);
                    keep.insert(key);
                    if let Some((_, is_public)) = self.listeners.get_mut(&key) {
                        *is_public = info.public;
                        continue;
                    }
                    self.listeners.insert(
                        key,
                        (
                            TcpListener::bind(match ipnet.addr() {
                                IpAddr::V6(ip6) => {
                                    SocketAddrV6::new(ip6, self.port, 0, scope_id).into()
                                }
                                ip => SocketAddr::new(ip, self.port),
                            })
                            .await?,
                            info.public,
                        ),
                    );
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
            dbg!(ip, listener);
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
                self.listeners.update(dbg!(&ip_info), public).await?;
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
