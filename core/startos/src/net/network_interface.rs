use std::collections::{BTreeMap, BTreeSet};
use std::future::Future;
use std::net::{IpAddr, SocketAddr};
use std::sync::{Arc, Weak};
use std::task::Poll;

use clap::Parser;
use futures::future::pending;
use futures::{FutureExt, TryFutureExt, TryStreamExt};
use imbl_value::InternedString;
use ipnet::IpNet;
use serde::{Deserialize, Serialize};
use tokio::net::TcpStream;
use tokio::sync::watch;
use tokio_stream::StreamExt;
use zbus::proxy::PropertyStream;
use zbus::zvariant::{
    DeserializeDict, OwnedObjectPath, OwnedValue, Type as ZType, Value as ZValue,
};
use zbus::{proxy, Connection};

use crate::db::model::public::IpInfo;
use crate::db::model::Database;
use crate::prelude::*;
use crate::util::actor::background::BackgroundJobQueue;
use crate::util::sync::SyncMutex;

#[proxy(
    interface = "org.freedesktop.NetworkManager",
    default_service = "org.freedesktop.NetworkManager",
    default_path = "/org/freedesktop/NetworkManager"
)]
trait NetworkManager {
    #[zbus(property)]
    fn active_connections(&self) -> Result<Vec<OwnedObjectPath>, Error>;
}

#[proxy(
    interface = "org.freedesktop.NetworkManager.Connection.Active",
    default_service = "org.freedesktop.NetworkManager"
)]
trait ActiveConnection {
    #[zbus(property)]
    fn ip4_config(&self) -> Result<OwnedObjectPath, Error>;
    #[zbus(property)]
    fn devices(&self) -> Result<Vec<OwnedObjectPath>, Error>;
}

#[proxy(
    interface = "org.freedesktop.NetworkManager.IP4Config",
    default_service = "org.freedesktop.NetworkManager"
)]
trait Ip4Config {
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
}

#[tokio::test]
async fn test() -> Result<(), Error> {
    let (write_to, mut read_from) = watch::channel(BTreeMap::new());
    tokio::task::spawn(watcher(write_to));
    loop {
        eprintln!("{:?}", &*read_from.borrow());
        read_from.changed().await;
    }

    Ok(())
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
    async fn new(mut stream: PropertyStream<'a, T>) -> Result<Self, Error> {
        let last = stream
            .next()
            .await
            .ok_or_else(|| Error::new(eyre!("stream is empty"), ErrorKind::DBus))?
            .get()
            .await?;
        Ok(Self { stream, last })
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

async fn watcher(write_to: watch::Sender<BTreeMap<InternedString, IpInfo>>) {
    loop {
        if let Err(e) = async {
            let connection = Connection::system().await?;
            let netman_proxy = NetworkManagerProxy::new(&connection).await?;

            let mut active_sub =
                WatchPropertyStream::new(netman_proxy.receive_active_connections_changed().await)
                    .await?;

            loop {
                let active = active_sub.last.clone();
                active_sub
                    .until_changed(async {
                        let mut ifaces = BTreeSet::new();
                        let mut jobs = Vec::new();
                        for active in active {
                            let ac_proxy = ActiveConnectionProxy::new(&connection, active).await?;
                            let mut devices = ac_proxy.devices().await?;
                            if devices.len() == 1 {
                                let dev_proxy =
                                    DeviceProxy::new(&connection, devices.swap_remove(0)).await?;
                                let iface = InternedString::intern(dev_proxy.ip_interface().await?);
                                ifaces.insert(iface.clone());
                                jobs.push(async {
                                    let ac_proxy = ac_proxy;
                                    let iface = iface;
                                    let mut ip_config_sub = WatchPropertyStream::new(
                                        ac_proxy.receive_ip4_config_changed().await,
                                    )
                                    .await?;

                                    loop {
                                        let ip_config = ip_config_sub.last.clone();
                                        ip_config_sub
                                            .until_changed(async {
                                                let ip_proxy =
                                                    Ip4ConfigProxy::new(&connection, ip_config)
                                                        .await?;
                                                let mut address_sub = WatchPropertyStream::new(
                                                    ip_proxy.receive_address_data_changed().await,
                                                )
                                                .await?;

                                                loop {
                                                    let addresses = address_sub.last.clone();
                                                    address_sub
                                                        .until_changed(async {
                                                            let ip_info: IpInfo =
                                                                addresses.try_into()?;

                                                            write_to.send_if_modified(|m| {
                                                                m.insert(iface.clone(), ip_info.clone())
                                                                    .filter(|old| old == &ip_info)
                                                                    .is_none()
                                                            });

                                                            Ok::<_, Error>(())
                                                        })
                                                        .await?;
                                                }
                                            })
                                            .await?;
                                    }

                                    Ok::<_, Error>(())
                                });
                            } else {
                                tracing::warn!("devices.len ({}) is not exactly 1. We're not sure what this means, but it shouldn't happen", devices.len());
                            }
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

            Ok::<_, Error>(())
        }
        .await
        {
            tracing::error!("{e}");
            tracing::debug!("{e:?}");
        }
    }
}

pub struct NetworkInterfaceController {
    db: TypedPatchDb<Database>,
    listeners: SyncMutex<BTreeMap<u16, Weak<()>>>,
}
impl NetworkInterfaceController {
    pub fn new(db: TypedPatchDb<Database>) -> Self {
        Self {
            db,
            listeners: SyncMutex::new(BTreeMap::new()),
        }
    }

    pub fn bind(&self, port: u16) -> Result<NetworkInterfaceListener, Error> {
        todo!()
    }
}

pub struct NetworkInterfaceListener {
    ctrl: Arc<NetworkInterfaceController>,
}
impl NetworkInterfaceListener {
    pub async fn accept(&mut self, public: bool) -> Result<Accepted, Error> {
        todo!()
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
