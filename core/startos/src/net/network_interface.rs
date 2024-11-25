use std::collections::{BTreeMap, BTreeSet};
use std::net::{IpAddr, SocketAddr};
use std::sync::{Arc, Weak};

use clap::Parser;
use futures::TryStreamExt;
use imbl_value::InternedString;
use rpc_toolkit::{from_fn_async, Context, HandlerExt, ParentHandler};
use serde::{Deserialize, Serialize};
use tokio::net::TcpStream;
use tokio::sync::RwLock;
use tokio_stream::StreamExt;
use ts_rs::TS;
use zbus::zvariant::OwnedObjectPath;
use zbus::{proxy, Connection};

use crate::context::{CliContext, RpcContext};
use crate::db::model::public::IpInfo;
use crate::db::model::Database;
use crate::net::utils::{iface_is_physical, list_interfaces};
use crate::prelude::*;
use crate::util::sync::SyncMutex;

#[proxy(
    interface = "org.freedesktop.NetworkManager",
    default_service = "org.freedesktop.NetworkManager",
    default_path = "/org/freedesktop/NetworkManager"
)]
trait NetworkManager {
    async fn get_all_devices(&self) -> Result<Vec<OwnedObjectPath>, Error>;
    #[zbus(property)]
    fn active_connections(&self) -> Result<Vec<OwnedObjectPath>, Error>;
}

#[tokio::test]
async fn test() -> Result<(), Error> {
    let connection = Connection::system().await?;

    let proxy = NetworkManagerProxy::new(&connection).await?;
    let reply = proxy.active_connections().await?;
    println!("{reply:?}");

    let mut stream = proxy.receive_active_connections_changed().await;
    while let Some(conn) = stream.next().await {
        println!("{:?}", conn.get().await?);
    }

    Ok(())
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
