use std::collections::{BTreeMap, BTreeSet};
use std::net::{IpAddr, SocketAddr, SocketAddrV6};
use std::sync::{Arc, Weak};

use futures::channel::oneshot;
use helpers::NonDetachingJoinHandle;
use id_pool::IdPool;
use imbl::OrdMap;
use models::GatewayId;
use rpc_toolkit::{from_fn_async, Context, HandlerArgs, HandlerExt, ParentHandler};
use serde::{Deserialize, Serialize};
use tokio::process::Command;
use tokio::sync::mpsc;

use crate::context::{CliContext, RpcContext};
use crate::db::model::public::NetworkInterfaceInfo;
use crate::net::gateway::{DynInterfaceFilter, InterfaceFilter, SecureFilter};
use crate::net::utils::ipv6_is_link_local;
use crate::prelude::*;
use crate::util::serde::{display_serializable, HandlerExtSerde};
use crate::util::sync::Watch;
use crate::util::Invoke;

pub const START9_BRIDGE_IFACE: &str = "lxcbr0";
pub const FIRST_DYNAMIC_PRIVATE_PORT: u16 = 49152;

#[derive(Debug, Deserialize, Serialize)]
pub struct AvailablePorts(IdPool);
impl AvailablePorts {
    pub fn new() -> Self {
        Self(IdPool::new_ranged(FIRST_DYNAMIC_PRIVATE_PORT..u16::MAX))
    }
    pub fn alloc(&mut self) -> Result<u16, Error> {
        self.0.request_id().ok_or_else(|| {
            Error::new(
                eyre!("No more dynamic ports available!"),
                ErrorKind::Network,
            )
        })
    }
    pub fn free(&mut self, ports: impl IntoIterator<Item = u16>) {
        for port in ports {
            self.0.return_id(port).unwrap_or_default();
        }
    }
}

pub fn forward_api<C: Context>() -> ParentHandler<C> {
    ParentHandler::new().subcommand(
        "dump-table",
        from_fn_async(
            |ctx: RpcContext| async move { ctx.net_controller.forward.dump_table().await },
        )
        .with_display_serializable()
        .with_custom_display_fn(|HandlerArgs { params, .. }, res| {
            use prettytable::*;

            if let Some(format) = params.format {
                return display_serializable(format, res);
            }

            let mut table = Table::new();
            table.add_row(row![bc => "FROM", "TO", "FILTER / GATEWAY"]);

            for (external, target) in res.0 {
                table.add_row(row![external, target.target, target.filter]);
                for (source, gateway) in target.gateways {
                    table.add_row(row![
                        format!("{}:{}", source, external),
                        target.target,
                        gateway
                    ]);
                }
            }

            table.print_tty(false)?;

            Ok(())
        })
        .with_call_remote::<CliContext>(),
    )
}

struct ForwardRequest {
    external: u16,
    target: SocketAddr,
    filter: DynInterfaceFilter,
    rc: Weak<()>,
}

#[derive(Clone)]
struct ForwardEntry {
    external: u16,
    target: SocketAddr,
    prev_filter: DynInterfaceFilter,
    forwards: BTreeMap<SocketAddr, GatewayId>,
    rc: Weak<()>,
}
impl ForwardEntry {
    fn new(external: u16, target: SocketAddr, rc: Weak<()>) -> Self {
        Self {
            external,
            target,
            prev_filter: false.into_dyn(),
            forwards: BTreeMap::new(),
            rc,
        }
    }

    fn take(&mut self) -> Self {
        Self {
            external: self.external,
            target: self.target,
            prev_filter: std::mem::replace(&mut self.prev_filter, false.into_dyn()),
            forwards: std::mem::take(&mut self.forwards),
            rc: self.rc.clone(),
        }
    }

    async fn destroy(mut self) -> Result<(), Error> {
        while let Some((source, interface)) = self.forwards.pop_first() {
            unforward(interface.as_str(), source, self.target).await?;
        }
        Ok(())
    }

    async fn update(
        &mut self,
        ip_info: &OrdMap<GatewayId, NetworkInterfaceInfo>,
        filter: Option<DynInterfaceFilter>,
    ) -> Result<(), Error> {
        if self.rc.strong_count() == 0 {
            return self.take().destroy().await;
        }
        let filter_ref = filter.as_ref().unwrap_or(&self.prev_filter);
        let mut keep = BTreeSet::<SocketAddr>::new();
        for (iface, info) in ip_info
            .iter()
            // .chain([NetworkInterfaceInfo::loopback()])
            .filter(|(id, info)| filter_ref.filter(*id, *info))
        {
            if let Some(ip_info) = &info.ip_info {
                for ipnet in &ip_info.subnets {
                    let addr = match ipnet.addr() {
                        IpAddr::V6(ip6) => SocketAddrV6::new(
                            ip6,
                            self.external,
                            0,
                            if ipv6_is_link_local(ip6) {
                                ip_info.scope_id
                            } else {
                                0
                            },
                        )
                        .into(),
                        ip => SocketAddr::new(ip, self.external),
                    };
                    keep.insert(addr);
                    if !self.forwards.contains_key(&addr) {
                        forward(iface.as_str(), addr, self.target).await?;
                        self.forwards.insert(addr, iface.clone());
                    }
                }
            }
        }
        let rm = self
            .forwards
            .keys()
            .copied()
            .filter(|a| !keep.contains(a))
            .collect::<Vec<_>>();
        for rm in rm {
            if let Some((source, interface)) = self.forwards.remove_entry(&rm) {
                unforward(interface.as_str(), source, self.target).await?;
            }
        }
        if let Some(filter) = filter {
            self.prev_filter = filter;
        }
        Ok(())
    }

    async fn update_request(
        &mut self,
        ForwardRequest {
            external,
            target,
            filter,
            rc,
        }: ForwardRequest,
        ip_info: &OrdMap<GatewayId, NetworkInterfaceInfo>,
    ) -> Result<(), Error> {
        if external != self.external || target != self.target {
            self.take().destroy().await?;
            *self = Self::new(external, target, rc);
            self.update(ip_info, Some(filter)).await?;
        } else {
            self.rc = rc;
            self.update(ip_info, Some(filter).filter(|f| f != &self.prev_filter))
                .await?;
        }
        Ok(())
    }
}
impl Drop for ForwardEntry {
    fn drop(&mut self) {
        if !self.forwards.is_empty() {
            let take = self.take();
            tokio::spawn(async move {
                take.destroy().await.log_err();
            });
        }
    }
}

#[derive(Default, Clone)]
struct ForwardState {
    state: BTreeMap<u16, ForwardEntry>,
}
impl ForwardState {
    async fn handle_request(
        &mut self,
        request: ForwardRequest,
        ip_info: &OrdMap<GatewayId, NetworkInterfaceInfo>,
    ) -> Result<(), Error> {
        self.state
            .entry(request.external)
            .or_insert_with(|| ForwardEntry::new(request.external, request.target, Weak::new()))
            .update_request(request, ip_info)
            .await
    }
    async fn sync(
        &mut self,
        ip_info: &OrdMap<GatewayId, NetworkInterfaceInfo>,
    ) -> Result<(), Error> {
        for entry in self.state.values_mut() {
            entry.update(ip_info, None).await?;
        }
        self.state.retain(|_, fwd| fwd.rc.strong_count() > 0);
        Ok(())
    }
}

fn err_has_exited<T>(_: T) -> Error {
    Error::new(
        eyre!("PortForwardController thread has exited"),
        ErrorKind::Unknown,
    )
}

#[derive(Debug, Clone, Deserialize, Serialize)]
pub struct ForwardTable(pub BTreeMap<u16, ForwardTarget>);
#[derive(Debug, Clone, Deserialize, Serialize)]
pub struct ForwardTarget {
    pub target: SocketAddr,
    pub filter: String,
    pub gateways: BTreeMap<SocketAddr, GatewayId>,
}
impl From<&ForwardState> for ForwardTable {
    fn from(value: &ForwardState) -> Self {
        Self(
            value
                .state
                .iter()
                .map(|(external, entry)| {
                    (
                        *external,
                        ForwardTarget {
                            target: entry.target,
                            filter: format!("{:?}", entry.prev_filter),
                            gateways: entry.forwards.clone(),
                        },
                    )
                })
                .collect(),
        )
    }
}

enum ForwardCommand {
    Forward(ForwardRequest, oneshot::Sender<Result<(), Error>>),
    Sync(oneshot::Sender<Result<(), Error>>),
    DumpTable(oneshot::Sender<ForwardTable>),
}

#[test]
fn test() {
    assert_ne!(
        false.into_dyn(),
        SecureFilter { secure: false }.into_dyn().into_dyn()
    );
}

pub struct PortForwardController {
    req: mpsc::UnboundedSender<ForwardCommand>,
    _thread: NonDetachingJoinHandle<()>,
}
impl PortForwardController {
    pub fn new(mut ip_info: Watch<OrdMap<GatewayId, NetworkInterfaceInfo>>) -> Self {
        let (req_send, mut req_recv) = mpsc::unbounded_channel::<ForwardCommand>();
        let thread = NonDetachingJoinHandle::from(tokio::spawn(async move {
            let mut state = ForwardState::default();
            let mut interfaces = ip_info.read_and_mark_seen();
            loop {
                tokio::select! {
                    msg = req_recv.recv() => {
                        if let Some(cmd) = msg {
                            match cmd {
                                ForwardCommand::Forward(req, re) => re.send(state.handle_request(req, &interfaces).await).ok(),
                                ForwardCommand::Sync(re) => re.send(state.sync(&interfaces).await).ok(),
                                ForwardCommand::DumpTable(re) => re.send((&state).into()).ok(),
                            };
                        } else {
                            break;
                        }
                    }
                    _ = ip_info.changed() => {
                        interfaces = ip_info.read();
                        state.sync(&interfaces).await.log_err();
                    }
                }
            }
        }));
        Self {
            req: req_send,
            _thread: thread,
        }
    }
    pub async fn add(
        &self,
        external: u16,
        filter: DynInterfaceFilter,
        target: SocketAddr,
    ) -> Result<Arc<()>, Error> {
        let rc = Arc::new(());
        let (send, recv) = oneshot::channel();
        self.req
            .send(ForwardCommand::Forward(
                ForwardRequest {
                    external,
                    target,
                    filter,
                    rc: Arc::downgrade(&rc),
                },
                send,
            ))
            .map_err(err_has_exited)?;

        recv.await.map_err(err_has_exited)?.map(|_| rc)
    }
    pub async fn gc(&self) -> Result<(), Error> {
        let (send, recv) = oneshot::channel();
        self.req
            .send(ForwardCommand::Sync(send))
            .map_err(err_has_exited)?;

        recv.await.map_err(err_has_exited)?
    }
    pub async fn dump_table(&self) -> Result<ForwardTable, Error> {
        let (req, res) = oneshot::channel();
        self.req
            .send(ForwardCommand::DumpTable(req))
            .map_err(err_has_exited)?;
        res.await.map_err(err_has_exited)
    }
}

async fn forward(interface: &str, source: SocketAddr, target: SocketAddr) -> Result<(), Error> {
    if source.is_ipv6() {
        return Ok(()); // TODO: socat? ip6tables?
    }
    Command::new("/usr/lib/startos/scripts/forward-port")
        .env("iiface", interface)
        .env("oiface", START9_BRIDGE_IFACE)
        .env("sip", source.ip().to_string())
        .env("dip", target.ip().to_string())
        .env("sport", source.port().to_string())
        .env("dport", target.port().to_string())
        .invoke(ErrorKind::Network)
        .await?;
    Ok(())
}

async fn unforward(interface: &str, source: SocketAddr, target: SocketAddr) -> Result<(), Error> {
    if source.is_ipv6() {
        return Ok(()); // TODO: socat? ip6tables?
    }
    Command::new("/usr/lib/startos/scripts/forward-port")
        .env("UNDO", "1")
        .env("iiface", interface)
        .env("oiface", START9_BRIDGE_IFACE)
        .env("sip", source.ip().to_string())
        .env("dip", target.ip().to_string())
        .env("sport", source.port().to_string())
        .env("dport", target.port().to_string())
        .invoke(ErrorKind::Network)
        .await?;
    Ok(())
}
