use std::collections::{BTreeMap, BTreeSet};
use std::net::{IpAddr, SocketAddr, SocketAddrV6};
use std::sync::{Arc, Weak};

use futures::channel::oneshot;
use helpers::NonDetachingJoinHandle;
use id_pool::IdPool;
use iddqd::{IdOrdItem, IdOrdMap};
use imbl::OrdMap;
use models::GatewayId;
use rpc_toolkit::{from_fn_async, Context, HandlerArgs, HandlerExt, ParentHandler};
use serde::{Deserialize, Serialize};
use tokio::process::Command;
use tokio::sync::mpsc;

use crate::context::{CliContext, RpcContext};
use crate::db::model::public::NetworkInterfaceInfo;
use crate::net::gateway::{DynInterfaceFilter, InterfaceFilter};
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
            table.add_row(row![bc => "FROM", "TO", "FILTER"]);

            for (external, target) in res.0 {
                table.add_row(row![external, target.target, target.filter]);
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
    rc: Arc<()>,
}

#[derive(Clone)]
struct ForwardEntry {
    external: u16,
    filter: BTreeMap<DynInterfaceFilter, (SocketAddr, Weak<()>)>,
    forwards: BTreeMap<SocketAddr, (GatewayId, SocketAddr)>,
}
impl IdOrdItem for ForwardEntry {
    type Key<'a> = u16;
    fn key(&self) -> Self::Key<'_> {
        self.external
    }

    iddqd::id_upcast!();
}
impl ForwardEntry {
    fn new(external: u16) -> Self {
        Self {
            external,
            filter: BTreeMap::new(),
            forwards: BTreeMap::new(),
        }
    }

    fn take(&mut self) -> Self {
        Self {
            external: self.external,
            filter: std::mem::take(&mut self.filter),
            forwards: std::mem::take(&mut self.forwards),
        }
    }

    async fn destroy(mut self) -> Result<(), Error> {
        while let Some((source, (interface, target))) = self.forwards.pop_first() {
            unforward(interface.as_str(), source, target).await?;
        }
        Ok(())
    }

    async fn update(
        &mut self,
        ip_info: &OrdMap<GatewayId, NetworkInterfaceInfo>,
    ) -> Result<(), Error> {
        let mut keep = BTreeSet::<SocketAddr>::new();
        for (iface, info) in ip_info.iter() {
            if let Some(target) = self
                .filter
                .iter()
                .filter(|(_, (_, rc))| rc.strong_count() > 0)
                .find(|(filter, _)| filter.filter(iface, info))
                .map(|(_, (target, _))| *target)
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
                            forward(iface.as_str(), addr, target).await?;
                            self.forwards.insert(addr, (iface.clone(), target));
                        }
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
            if let Some((source, (interface, target))) = self.forwards.remove_entry(&rm) {
                unforward(interface.as_str(), source, target).await?;
            }
        }
        Ok(())
    }

    async fn update_request(
        &mut self,
        ForwardRequest {
            external,
            target,
            filter,
            mut rc,
        }: ForwardRequest,
        ip_info: &OrdMap<GatewayId, NetworkInterfaceInfo>,
    ) -> Result<Arc<()>, Error> {
        if external != self.external {
            return Err(Error::new(
                eyre!("Mismatched external port in ForwardEntry"),
                ErrorKind::InvalidRequest,
            ));
        }

        let entry = self
            .filter
            .entry(filter)
            .or_insert_with(|| (target, Arc::downgrade(&rc)));
        if entry.0 != target {
            entry.0 = target;
            entry.1 = Arc::downgrade(&rc);
        }
        if let Some(existing) = entry.1.upgrade() {
            rc = existing;
        } else {
            entry.1 = Arc::downgrade(&rc);
        }

        self.update(ip_info).await?;

        Ok(rc)
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
    state: IdOrdMap<ForwardEntry>,
}
impl ForwardState {
    async fn handle_request(
        &mut self,
        request: ForwardRequest,
        ip_info: &OrdMap<GatewayId, NetworkInterfaceInfo>,
    ) -> Result<Arc<()>, Error> {
        self.state
            .entry(request.external)
            .or_insert_with(|| ForwardEntry::new(request.external))
            .update_request(request, ip_info)
            .await
    }
    async fn sync(
        &mut self,
        ip_info: &OrdMap<GatewayId, NetworkInterfaceInfo>,
    ) -> Result<(), Error> {
        for mut entry in self.state.iter_mut() {
            entry.update(ip_info).await?;
        }
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
}
impl From<&ForwardState> for ForwardTable {
    fn from(value: &ForwardState) -> Self {
        Self(
            value
                .state
                .iter()
                .flat_map(|entry| {
                    entry
                        .filter
                        .iter()
                        .filter(|(_, (_, rc))| rc.strong_count() > 0)
                        .map(|(filter, (target, _))| {
                            (
                                entry.external,
                                ForwardTarget {
                                    target: *target,
                                    filter: format!("{:?}", filter),
                                },
                            )
                        })
                })
                .collect(),
        )
    }
}

enum ForwardCommand {
    Forward(ForwardRequest, oneshot::Sender<Result<Arc<()>, Error>>),
    Sync(oneshot::Sender<Result<(), Error>>),
    DumpTable(oneshot::Sender<ForwardTable>),
}

#[test]
fn test() {
    use crate::net::gateway::SecureFilter;

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
                    rc,
                },
                send,
            ))
            .map_err(err_has_exited)?;

        recv.await.map_err(err_has_exited)?
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
    if interface == START9_BRIDGE_IFACE {
        return Ok(());
    }
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
    if interface == START9_BRIDGE_IFACE {
        return Ok(());
    }
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
