use std::collections::{BTreeMap, BTreeSet};
use std::net::{IpAddr, SocketAddr, SocketAddrV6};
use std::sync::{Arc, Weak};

use futures::channel::oneshot;
use helpers::NonDetachingJoinHandle;
use id_pool::IdPool;
use iddqd::{IdOrdItem, IdOrdMap};
use imbl::OrdMap;
use models::GatewayId;
use rpc_toolkit::{Context, HandlerArgs, HandlerExt, ParentHandler, from_fn_async};
use serde::{Deserialize, Serialize};
use tokio::process::Command;
use tokio::sync::mpsc;

use crate::context::{CliContext, RpcContext};
use crate::db::model::public::NetworkInterfaceInfo;
use crate::net::gateway::{DynInterfaceFilter, InterfaceFilter};
use crate::net::utils::ipv6_is_link_local;
use crate::prelude::*;
use crate::util::Invoke;
use crate::util::serde::{HandlerExtSerde, display_serializable};
use crate::util::sync::Watch;

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

struct ForwardMapping {
    source: SocketAddr,
    target: SocketAddr,
    interface: GatewayId,
    rc: Weak<()>,
}

#[derive(Default)]
struct PortForwardState {
    mappings: BTreeMap<SocketAddr, ForwardMapping>, // source -> target
}

impl PortForwardState {
    async fn add_forward(
        &mut self,
        interface: GatewayId,
        source: SocketAddr,
        target: SocketAddr,
        rc: Arc<()>,
    ) -> Result<Arc<()>, Error> {
        // Check if mapping already exists
        if let Some(existing) = self.mappings.get_mut(&source) {
            // If it's the same target and interface, just update the reference
            if existing.target == target && existing.interface == interface {
                if let Some(existing_rc) = existing.rc.upgrade() {
                    return Ok(existing_rc);
                } else {
                    existing.rc = Arc::downgrade(&rc);
                    return Ok(rc);
                }
            } else {
                // Different target/interface, need to remove old and add new
                if let Some(mapping) = self.mappings.remove(&source) {
                    unforward(mapping.interface.as_str(), mapping.source, mapping.target).await?;
                }
            }
        }

        // Add the new forward
        forward(interface.as_str(), source, target).await?;
        self.mappings.insert(
            source,
            ForwardMapping {
                source,
                target,
                interface: interface.clone(),
                rc: Arc::downgrade(&rc),
            },
        );

        Ok(rc)
    }

    async fn gc(&mut self) -> Result<(), Error> {
        let to_remove: Vec<SocketAddr> = self
            .mappings
            .iter()
            .filter(|(_, mapping)| mapping.rc.strong_count() == 0)
            .map(|(source, _)| *source)
            .collect();

        for source in to_remove {
            if let Some(mapping) = self.mappings.remove(&source) {
                unforward(mapping.interface.as_str(), mapping.source, mapping.target).await?;
            }
        }
        Ok(())
    }

    fn dump(&self) -> BTreeMap<SocketAddr, SocketAddr> {
        self.mappings
            .iter()
            .filter(|(_, mapping)| mapping.rc.strong_count() > 0)
            .map(|(source, mapping)| (*source, mapping.target))
            .collect()
    }
}

impl Drop for PortForwardState {
    fn drop(&mut self) {
        if !self.mappings.is_empty() {
            let mappings = std::mem::take(&mut self.mappings);
            tokio::spawn(async move {
                for (_, mapping) in mappings {
                    unforward(mapping.interface.as_str(), mapping.source, mapping.target)
                        .await
                        .log_err();
                }
            });
        }
    }
}

enum PortForwardCommand {
    AddForward {
        interface: GatewayId,
        source: SocketAddr,
        target: SocketAddr,
        rc: Arc<()>,
        respond: oneshot::Sender<Result<Arc<()>, Error>>,
    },
    Gc {
        respond: oneshot::Sender<Result<(), Error>>,
    },
    Dump {
        respond: oneshot::Sender<BTreeMap<SocketAddr, SocketAddr>>,
    },
}

pub struct PortForwardController {
    req: mpsc::UnboundedSender<PortForwardCommand>,
    _thread: NonDetachingJoinHandle<()>,
}

impl PortForwardController {
    pub fn new() -> Self {
        let (req_send, mut req_recv) = mpsc::unbounded_channel::<PortForwardCommand>();
        let thread = NonDetachingJoinHandle::from(tokio::spawn(async move {
            let mut state = PortForwardState::default();
            while let Some(cmd) = req_recv.recv().await {
                match cmd {
                    PortForwardCommand::AddForward {
                        interface,
                        source,
                        target,
                        rc,
                        respond,
                    } => {
                        let result = state.add_forward(interface, source, target, rc).await;
                        respond.send(result).ok();
                    }
                    PortForwardCommand::Gc { respond } => {
                        let result = state.gc().await;
                        respond.send(result).ok();
                    }
                    PortForwardCommand::Dump { respond } => {
                        respond.send(state.dump()).ok();
                    }
                }
            }
        }));

        Self {
            req: req_send,
            _thread: thread,
        }
    }

    pub async fn add_forward(
        &self,
        interface: GatewayId,
        source: SocketAddr,
        target: SocketAddr,
    ) -> Result<Arc<()>, Error> {
        let rc = Arc::new(());
        let (send, recv) = oneshot::channel();
        self.req
            .send(PortForwardCommand::AddForward {
                interface,
                source,
                target,
                rc,
                respond: send,
            })
            .map_err(err_has_exited)?;

        recv.await.map_err(err_has_exited)?
    }

    pub async fn gc(&self) -> Result<(), Error> {
        let (send, recv) = oneshot::channel();
        self.req
            .send(PortForwardCommand::Gc { respond: send })
            .map_err(err_has_exited)?;

        recv.await.map_err(err_has_exited)?
    }

    pub async fn dump(&self) -> Result<BTreeMap<SocketAddr, SocketAddr>, Error> {
        let (send, recv) = oneshot::channel();
        self.req
            .send(PortForwardCommand::Dump { respond: send })
            .map_err(err_has_exited)?;

        recv.await.map_err(err_has_exited)
    }
}

struct InterfaceForwardRequest {
    external: u16,
    target: SocketAddr,
    filter: DynInterfaceFilter,
    rc: Arc<()>,
}

#[derive(Clone)]
struct InterfaceForwardEntry {
    external: u16,
    filter: BTreeMap<DynInterfaceFilter, (SocketAddr, Weak<()>)>,
    // Maps source SocketAddr -> strong reference for the forward created in PortForwardController
    forwards: BTreeMap<SocketAddr, Arc<()>>,
}

impl IdOrdItem for InterfaceForwardEntry {
    type Key<'a> = u16;
    fn key(&self) -> Self::Key<'_> {
        self.external
    }

    iddqd::id_upcast!();
}

impl InterfaceForwardEntry {
    fn new(external: u16) -> Self {
        Self {
            external,
            filter: BTreeMap::new(),
            forwards: BTreeMap::new(),
        }
    }

    async fn update(
        &mut self,
        ip_info: &OrdMap<GatewayId, NetworkInterfaceInfo>,
        port_forward: &PortForwardController,
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
                            let rc = port_forward
                                .add_forward(iface.clone(), addr, target)
                                .await?;
                            self.forwards.insert(addr, rc);
                        }
                    }
                }
            }
        }

        // Remove forwards that should no longer exist (drops the strong references)
        self.forwards.retain(|addr, _| keep.contains(addr));

        Ok(())
    }

    async fn update_request(
        &mut self,
        InterfaceForwardRequest {
            external,
            target,
            filter,
            mut rc,
        }: InterfaceForwardRequest,
        ip_info: &OrdMap<GatewayId, NetworkInterfaceInfo>,
        port_forward: &PortForwardController,
    ) -> Result<Arc<()>, Error> {
        if external != self.external {
            return Err(Error::new(
                eyre!("Mismatched external port in InterfaceForwardEntry"),
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

        self.update(ip_info, port_forward).await?;

        Ok(rc)
    }

    async fn gc(
        &mut self,
        ip_info: &OrdMap<GatewayId, NetworkInterfaceInfo>,
        port_forward: &PortForwardController,
    ) -> Result<(), Error> {
        // Clean up dead filter references
        self.filter.retain(|_, (_, rc)| rc.strong_count() > 0);

        // Update to add/remove forwards based on current state (this will drop strong references as needed)
        self.update(ip_info, port_forward).await
    }
}

struct InterfaceForwardState {
    port_forward: PortForwardController,
    state: IdOrdMap<InterfaceForwardEntry>,
}

impl InterfaceForwardState {
    fn new(port_forward: PortForwardController) -> Self {
        Self {
            port_forward,
            state: IdOrdMap::new(),
        }
    }
}

impl InterfaceForwardState {
    async fn handle_request(
        &mut self,
        request: InterfaceForwardRequest,
        ip_info: &OrdMap<GatewayId, NetworkInterfaceInfo>,
    ) -> Result<Arc<()>, Error> {
        self.state
            .entry(request.external)
            .or_insert_with(|| InterfaceForwardEntry::new(request.external))
            .update_request(request, ip_info, &self.port_forward)
            .await
    }

    async fn sync(
        &mut self,
        ip_info: &OrdMap<GatewayId, NetworkInterfaceInfo>,
    ) -> Result<(), Error> {
        for mut entry in self.state.iter_mut() {
            entry.gc(ip_info, &self.port_forward).await?;
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

impl From<&InterfaceForwardState> for ForwardTable {
    fn from(value: &InterfaceForwardState) -> Self {
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

enum InterfaceForwardCommand {
    Forward(
        InterfaceForwardRequest,
        oneshot::Sender<Result<Arc<()>, Error>>,
    ),
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

pub struct InterfacePortForwardController {
    req: mpsc::UnboundedSender<InterfaceForwardCommand>,
    _thread: NonDetachingJoinHandle<()>,
}

impl InterfacePortForwardController {
    pub fn new(mut ip_info: Watch<OrdMap<GatewayId, NetworkInterfaceInfo>>) -> Self {
        let port_forward = PortForwardController::new();

        let (req_send, mut req_recv) = mpsc::unbounded_channel::<InterfaceForwardCommand>();
        let thread = NonDetachingJoinHandle::from(tokio::spawn(async move {
            let mut state = InterfaceForwardState::new(port_forward);
            let mut interfaces = ip_info.read_and_mark_seen();
            loop {
                tokio::select! {
                    msg = req_recv.recv() => {
                        if let Some(cmd) = msg {
                            match cmd {
                                InterfaceForwardCommand::Forward(req, re) => {
                                    re.send(state.handle_request(req, &interfaces).await).ok()
                                }
                                InterfaceForwardCommand::Sync(re) => {
                                    re.send(state.sync(&interfaces).await).ok()
                                }
                                InterfaceForwardCommand::DumpTable(re) => {
                                    re.send((&state).into()).ok()
                                }
                            };
                        } else {
                            break;
                        }
                    }
                    _ = ip_info.changed() => {
                        interfaces = ip_info.read();
                        state.sync(&interfaces).await.log_err();
                        state.port_forward.gc().await.log_err();
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
            .send(InterfaceForwardCommand::Forward(
                InterfaceForwardRequest {
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
            .send(InterfaceForwardCommand::Sync(send))
            .map_err(err_has_exited)?;

        recv.await.map_err(err_has_exited)?
    }

    pub async fn dump_table(&self) -> Result<ForwardTable, Error> {
        let (req, res) = oneshot::channel();
        self.req
            .send(InterfaceForwardCommand::DumpTable(req))
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
