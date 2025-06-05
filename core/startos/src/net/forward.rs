use std::collections::{BTreeMap, BTreeSet};
use std::net::{IpAddr, Ipv4Addr, SocketAddr};
use std::sync::{Arc, Weak};

use futures::channel::oneshot;
use helpers::NonDetachingJoinHandle;
use id_pool::IdPool;
use imbl_value::InternedString;
use serde::{Deserialize, Serialize};
use tokio::process::Command;
use tokio::sync::mpsc;

use crate::db::model::public::NetworkInterfaceInfo;
use crate::prelude::*;
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

#[derive(Debug)]
struct ForwardRequest {
    public: bool,
    target: SocketAddr,
    rc: Weak<()>,
}

#[derive(Debug, Default)]
struct ForwardState {
    requested: BTreeMap<u16, ForwardRequest>,
    current: BTreeMap<u16, BTreeMap<InternedString, SocketAddr>>,
}
impl ForwardState {
    async fn sync(
        &mut self,
        interfaces: &BTreeMap<InternedString, (bool, Vec<Ipv4Addr>)>,
    ) -> Result<(), Error> {
        let private_interfaces = interfaces
            .iter()
            .filter(|(_, (public, _))| !*public)
            .map(|(i, _)| i)
            .collect::<BTreeSet<_>>();
        let all_interfaces = interfaces.keys().collect::<BTreeSet<_>>();
        self.requested.retain(|_, req| req.rc.strong_count() > 0);
        for external in self
            .requested
            .keys()
            .chain(self.current.keys())
            .copied()
            .collect::<BTreeSet<_>>()
        {
            match (
                self.requested.get(&external),
                self.current.get_mut(&external),
            ) {
                (Some(req), Some(cur)) => {
                    let expected = if req.public {
                        &all_interfaces
                    } else {
                        &private_interfaces
                    };
                    let actual = cur.keys().collect::<BTreeSet<_>>();
                    let mut to_rm = actual
                        .difference(expected)
                        .copied()
                        .map(|i| (i.clone(), &interfaces[i].1))
                        .collect::<BTreeMap<_, _>>();
                    let mut to_add = expected
                        .difference(&actual)
                        .copied()
                        .map(|i| (i.clone(), &interfaces[i].1))
                        .collect::<BTreeMap<_, _>>();
                    for interface in actual.intersection(expected).copied() {
                        if cur[interface] != req.target {
                            to_rm.insert(interface.clone(), &interfaces[interface].1);
                            to_add.insert(interface.clone(), &interfaces[interface].1);
                        }
                    }
                    for (interface, ips) in to_rm {
                        for ip in ips {
                            unforward(&*interface, (*ip, external).into(), cur[&interface]).await?;
                        }
                        cur.remove(&interface);
                    }
                    for (interface, ips) in to_add {
                        cur.insert(interface.clone(), req.target);
                        for ip in ips {
                            forward(&*interface, (*ip, external).into(), cur[&interface]).await?;
                        }
                    }
                }
                (Some(req), None) => {
                    let cur = self.current.entry(external).or_default();
                    for interface in if req.public {
                        &all_interfaces
                    } else {
                        &private_interfaces
                    }
                    .into_iter()
                    .copied()
                    {
                        cur.insert(interface.clone(), req.target);
                        for ip in &interfaces[interface].1 {
                            forward(&**interface, (*ip, external).into(), req.target).await?;
                        }
                    }
                }
                (None, Some(cur)) => {
                    let to_rm = cur.keys().cloned().collect::<BTreeSet<_>>();
                    for interface in to_rm {
                        for ip in &interfaces[&interface].1 {
                            unforward(&*interface, (*ip, external).into(), cur[&interface]).await?;
                        }
                        cur.remove(&interface);
                    }
                    self.current.remove(&external);
                }
                _ => (),
            }
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

pub struct LanPortForwardController {
    req: mpsc::UnboundedSender<(
        Option<(u16, ForwardRequest)>,
        oneshot::Sender<Result<(), Error>>,
    )>,
    _thread: NonDetachingJoinHandle<()>,
}
impl LanPortForwardController {
    pub fn new(mut ip_info: Watch<BTreeMap<InternedString, NetworkInterfaceInfo>>) -> Self {
        let (req_send, mut req_recv) = mpsc::unbounded_channel();
        let thread = NonDetachingJoinHandle::from(tokio::spawn(async move {
            let mut state = ForwardState::default();
            let mut interfaces = ip_info.peek_and_mark_seen(|ip_info| {
                ip_info
                    .iter()
                    .map(|(iface, info)| {
                        (
                            iface.clone(),
                            (
                                info.inbound(),
                                info.ip_info.as_ref().map_or(Vec::new(), |i| {
                                    i.subnets
                                        .iter()
                                        .filter_map(|s| {
                                            if let IpAddr::V4(ip) = s.addr() {
                                                Some(ip)
                                            } else {
                                                None
                                            }
                                        })
                                        .collect()
                                }),
                            ),
                        )
                    })
                    .collect()
            });
            let mut reply: Option<oneshot::Sender<Result<(), Error>>> = None;
            loop {
                tokio::select! {
                    msg = req_recv.recv() => {
                        if let Some((msg, re)) = msg {
                            if let Some((external, req)) = msg {
                                state.requested.insert(external, req);
                            }
                            reply = Some(re);
                        } else {
                            break;
                        }
                    }
                    _ = ip_info.changed() => {
                        interfaces = ip_info.peek(|ip_info| {
                            ip_info
                                .iter()
                                .map(|(iface, info)| (iface.clone(), (
                                    info.inbound(),
                                    info.ip_info.as_ref().map_or(Vec::new(), |i| {
                                        i.subnets
                                            .iter()
                                            .filter_map(|s| {
                                                if let IpAddr::V4(ip) = s.addr() {
                                                    Some(ip)
                                                } else {
                                                    None
                                                }
                                            })
                                            .collect()
                                    }),
                                )))
                                .collect()
                        });
                    }
                }
                let res = state.sync(&interfaces).await;
                if let Err(e) = &res {
                    tracing::error!("Error in PortForwardController: {e}");
                    tracing::debug!("{e:?}");
                }
                if let Some(re) = reply.take() {
                    let _ = re.send(res);
                }
            }
        }));
        Self {
            req: req_send,
            _thread: thread,
        }
    }
    pub async fn add(&self, port: u16, public: bool, target: SocketAddr) -> Result<Arc<()>, Error> {
        let rc = Arc::new(());
        let (send, recv) = oneshot::channel();
        self.req
            .send((
                Some((
                    port,
                    ForwardRequest {
                        public,
                        target,
                        rc: Arc::downgrade(&rc),
                    },
                )),
                send,
            ))
            .map_err(err_has_exited)?;

        recv.await.map_err(err_has_exited)?.map(|_| rc)
    }
    pub async fn gc(&self) -> Result<(), Error> {
        let (send, recv) = oneshot::channel();
        self.req.send((None, send)).map_err(err_has_exited)?;

        recv.await.map_err(err_has_exited)?
    }
}

async fn forward(interface: &str, source: SocketAddr, target: SocketAddr) -> Result<(), Error> {
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
