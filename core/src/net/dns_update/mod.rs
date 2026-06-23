//! Best-effort RFC 2136 (DNS UPDATE) client. When a private domain is enabled
//! on a gateway, push an `A` record (`domain -> this host's IP on that
//! gateway's subnet`) to the gateway's DNS server so other LAN devices can
//! resolve it; withdraw it when the domain is disabled/deleted.
//!
//! The gateway authorizes updates by source IP (a per-device "allow DNS
//! injection" toggle on StartTunnel / StartWRT), so updates are sent unsigned
//! and bound to our address on that gateway. Everything is best-effort: a
//! gateway that doesn't accept RFC 2136 just means the domain only resolves on
//! StartOS's own resolver, as before. Mirrors the `add`/`gc` shape of
//! [`crate::net::dns`] and reconciles like [`crate::net::forward`].

pub mod rfc2136;

use std::collections::{BTreeMap, BTreeSet};
use std::net::{IpAddr, Ipv4Addr, SocketAddr};
use std::time::Duration;

use hickory_server::proto::op::update_message::{append, delete_rrset};
use hickory_server::proto::op::{Message, ResponseCode};
use hickory_server::proto::rr::rdata::A;
use hickory_server::proto::rr::{Name, RData, Record, RecordSet, RecordType};
use imbl::OrdMap;
use imbl_value::InternedString;
use tokio::net::UdpSocket;
use tokio::sync::mpsc;
use tokio::time::{interval, timeout};

use crate::db::model::public::NetworkInterfaceInfo;
use crate::net::port_map::candidate_gateways;
use crate::prelude::*;
use crate::util::sync::Watch;
use crate::GatewayId;

const DNS_PORT: u16 = 53;
const RECORD_TTL: u32 = 300;
const QUERY_TIMEOUT: Duration = Duration::from_secs(2);
/// Re-assert desired records so they survive a gateway DNS restart.
const REFRESH_INTERVAL: Duration = Duration::from_secs(180);

/// (DNS server to update, our address on that subnet -> the A record value/source).
type Target = (Ipv4Addr, Ipv4Addr);

enum Command {
    Add {
        fqdn: InternedString,
        gateways: BTreeSet<GatewayId>,
    },
    Gc {
        rm: BTreeSet<InternedString>,
    },
}

/// Mirrors [`crate::net::dns::DnsController`]'s private-domain API so the
/// net-service reconcile can drive both in lock-step.
#[derive(Clone)]
pub struct DnsUpdateController {
    req: mpsc::UnboundedSender<Command>,
}

impl DnsUpdateController {
    pub fn new(mut net_iface: Watch<OrdMap<GatewayId, NetworkInterfaceInfo>>) -> Self {
        let (req, mut recv) = mpsc::unbounded_channel::<Command>();
        tokio::spawn(async move {
            // fqdn -> gateways it should be published on.
            let mut desired: BTreeMap<InternedString, BTreeSet<GatewayId>> = BTreeMap::new();
            // fqdn -> targets currently published (so we can withdraw stale ones).
            let mut active: BTreeMap<InternedString, BTreeSet<Target>> = BTreeMap::new();
            let mut ifaces = net_iface.read_and_mark_seen();
            let mut refresh = interval(REFRESH_INTERVAL);
            refresh.set_missed_tick_behavior(tokio::time::MissedTickBehavior::Skip);
            loop {
                tokio::select! {
                    cmd = recv.recv() => match cmd {
                        Some(Command::Add { fqdn, gateways }) => {
                            let changed = desired.get(&fqdn) != Some(&gateways);
                            desired.insert(fqdn.clone(), gateways);
                            if changed {
                                reconcile_one(&fqdn, &desired, &mut active, &ifaces).await;
                            }
                        }
                        Some(Command::Gc { rm }) => {
                            for fqdn in rm {
                                desired.remove(&fqdn);
                                reconcile_one(&fqdn, &desired, &mut active, &ifaces).await;
                            }
                        }
                        None => break,
                    },
                    _ = net_iface.changed() => {
                        ifaces = net_iface.read();
                        for fqdn in desired.keys().cloned().collect::<Vec<_>>() {
                            reconcile_one(&fqdn, &desired, &mut active, &ifaces).await;
                        }
                    }
                    _ = refresh.tick() => {
                        // Re-assert everything (cheap, idempotent at the server).
                        for (fqdn, targets) in &active {
                            if let Ok(name) = fqdn_to_name(fqdn) {
                                for (server, ip) in targets {
                                    apply(&name, *server, *ip).await;
                                }
                            }
                        }
                    }
                }
            }
        });
        Self { req }
    }

    pub fn add(&self, fqdn: InternedString, gateways: BTreeSet<GatewayId>) {
        self.req.send(Command::Add { fqdn, gateways }).ok();
    }

    pub fn gc(&self, rm: BTreeSet<InternedString>) {
        if !rm.is_empty() {
            self.req.send(Command::Gc { rm }).ok();
        }
    }
}

/// The (resolver, our-ip) pairs for a private domain on one gateway: for each
/// of our IPv4 subnets, the record points at our address on that subnet and is
/// sent to that subnet's resolver (its NM gateway / `.1`).
fn targets_for(info: &NetworkInterfaceInfo) -> Vec<Target> {
    let Some(ip_info) = &info.ip_info else {
        return Vec::new();
    };
    let resolvers = candidate_gateways(info);
    let mut out = Vec::new();
    for subnet in &ip_info.subnets {
        let IpAddr::V4(our_ip) = subnet.addr() else {
            continue;
        };
        // Prefer a resolver on the same subnet as our address.
        let server = resolvers
            .iter()
            .copied()
            .find(|r| subnet.contains(&IpAddr::V4(*r)))
            .or_else(|| match subnet.hosts().next() {
                Some(IpAddr::V4(v4)) => Some(v4),
                _ => None,
            });
        if let Some(server) = server {
            out.push((server, our_ip));
        }
    }
    out
}

async fn reconcile_one(
    fqdn: &InternedString,
    desired: &BTreeMap<InternedString, BTreeSet<GatewayId>>,
    active: &mut BTreeMap<InternedString, BTreeSet<Target>>,
    ifaces: &OrdMap<GatewayId, NetworkInterfaceInfo>,
) {
    let Ok(name) = fqdn_to_name(fqdn) else {
        return;
    };
    let want: BTreeSet<Target> = desired
        .get(fqdn)
        .into_iter()
        .flatten()
        .filter_map(|gw| ifaces.get(gw))
        .flat_map(targets_for)
        .collect();
    let had = active.remove(fqdn).unwrap_or_default();
    for (server, ip) in had.difference(&want) {
        withdraw(&name, *server, *ip).await;
    }
    for (server, ip) in &want {
        apply(&name, *server, *ip).await;
    }
    if !want.is_empty() {
        active.insert(fqdn.clone(), want);
    }
}

fn fqdn_to_name(fqdn: &str) -> Result<Name, Error> {
    let mut name = Name::from_utf8(fqdn).with_kind(ErrorKind::ParseUrl)?;
    name.set_fqdn(true);
    Ok(name)
}

/// RFC 2136 updates use the query section as the zone; the parent of the FQDN
/// is a valid zone origin (and our own servers don't enforce it strictly).
fn zone_of(fqdn: &Name) -> Name {
    let base = fqdn.base_name();
    if base.is_root() {
        fqdn.clone()
    } else {
        base
    }
}

async fn apply(fqdn: &Name, server: Ipv4Addr, ip: Ipv4Addr) {
    let zone = zone_of(fqdn);
    // Replace: drop any existing A rrset for the name, then add ours.
    let delete = delete_rrset(
        Record::update0(fqdn.clone(), 0, RecordType::A),
        zone.clone(),
        false,
    );
    let mut rrset = RecordSet::new(fqdn.clone(), RecordType::A, 0);
    rrset.insert(
        Record::from_rdata(fqdn.clone(), RECORD_TTL, RData::A(A::from(ip))),
        0,
    );
    let add = append(rrset, zone, false, false);
    for msg in [delete, add] {
        if let Err(e) = send(server, ip, &msg).await {
            tracing::debug!("RFC 2136 update of {fqdn} on {server} failed: {e}");
            return;
        }
    }
    tracing::debug!("published {fqdn} -> {ip} via RFC 2136 on {server}");
}

async fn withdraw(fqdn: &Name, server: Ipv4Addr, ip: Ipv4Addr) {
    let msg = delete_rrset(
        Record::update0(fqdn.clone(), 0, RecordType::A),
        zone_of(fqdn),
        false,
    );
    if let Err(e) = send(server, ip, &msg).await {
        tracing::debug!("RFC 2136 delete of {fqdn} on {server} failed: {e}");
    }
}

async fn send(server: Ipv4Addr, local_ip: Ipv4Addr, message: &Message) -> Result<(), Error> {
    let bytes = message
        .to_vec()
        .map_err(|e| Error::new(eyre!("encode DNS UPDATE: {e}"), ErrorKind::Network))?;
    // Bind to our address on the gateway so the server authorizes us by source IP.
    let socket = UdpSocket::bind(SocketAddr::new(IpAddr::V4(local_ip), 0))
        .await
        .with_kind(ErrorKind::Network)?;
    socket
        .connect(SocketAddr::new(IpAddr::V4(server), DNS_PORT))
        .await
        .with_kind(ErrorKind::Network)?;
    socket.send(&bytes).await.with_kind(ErrorKind::Network)?;
    let mut buf = [0u8; 1232];
    let n = timeout(QUERY_TIMEOUT, socket.recv(&mut buf))
        .await
        .map_err(|_| Error::new(eyre!("timed out"), ErrorKind::Network))?
        .with_kind(ErrorKind::Network)?;
    let resp = Message::from_vec(&buf[..n])
        .map_err(|e| Error::new(eyre!("decode DNS response: {e}"), ErrorKind::Network))?;
    match resp.metadata.response_code {
        // NXRRSet on a delete (nothing to remove) is fine.
        ResponseCode::NoError | ResponseCode::NXRRSet => Ok(()),
        other => Err(Error::new(
            eyre!("DNS UPDATE refused: {other}"),
            ErrorKind::Network,
        )),
    }
}
