//! Best-effort automatic port mapping on a public address's upstream gateway.
//!
//! Prioritizes PCP (RFC 6887), falling back to NAT-PMP, then UPnP IGD — the
//! same code path covers a home router and a StartTunnel gateway (which speaks
//! PCP over WireGuard, see [`crate::tunnel::pcp`]). PCP/NAT-PMP are handled by
//! the `crab_nat` crate; UPnP by [`crate::net::upnp`].
//!
//! Everything here is best-effort: a gateway that supports none of these (or
//! has them disabled) just means the user falls back to a manual port forward,
//! so failures are logged, never surfaced to the (nftables) forward reconcile.
//! `ensure`/`remove` are fire-and-forget sends so a slow or absent gateway
//! never blocks the forward path.

use std::collections::BTreeMap;
use std::net::{IpAddr, Ipv4Addr};
use std::num::NonZeroU16;
use std::time::Duration;

use crab_nat::{InternetProtocol, PortMapping, PortMappingOptions, TimeoutConfig, pcp};
use igd_next::aio::Gateway;
use igd_next::aio::tokio::Tokio;
use tokio::sync::mpsc;
use tokio::time::{Instant, interval};

use crate::db::model::public::NetworkInterfaceInfo;
use crate::net::pcp_hostname::OPTION_HOSTNAME;
use crate::net::upnp;
use crate::prelude::*;

/// Re-assert/renew every desired mapping on this cadence (well under the PCP
/// lease, and enough to recover a UPnP mapping lost to a gateway reboot).
const REFRESH_INTERVAL: Duration = Duration::from_secs(180);
const GATEWAY_CACHE_TTL: Duration = Duration::from_secs(600);
const PCP_LIFETIME_SECONDS: u32 = 3600;
/// Keep PCP/NAT-PMP attempts snappy: a gateway that doesn't speak them should
/// fail fast so we move on to UPnP, rather than the crate's multi-minute RFC
/// backoff.
const PCP_TIMEOUTS: TimeoutConfig = TimeoutConfig {
    initial_timeout: Duration::from_millis(250),
    max_retries: 1,
    max_retry_timeout: Some(Duration::from_secs(1)),
};

/// (local interface IP we map from, external port).
type MappingKey = (Ipv4Addr, u16);

/// Candidate PCP/NAT-PMP servers for a gateway interface: the NM default
/// gateway (router) plus each subnet's `.1` (covers a StartTunnel, whose server
/// is the subnet's `.1`, and is the usual router address otherwise).
pub fn candidate_gateways(info: &NetworkInterfaceInfo) -> Vec<Ipv4Addr> {
    let mut out = Vec::new();
    let mut push = |ip: Ipv4Addr| {
        if !ip.is_unspecified() && !ip.is_loopback() && !ip.is_broadcast() && !out.contains(&ip) {
            out.push(ip);
        }
    };
    if let Some(ip_info) = &info.ip_info {
        for ip in &ip_info.lan_ip {
            if let IpAddr::V4(v4) = ip {
                push(*v4);
            }
        }
        for subnet in &ip_info.subnets {
            if let Some(IpAddr::V4(v4)) = subnet.hosts().next() {
                push(v4);
            }
        }
    }
    out
}

#[derive(Clone)]
struct Spec {
    internal_port: u16,
    gateways: Vec<Ipv4Addr>,
    /// FQDNs to bind via the PCP HOSTNAME option (SNI-demultiplexed mappings).
    /// When non-empty the mapping is PCP-only — no NAT-PMP/UPnP fallback, since
    /// the gateway demuxes inbound TLS by SNI to share one external port.
    hostnames: Vec<String>,
}

enum Active {
    Pcp(PortMapping),
    Upnp,
}

enum Command {
    Ensure { key: MappingKey, spec: Spec },
    Remove { key: MappingKey },
}

#[derive(Clone)]
pub struct PortMapController {
    req: mpsc::UnboundedSender<Command>,
}

impl PortMapController {
    pub fn new() -> Self {
        let (req, mut recv) = mpsc::unbounded_channel::<Command>();
        // Detached daemon: plain `tokio::spawn` does not abort on handle drop;
        // the loop exits once every clone is dropped (all senders gone).
        tokio::spawn(async move {
            let mut state = State::default();
            let mut refresh = interval(REFRESH_INTERVAL);
            refresh.set_missed_tick_behavior(tokio::time::MissedTickBehavior::Skip);
            loop {
                tokio::select! {
                    cmd = recv.recv() => match cmd {
                        Some(Command::Ensure { key, spec }) => state.ensure(key, spec).await,
                        Some(Command::Remove { key }) => state.remove(key).await,
                        None => break,
                    },
                    _ = refresh.tick() => state.refresh().await,
                }
            }
        });
        Self { req }
    }

    pub fn ensure(
        &self,
        local_ip: Ipv4Addr,
        external_port: u16,
        internal_port: u16,
        gateways: Vec<Ipv4Addr>,
    ) {
        self.ensure_hostnames(local_ip, external_port, internal_port, gateways, Vec::new());
    }

    /// Like [`ensure`](Self::ensure) but binds the given FQDNs via the PCP
    /// HOSTNAME option so the gateway SNI-demultiplexes this external port.
    /// PCP-only (no NAT-PMP/UPnP fallback).
    pub fn ensure_hostnames(
        &self,
        local_ip: Ipv4Addr,
        external_port: u16,
        internal_port: u16,
        gateways: Vec<Ipv4Addr>,
        hostnames: Vec<String>,
    ) {
        self.req
            .send(Command::Ensure {
                key: (local_ip, external_port),
                spec: Spec {
                    internal_port,
                    gateways,
                    hostnames,
                },
            })
            .ok();
    }

    pub fn remove(&self, local_ip: Ipv4Addr, external_port: u16) {
        self.req
            .send(Command::Remove {
                key: (local_ip, external_port),
            })
            .ok();
    }
}

impl Default for PortMapController {
    fn default() -> Self {
        Self::new()
    }
}

#[derive(Default)]
struct State {
    desired: BTreeMap<MappingKey, Spec>,
    active: BTreeMap<MappingKey, Active>,
    upnp_cache: BTreeMap<Ipv4Addr, (Gateway<Tokio>, Instant)>,
}

impl State {
    async fn ensure(&mut self, key: MappingKey, spec: Spec) {
        let changed = self.desired.get(&key).map_or(true, |s| {
            s.internal_port != spec.internal_port
                || s.gateways != spec.gateways
                || s.hostnames != spec.hostnames
        });
        self.desired.insert(key, spec);
        if changed || !self.active.contains_key(&key) {
            self.teardown(key).await;
            self.apply(key).await;
        }
    }

    async fn remove(&mut self, key: MappingKey) {
        self.desired.remove(&key);
        self.teardown(key).await;
    }

    async fn refresh(&mut self) {
        for key in self.desired.keys().copied().collect::<Vec<_>>() {
            match self.active.get_mut(&key) {
                Some(Active::Pcp(m)) => {
                    if let Err(e) = m.renew().await {
                        tracing::debug!("PCP/NAT-PMP renew for {key:?} failed, re-mapping: {e}");
                        self.teardown(key).await;
                        self.apply(key).await;
                    }
                }
                // UPnP has no lease to renew, but a gateway reboot may have
                // dropped it; re-assert. `None` retries a prior failure.
                Some(Active::Upnp) | None => {
                    self.teardown(key).await;
                    self.apply(key).await;
                }
            }
        }
        self.upnp_cache
            .retain(|_, (_, at)| at.elapsed() < GATEWAY_CACHE_TTL);
    }

    async fn teardown(&mut self, key: MappingKey) {
        match self.active.remove(&key) {
            Some(Active::Pcp(m)) => {
                if let Err((e, _)) = m.try_drop().await {
                    tracing::debug!("PCP/NAT-PMP unmap for {key:?} failed: {e}");
                }
            }
            Some(Active::Upnp) => {
                let (local_ip, external_port) = key;
                if let Some(gw) = self.gateway_for(local_ip).await {
                    upnp::remove_port(gw, external_port).await.log_err();
                }
            }
            None => {}
        }
    }

    async fn apply(&mut self, key: MappingKey) {
        let Some(spec) = self.desired.get(&key).cloned() else {
            return;
        };
        let (local_ip, external_port) = key;
        let (Some(ext), Some(intl)) = (
            NonZeroU16::new(external_port),
            NonZeroU16::new(spec.internal_port),
        ) else {
            return;
        };

        // Hostname (SNI-demux) mappings carry HOSTNAME options and are PCP-only:
        // NAT-PMP/UPnP can't demultiplex by SNI, so there is no fallback.
        if !spec.hostnames.is_empty() {
            let options: Vec<pcp::PcpOption> = spec
                .hostnames
                .iter()
                .map(|h| pcp::PcpOption {
                    code: OPTION_HOSTNAME,
                    data: h.as_bytes().to_vec(),
                })
                .collect();
            for gw in &spec.gateways {
                match pcp::port_mapping(
                    pcp::BaseMapRequest::new(IpAddr::V4(*gw), IpAddr::V4(local_ip), InternetProtocol::Tcp, intl),
                    None,
                    None,
                    PortMappingOptions {
                        external_port: Some(ext),
                        lifetime_seconds: Some(PCP_LIFETIME_SECONDS),
                        timeout_config: Some(PCP_TIMEOUTS),
                    },
                    &options,
                )
                .await
                {
                    Ok(m) if m.external_port() == ext => {
                        tracing::debug!(
                            "PCP HOSTNAME mapped {external_port}->{local_ip}:{} {:?} via {gw}",
                            spec.internal_port,
                            spec.hostnames
                        );
                        self.active.insert(key, Active::Pcp(m));
                        return;
                    }
                    Ok(m) => {
                        let _ = m.try_drop().await;
                    }
                    Err(e) => tracing::debug!("PCP HOSTNAME map via {gw} failed: {e}"),
                }
            }
            return;
        }

        // PCP first, NAT-PMP fallback (crab_nat), against each candidate gateway.
        for gw in &spec.gateways {
            match PortMapping::new(
                IpAddr::V4(*gw),
                IpAddr::V4(local_ip),
                InternetProtocol::Tcp,
                intl,
                PortMappingOptions {
                    external_port: Some(ext),
                    lifetime_seconds: Some(PCP_LIFETIME_SECONDS),
                    timeout_config: Some(PCP_TIMEOUTS),
                },
            )
            .await
            {
                Ok(m) if m.external_port() == ext => {
                    tracing::debug!(
                        "{} mapped {external_port}->{local_ip}:{} via {gw}",
                        m.mapping_type(),
                        spec.internal_port
                    );
                    self.active.insert(key, Active::Pcp(m));
                    return;
                }
                // A different external port is useless for a fixed public port.
                Ok(m) => {
                    let _ = m.try_drop().await;
                }
                Err(e) => tracing::debug!("PCP/NAT-PMP map via {gw} failed: {e}"),
            }
        }

        // Fall back to UPnP.
        let added = match self.gateway_for(local_ip).await {
            Some(gw) => match upnp::add_port(gw, external_port, local_ip, spec.internal_port).await {
                Ok(()) => {
                    tracing::debug!("UPnP mapped {external_port}->{local_ip}:{}", spec.internal_port);
                    true
                }
                Err(e) => {
                    tracing::debug!("UPnP map failed: {e}");
                    false
                }
            },
            None => false,
        };
        if added {
            self.active.insert(key, Active::Upnp);
        } else {
            // Re-discover next time in case the gateway went away.
            self.upnp_cache.remove(&local_ip);
        }
    }

    async fn gateway_for(&mut self, local_ip: Ipv4Addr) -> Option<&Gateway<Tokio>> {
        let fresh = self
            .upnp_cache
            .get(&local_ip)
            .map_or(false, |(_, at)| at.elapsed() < GATEWAY_CACHE_TTL);
        if !fresh {
            match upnp::discover(local_ip).await {
                Ok(g) => {
                    self.upnp_cache.insert(local_ip, (g, Instant::now()));
                }
                Err(e) => {
                    tracing::debug!("no UPnP gateway on {local_ip}: {e}");
                    self.upnp_cache.remove(&local_ip);
                    return None;
                }
            }
        }
        self.upnp_cache.get(&local_ip).map(|(g, _)| g)
    }
}
