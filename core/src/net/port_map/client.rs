//! Best-effort automatic port mapping on a public address's upstream gateway.
//!
//! Tries PCP (RFC 6887), then NAT-PMP, then UPnP IGD — one code path for a home
//! router and a StartTunnel gateway (PCP over WireGuard, see
//! [`crate::tunnel::forward::pcp`]). PCP/NAT-PMP via `crab_nat`, UPnP via
//! [`crate::net::port_map::upnp`].
//!
//! All best-effort: failures are logged, never surfaced to the nftables forward
//! reconcile, so a gateway with none of these just falls back to a manual
//! forward. `ensure`/`remove` are fire-and-forget sends so a slow or absent
//! gateway never blocks the forward path.

use std::collections::BTreeMap;
use std::net::{IpAddr, Ipv4Addr};
use std::num::NonZeroU16;
use std::time::Duration;

use crab_nat::{InternetProtocol, PortMapping, PortMappingOptions, TimeoutConfig, pcp};
use igd_next::aio::Gateway;
use igd_next::aio::tokio::Tokio;
use tokio::sync::{mpsc, oneshot};
use tokio::time::{Instant, interval};

use crate::db::model::public::NetworkInterfaceInfo;
use crate::net::port_map::pcp::hostname::OPTION_HOSTNAME;
use crate::net::port_map::pcp::portset::{OPTION_PORT_SET, PortSet};
use crate::net::port_map::upnp;
use crate::prelude::*;

/// Re-assert/renew every desired mapping on this cadence (well under the PCP
/// lease, and enough to recover a UPnP mapping lost to a gateway reboot).
const REFRESH_INTERVAL: Duration = Duration::from_secs(180);
/// Retry floor for a desired-but-not-active mapping, so a reconcile burst can't
/// busy-loop a failing apply yet boot/tunnel-restart races still recover in
/// seconds rather than waiting for the 180s refresh.
const RETRY_INTERVAL: Duration = Duration::from_secs(15);
const GATEWAY_CACHE_TTL: Duration = Duration::from_secs(600);
const PCP_LIFETIME_SECONDS: u32 = 3600;
/// Fail fast onto UPnP instead of the crate's multi-minute RFC backoff when a
/// gateway doesn't speak PCP/NAT-PMP.
const PCP_TIMEOUTS: TimeoutConfig = TimeoutConfig {
    initial_timeout: Duration::from_millis(250),
    max_retries: 1,
    max_retry_timeout: Some(Duration::from_secs(1)),
};

/// (local IP, external port, optional SNI hostname). Hostname is part of the
/// identity: many hostnames share one external port via gateway SNI demux, each
/// an independent mapping, so adding/removing one never tears down the others.
type MappingKey = (Ipv4Addr, u16, Option<String>);

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
            // Only a gateway on one of this interface's own subnets can forward
            // for it; NM may report a default-route gateway belonging to another
            // interface (e.g. the LAN router inherited by a WireGuard link).
            if let IpAddr::V4(v4) = ip {
                if ip_info.subnets.iter().any(|s| s.contains(ip)) {
                    push(*v4);
                }
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
    /// Contiguous ports to map via PCP PORT_SET (RFC 7753); `1` is single-port.
    /// `> 1` is PCP-only and skipped where the gateway won't grant the full
    /// range (UPnP/NAT-PMP can't map ranges). Always `1` for HOSTNAME mappings.
    count: u16,
}

enum Active {
    Pcp(PortMapping),
    Upnp { external_ip: Option<Ipv4Addr> },
}

enum Command {
    Ensure {
        key: MappingKey,
        spec: Spec,
    },
    Remove {
        key: MappingKey,
    },
    /// Gateway-assigned external IP for an active mapping on
    /// `(local_ip, external_port)`, to confirm reachability without a remote
    /// echo. `None` if not mapped or the external IP is unknown.
    ExternalIp {
        local_ip: Ipv4Addr,
        external_port: u16,
        resp: oneshot::Sender<Option<IpAddr>>,
    },
}

#[derive(Clone)]
pub struct PortMapController {
    req: mpsc::UnboundedSender<Command>,
}

impl PortMapController {
    pub fn new() -> Self {
        let (req, mut recv) = mpsc::unbounded_channel::<Command>();
        // Detached: `tokio::spawn` won't abort on drop; loop exits when all
        // senders are gone.
        tokio::spawn(async move {
            let mut state = State::default();
            let mut refresh = interval(REFRESH_INTERVAL);
            refresh.set_missed_tick_behavior(tokio::time::MissedTickBehavior::Skip);
            loop {
                tokio::select! {
                    cmd = recv.recv() => match cmd {
                        Some(Command::Ensure { key, spec }) => state.ensure(key, spec).await,
                        Some(Command::Remove { key }) => state.remove(key).await,
                        Some(Command::ExternalIp { local_ip, external_port, resp }) => {
                            let ip = state
                                .active
                                .iter()
                                .find(|(k, _)| k.0 == local_ip && k.1 == external_port)
                                .and_then(|(_, a)| match a {
                                    Active::Pcp(m) => m.external_ip(),
                                    Active::Upnp { external_ip } => external_ip.map(IpAddr::V4),
                                });
                            let _ = resp.send(ip);
                        }
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
        self.send_ensure(local_ip, external_port, internal_port, gateways, None, 1);
    }

    /// Like [`ensure`](Self::ensure) but binds one FQDN via PCP HOSTNAME so the
    /// gateway SNI-demuxes this external port. PCP-only; each hostname is an
    /// independent mapping sharing the port.
    pub fn ensure_hostname(
        &self,
        local_ip: Ipv4Addr,
        external_port: u16,
        internal_port: u16,
        gateways: Vec<Ipv4Addr>,
        hostname: String,
    ) {
        self.send_ensure(local_ip, external_port, internal_port, gateways, Some(hostname), 1);
    }

    /// Map `count` contiguous ports starting at `external_port` via the PCP
    /// PORT_SET option (RFC 7753). PCP-only; skipped on gateways that don't
    /// grant the full range.
    pub fn ensure_range(
        &self,
        local_ip: Ipv4Addr,
        external_port: u16,
        internal_port: u16,
        count: u16,
        gateways: Vec<Ipv4Addr>,
    ) {
        self.send_ensure(local_ip, external_port, internal_port, gateways, None, count);
    }

    fn send_ensure(
        &self,
        local_ip: Ipv4Addr,
        external_port: u16,
        internal_port: u16,
        gateways: Vec<Ipv4Addr>,
        hostname: Option<String>,
        count: u16,
    ) {
        self.req
            .send(Command::Ensure {
                key: (local_ip, external_port, hostname),
                spec: Spec {
                    internal_port,
                    gateways,
                    count,
                },
            })
            .ok();
    }

    pub fn remove(&self, local_ip: Ipv4Addr, external_port: u16) {
        self.req
            .send(Command::Remove {
                key: (local_ip, external_port, None),
            })
            .ok();
    }

    /// Remove the SNI HOSTNAME mapping for `hostname` on
    /// `(local_ip, external_port)`, leaving any other hostnames on that port.
    pub fn remove_hostname(&self, local_ip: Ipv4Addr, external_port: u16, hostname: String) {
        self.req
            .send(Command::Remove {
                key: (local_ip, external_port, Some(hostname)),
            })
            .ok();
    }

    /// Gateway-assigned external IP if a mapping is active for
    /// `(local_ip, external_port)`, else `None`. `Some` means the port was
    /// forwarded automatically, so a remote reachability check can be skipped.
    pub async fn mapped_external_ip(
        &self,
        local_ip: Ipv4Addr,
        external_port: u16,
    ) -> Option<IpAddr> {
        let (resp, rx) = oneshot::channel();
        self.req
            .send(Command::ExternalIp {
                local_ip,
                external_port,
                resp,
            })
            .ok()?;
        rx.await.ok().flatten()
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
    /// Last apply() attempt per key, to rate-limit on-demand retries of
    /// not-yet-active mappings (see RETRY_INTERVAL).
    last_attempt: BTreeMap<MappingKey, Instant>,
}

impl State {
    async fn ensure(&mut self, key: MappingKey, spec: Spec) {
        let changed = self.desired.get(&key).map_or(true, |s| {
            s.internal_port != spec.internal_port
                || s.gateways != spec.gateways
                || s.count != spec.count
        });
        self.desired.insert(key.clone(), spec);
        // Reapply on a spec change, or to retry a not-yet-active mapping — but
        // rate-limited to RETRY_INTERVAL per key so a reconcile burst can't
        // busy-loop one the gateway can't satisfy.
        let stale = !self.active.contains_key(&key)
            && self
                .last_attempt
                .get(&key)
                .map_or(true, |t| t.elapsed() >= RETRY_INTERVAL);
        if changed || stale {
            self.teardown(key.clone()).await;
            self.apply(key).await;
        }
    }

    async fn remove(&mut self, key: MappingKey) {
        self.desired.remove(&key);
        self.last_attempt.remove(&key);
        self.teardown(key).await;
    }

    async fn refresh(&mut self) {
        for key in self.desired.keys().cloned().collect::<Vec<_>>() {
            match self.active.get_mut(&key) {
                Some(Active::Pcp(m)) => {
                    if let Err(e) = m.renew().await {
                        tracing::debug!("PCP/NAT-PMP renew for {key:?} failed, re-mapping: {e}");
                        self.teardown(key.clone()).await;
                        self.apply(key).await;
                    }
                }
                // UPnP has no lease; re-assert in case a gateway reboot dropped
                // it. `None` retries a prior failure.
                Some(Active::Upnp { .. }) | None => {
                    self.teardown(key.clone()).await;
                    self.apply(key).await;
                }
            }
        }
        self.upnp_cache
            .retain(|_, (_, at)| at.elapsed() < GATEWAY_CACHE_TTL);
        self.last_attempt.retain(|k, _| self.desired.contains_key(k));
    }

    async fn teardown(&mut self, key: MappingKey) {
        match self.active.remove(&key) {
            Some(Active::Pcp(m)) => {
                if let Err((e, _)) = m.try_drop().await {
                    tracing::debug!("PCP/NAT-PMP unmap for {key:?} failed: {e}");
                }
            }
            Some(Active::Upnp { .. }) => {
                let (local_ip, external_port, _) = key;
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
        // Stamp before attempting so a permanently-failing mapping is also
        // rate-limited, not just successful ones.
        self.last_attempt.insert(key.clone(), Instant::now());
        let (local_ip, external_port, hostname) = (key.0, key.1, key.2.clone());
        let (Some(ext), Some(intl)) = (
            NonZeroU16::new(external_port),
            NonZeroU16::new(spec.internal_port),
        ) else {
            return;
        };

        // HOSTNAME (SNI-demux) mapping: PCP-only, since NAT-PMP/UPnP can't demux
        // by SNI. Other hostnames on the same port are separate mappings.
        if let Some(hostname) = &hostname {
            let options = [pcp::PcpOption {
                code: OPTION_HOSTNAME,
                data: hostname.as_bytes().to_vec(),
            }];
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
                            "PCP HOSTNAME mapped {external_port}->{local_ip}:{} {hostname} via {gw}",
                            spec.internal_port,
                        );
                        self.active.insert(key, Active::Pcp(m));
                        return;
                    }
                    Ok(m) => {
                        let _ = m.try_drop().await;
                    }
                    Err(e) => tracing::debug!(
                        "PCP HOSTNAME map {local_ip}:{external_port} {hostname} via {gw} failed: {e}"
                    ),
                }
            }
            return;
        }

        // Range mapping via PCP PORT_SET (RFC 7753), PCP-only. A gateway lacking
        // PORT_SET silently maps a single port; detect the missing/short grant
        // and skip rather than forward a partial range.
        if spec.count > 1 {
            let range_size = spec.count;
            let option = pcp::PcpOption {
                code: OPTION_PORT_SET,
                data: PortSet {
                    size: range_size,
                    first_internal_port: spec.internal_port,
                    parity: false,
                }
                .to_payload(),
            };
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
                    std::slice::from_ref(&option),
                )
                .await
                {
                    Ok(m) if m.external_port() == ext => {
                        let granted = m
                            .response_options()
                            .iter()
                            .find(|o| o.code == OPTION_PORT_SET)
                            .and_then(|o| PortSet::from_payload(&o.data))
                            .map_or(1, |ps| ps.size);
                        if granted >= range_size {
                            tracing::debug!(
                                "PCP PORT_SET mapped {external_port}+{range_size}->{local_ip}:{} via {gw}",
                                spec.internal_port
                            );
                            self.active.insert(key, Active::Pcp(m));
                            return;
                        }
                        tracing::debug!(
                            "gateway {gw} granted {granted}/{range_size} PORT_SET ports for {local_ip}:{external_port}; skipping range"
                        );
                        let _ = m.try_drop().await;
                    }
                    Ok(m) => {
                        let _ = m.try_drop().await;
                    }
                    Err(e) => tracing::debug!(
                        "PCP PORT_SET map {local_ip}:{external_port} via {gw} failed: {e}"
                    ),
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
                Err(e) => {
                    tracing::debug!("PCP/NAT-PMP map {local_ip}:{external_port} via {gw} failed: {e}")
                }
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
                    tracing::debug!("UPnP map {local_ip}:{external_port} failed: {e}");
                    false
                }
            },
            None => false,
        };
        if added {
            // Best-effort external IP (local IGD query) so a reachability check
            // can short-circuit; `get_external_ipv4` discards private/CGNAT.
            let external_ip = upnp::get_external_ipv4(local_ip).await.ok().flatten();
            self.active.insert(key, Active::Upnp { external_ip });
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

#[cfg(test)]
mod tests {
    use super::*;

    fn spec() -> Spec {
        // No gateways: apply() does no network I/O, so these tests exercise the
        // keying/identity logic only.
        Spec {
            internal_port: 443,
            gateways: Vec::new(),
            count: 1,
        }
    }

    // Distinct hostnames on the same external port are independent mappings;
    // removing one (or adding a plain mapping) never clobbers the others.
    #[tokio::test]
    async fn distinct_hostnames_share_a_port_without_clobbering() {
        let ip = Ipv4Addr::new(10, 59, 0, 2);
        let a: MappingKey = (ip, 443, Some("a.example.com".into()));
        let b: MappingKey = (ip, 443, Some("b.example.com".into()));
        let plain: MappingKey = (ip, 443, None);

        let mut state = State::default();
        state.ensure(a.clone(), spec()).await;
        state.ensure(b.clone(), spec()).await;
        assert!(state.desired.contains_key(&a));
        assert!(state.desired.contains_key(&b), "adding b clobbered a's siblings");

        state.ensure(plain.clone(), spec()).await;
        assert_eq!(state.desired.len(), 3, "plain mapping is a distinct identity");

        state.remove(a.clone()).await;
        assert!(!state.desired.contains_key(&a));
        assert!(state.desired.contains_key(&b), "removing a dropped b");
        assert!(state.desired.contains_key(&plain));
    }
}
