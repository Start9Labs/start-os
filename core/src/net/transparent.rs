//! Source-preserving transparent egress for the SNI demux (RFC §4.6).
//!
//! A demux proxy reads the TLS ClientHello on a normal listener (the host is the
//! legitimate destination of the inbound flow), then originates the internal leg
//! with the *client's* source address via `IP_TRANSPARENT`, so the backend sees
//! the real peer rather than this host. Backend→client replies (addressed to the
//! client, transiting this host as the backend's gateway) are diverted back into
//! the proxy socket by policy routing keyed on a conntrack mark.
//!
//! Datapath (all in `table ip startos` / iproute2):
//! - egress socket: `IP_TRANSPARENT`, bound to the client's `(ip, port)`.
//! - `mangle_output` `divert-save`: tag the locally-originated egress flow (its
//!   source is non-local — the spoofed client) with [`DIVERT_MARK`] on the
//!   *conntrack* entry only. Meta mark stays 0, so the outbound leg still routes
//!   to the backend via the main table.
//! - `mangle_prerouting` `restore-mark` (already installed by the gateway
//!   reconcile) copies the ct mark onto the reply packets' meta mark.
//! - `ip rule fwmark DIVERT_MARK lookup DIVERT_TABLE priority 49` + `ip route add
//!   local 0.0.0.0/0 dev lo table DIVERT_TABLE`: deliver the marked replies
//!   locally, into the transparent socket.

use std::net::{SocketAddr, SocketAddrV4};

use tokio::net::{TcpSocket, TcpStream};
use tokio::process::Command;
use tokio::sync::OnceCell;

use crate::net::utils::default_keepalive;
use crate::prelude::*;
use crate::util::Invoke;

/// Conntrack/firewall mark for transparent-egress reply diversion. Outside the
/// gateway's per-interface `1000 + ifindex` mark space and its priority-50 rule.
pub const DIVERT_MARK: u32 = 0x0054_0001;
/// Dedicated routing table holding the local-delivery default for diverted
/// replies. Outside the gateway's `1000 + ifindex` table space.
pub const DIVERT_TABLE: u32 = 1344;

/// `mangle_output` rule that tags the transparent-egress flow on its conntrack
/// entry (matched by its non-local source address — the spoofed client). Spliced
/// into the gateway's `reconcile_mangle_rules` so it survives that chain's flush.
/// Sets only the ct mark; the outbound leg keeps meta mark 0 and routes normally.
pub fn divert_save_rule() -> String {
    format!(
        "add rule ip startos mangle_output meta l4proto tcp fib saddr type != local ct state new ct mark set {DIVERT_MARK:#010x} comment \"divert-save\"\n"
    )
}

/// Open the internal leg of a demuxed connection from the client's own source
/// address, so the backend sees the real peer. Requires `CAP_NET_ADMIN` (startd
/// runs as root). The reply path is set up by [`ensure_divert_infra`].
pub async fn transparent_connect(
    client: SocketAddrV4,
    target: SocketAddrV4,
) -> std::io::Result<TcpStream> {
    let sock = TcpSocket::new_v4()?;
    {
        let sref = socket2::SockRef::from(&sock);
        // Must precede bind: permits binding a non-local (client) address.
        sref.set_ip_transparent_v4(true)?;
        sref.set_reuse_address(true)?;
        if let Err(e) = sref.set_tcp_keepalive(&default_keepalive()) {
            tracing::debug!("transparent egress keepalive: {e}");
        }
    }
    sock.bind(SocketAddr::V4(client))?;
    sock.connect(SocketAddr::V4(target)).await
}

static DIVERT_INFRA: OnceCell<()> = OnceCell::const_new();

/// [`ensure_divert_infra`] run at most once per process (cached only on success,
/// so a transient failure is retried on the next call). Cheap to call per
/// connection from the local passthrough path.
pub async fn ensure_divert_infra_once() {
    let _ = DIVERT_INFRA
        .get_or_try_init(|| async { ensure_divert_infra().await })
        .await;
}

/// Install the iproute2 half of the reply-path divert (idempotent). The nft half
/// (`divert-save`, plus the existing `restore-mark`) lives in the gateway mangle
/// reconcile. Safe to call repeatedly.
pub async fn ensure_divert_infra() -> Result<(), Error> {
    let table = DIVERT_TABLE.to_string();

    // Local-delivery default in the divert table: marked replies are delivered
    // to the local transparent socket instead of being forwarded back out.
    Command::new("ip")
        .args(["route", "replace", "local", "0.0.0.0/0", "dev", "lo", "table", &table])
        .invoke(ErrorKind::Network)
        .await?;

    // Policy rule at priority 49 — above the gateway's per-interface symmetric
    // -return rules at 50 — so diverted replies win.
    let rules = Command::new("ip")
        .args(["rule", "list"])
        .invoke(ErrorKind::Network)
        .await
        .unwrap_or_default();
    if !String::from_utf8_lossy(&rules).contains(&format!("lookup {table}")) {
        Command::new("ip")
            .args([
                "rule", "add", "fwmark", &format!("{DIVERT_MARK:#x}"), "lookup", &table,
                "priority", "49",
            ])
            .invoke(ErrorKind::Network)
            .await?;
    }

    // NOTE: strict reverse-path filtering (rp_filter=1) on the egress interface
    // may drop the asymmetric diverted replies. If VM testing shows drops, loosen
    // that one interface to 2 (never `all`, never 0) — not done preemptively.
    Ok(())
}
