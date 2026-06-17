//! Server-side UPnP IGD for StartTunnel, offered to connected WireGuard peers.
//!
//! A StartOS client behind a StartTunnel gateway opens its public ports the
//! same way it would behind a home router: UPnP IGD (`GetExternalIPAddress` /
//! `AddPortMapping` / `DeletePortMapping`). This module makes the tunnel answer
//! those requests over the WireGuard interface, so the client's UPnP code path
//! (see [`crate::net::upnp`]) is identical for a router and a tunnel.
//!
//! Secure mode: the IGD is reachable **only** over the WireGuard interface
//! (sockets are `SO_BINDTODEVICE`-bound to it) and only honors requests from a
//! configured peer. Crucially, a peer can only forward to **itself** — the
//! `NewInternalClient` in the SOAP body is ignored and the mapping target is
//! forced to the requesting peer's own tunnel IP. So, unlike classic UPnP, a
//! peer cannot open a port to any other host.

use std::net::{IpAddr, Ipv4Addr, SocketAddr, SocketAddrV4};
#[cfg(target_os = "linux")]
use std::os::unix::io::AsRawFd;
use std::sync::Arc;
use std::time::Duration;

use axum::Router;
use axum::extract::{ConnectInfo, State};
use axum::http::{HeaderMap, StatusCode};
use axum::response::{IntoResponse, Response};
use axum::routing::{get, post};
use nix::net::if_::if_nametoindex;
use socket2::{Domain, InterfaceIndexOrAddress, Protocol, SockAddr, Socket, Type};
use tokio::net::{TcpListener, UdpSocket};

use crate::db::model::public::NetworkInterfaceType;
use crate::net::igd_server::{
    CONTROL_PATH, IGD_HTTP_PORT, ROOT_DESC_PATH, SCPD, SCPD_PATH, SSDP_MULTICAST, SSDP_PORT,
    format_uuid, handle_control, header_value, render_root_desc, serve_static, ssdp_response,
    st_matches,
};
use crate::prelude::*;
use crate::tunnel::context::TunnelContext;
use crate::tunnel::db::PortForward;
use crate::tunnel::wg::WIREGUARD_INTERFACE_NAME;

/// Run the IGD server (SSDP responder + HTTP control server) for the life of
/// the tunnel. Both halves self-restart on error.
pub async fn run(ctx: TunnelContext) {
    let uuid = match device_uuid(&ctx).await {
        Ok(uuid) => uuid,
        Err(e) => {
            tracing::error!("UPnP IGD: cannot derive device uuid: {e}");
            return;
        }
    };
    let root_desc: Arc<str> = Arc::from(render_root_desc(&uuid));
    tokio::join!(http_server(ctx.clone(), root_desc), ssdp_server(ctx, uuid));
}

async fn device_uuid(ctx: &TunnelContext) -> Result<String, Error> {
    let key = ctx.db.peek().await.as_wg().as_key().de()?.verifying_key();
    Ok(format_uuid(key.0.as_bytes()))
}

// ---------------------------------------------------------------------------
// SSDP discovery responder
// ---------------------------------------------------------------------------

async fn ssdp_server(ctx: TunnelContext, uuid: String) {
    loop {
        if let Err(e) = ssdp_loop(&ctx, &uuid).await {
            tracing::warn!("UPnP IGD SSDP responder failed, retrying: {e}");
            tokio::time::sleep(Duration::from_secs(5)).await;
        }
    }
}

fn ssdp_socket() -> Result<UdpSocket, Error> {
    let socket = Socket::new(Domain::IPV4, Type::DGRAM, Some(Protocol::UDP))
        .with_kind(ErrorKind::Network)?;
    socket.set_reuse_address(true).with_kind(ErrorKind::Network)?;
    bind_to_wireguard(&socket)?;
    socket
        .bind(&SockAddr::from(SocketAddrV4::new(
            Ipv4Addr::UNSPECIFIED,
            SSDP_PORT,
        )))
        .with_kind(ErrorKind::Network)?;
    let ifindex = if_nametoindex(WIREGUARD_INTERFACE_NAME).with_kind(ErrorKind::Network)?;
    socket
        .join_multicast_v4_n(&SSDP_MULTICAST, &InterfaceIndexOrAddress::Index(ifindex))
        .with_kind(ErrorKind::Network)?;
    socket.set_multicast_loop_v4(false).with_kind(ErrorKind::Network)?;
    socket.set_nonblocking(true).with_kind(ErrorKind::Network)?;
    UdpSocket::from_std(socket.into()).with_kind(ErrorKind::Network)
}

async fn ssdp_loop(ctx: &TunnelContext, uuid: &str) -> Result<(), Error> {
    let socket = ssdp_socket()?;
    tracing::info!("UPnP IGD SSDP responder listening on {WIREGUARD_INTERFACE_NAME}");
    let mut buf = [0u8; 2048];
    loop {
        let (n, from) = socket.recv_from(&mut buf).await.with_kind(ErrorKind::Network)?;
        let Ok(text) = std::str::from_utf8(&buf[..n]) else {
            continue;
        };
        if !text.starts_with("M-SEARCH") {
            continue;
        }
        let Some(st) = header_value(text, "st") else {
            continue;
        };
        if !st_matches(&st) {
            continue;
        }
        let IpAddr::V4(peer) = from.ip() else {
            continue;
        };
        // Only answer a configured peer, and advertise the `.1` of its subnet.
        let Some(server_ip) = subnet_gateway_for(ctx, peer).await else {
            continue;
        };
        let resp = ssdp_response(server_ip, uuid);
        if let Err(e) = socket.send_to(resp.as_bytes(), from).await {
            tracing::debug!("UPnP IGD: failed to answer M-SEARCH from {from}: {e}");
        }
    }
}

/// The `.1` server address of the subnet that contains `peer`, if `peer` is a
/// configured client on it. Doubles as the peer-authorization check for SSDP.
pub(super) async fn subnet_gateway_for(ctx: &TunnelContext, peer: Ipv4Addr) -> Option<Ipv4Addr> {
    let subnets = ctx.db.peek().await.as_wg().as_subnets().de().ok()?;
    subnets.0.iter().find_map(|(subnet, cfg)| {
        if cfg.clients.0.contains_key(&peer) {
            Some(subnet.addr())
        } else {
            None
        }
    })
}

// ---------------------------------------------------------------------------
// HTTP: device description, SCPD, SOAP control
// ---------------------------------------------------------------------------

async fn http_server(ctx: TunnelContext, root_desc: Arc<str>) {
    let app = Router::new()
        .route(ROOT_DESC_PATH, get(move || serve_static(root_desc.clone(), "text/xml")))
        .route(SCPD_PATH, get(|| serve_static(Arc::from(SCPD), "text/xml")))
        .route(CONTROL_PATH, post(control))
        .with_state(ctx);
    loop {
        match igd_http_listener() {
            Ok(listener) => {
                tracing::info!("UPnP IGD control server listening on {WIREGUARD_INTERFACE_NAME}:{IGD_HTTP_PORT}");
                if let Err(e) = axum::serve(
                    listener,
                    app.clone()
                        .into_make_service_with_connect_info::<SocketAddr>(),
                )
                .await
                {
                    tracing::warn!("UPnP IGD control server exited, retrying: {e}");
                }
            }
            Err(e) => tracing::warn!("UPnP IGD control server bind failed, retrying: {e}"),
        }
        tokio::time::sleep(Duration::from_secs(5)).await;
    }
}

fn igd_http_listener() -> Result<TcpListener, Error> {
    let socket = Socket::new(Domain::IPV4, Type::STREAM, Some(Protocol::TCP))
        .with_kind(ErrorKind::Network)?;
    socket.set_reuse_address(true).with_kind(ErrorKind::Network)?;
    bind_to_wireguard(&socket)?;
    socket
        .bind(&SockAddr::from(SocketAddrV4::new(
            Ipv4Addr::UNSPECIFIED,
            IGD_HTTP_PORT,
        )))
        .with_kind(ErrorKind::Network)?;
    socket.listen(128).with_kind(ErrorKind::Network)?;
    socket.set_nonblocking(true).with_kind(ErrorKind::Network)?;
    TcpListener::from_std(socket.into()).with_kind(ErrorKind::Network)
}

/// `SO_BINDTODEVICE` to the WireGuard interface so the socket only ever sees
/// tunnel traffic — never the VPS's public interface. This is what keeps the
/// IGD private to authenticated peers. The tunnel server only runs on Linux;
/// the stub below keeps the `core` lib compiling for the other CI targets
/// (apple-darwin lacks `SO_BINDTODEVICE`) and is never reached at runtime.
#[cfg(target_os = "linux")]
pub(super) fn bind_to_wireguard(socket: &Socket) -> Result<(), Error> {
    let name = WIREGUARD_INTERFACE_NAME.as_bytes();
    // SAFETY: valid fd from `socket`; `name` is a valid byte slice and `len` is
    // its length; SO_BINDTODEVICE copies the bytes.
    let ret = unsafe {
        libc::setsockopt(
            socket.as_raw_fd(),
            libc::SOL_SOCKET,
            libc::SO_BINDTODEVICE,
            name.as_ptr() as *const libc::c_void,
            name.len() as libc::socklen_t,
        )
    };
    if ret != 0 {
        return Err(Error::new(
            eyre!(
                "SO_BINDTODEVICE({WIREGUARD_INTERFACE_NAME}): {}",
                std::io::Error::last_os_error()
            ),
            ErrorKind::Network,
        ));
    }
    Ok(())
}

#[cfg(not(target_os = "linux"))]
pub(super) fn bind_to_wireguard(_socket: &Socket) -> Result<(), Error> {
    Err(Error::new(
        eyre!("SO_BINDTODEVICE is only supported on Linux"),
        ErrorKind::Network,
    ))
}

async fn control(
    State(ctx): State<TunnelContext>,
    ConnectInfo(from): ConnectInfo<SocketAddr>,
    headers: HeaderMap,
    body: String,
) -> Response {
    let IpAddr::V4(peer) = from.ip() else {
        return StatusCode::BAD_REQUEST.into_response();
    };
    handle_control(&ctx, peer, &headers, &body).await
}

pub(super) async fn apply_peer_forward(
    ctx: &TunnelContext,
    source: SocketAddrV4,
    target: SocketAddrV4,
    peer: Ipv4Addr,
) -> Result<(), u16> {
    apply_peer_forward_range(ctx, source, target, 1, peer, "UPnP").await
}

/// Like [`apply_peer_forward`] but forwards `count` contiguous ports starting at
/// `source.port()`/`target.port()` (a PCP PORT_SET range). `protocol_label` is
/// the human label prefix recorded in the DB (e.g. "UPnP" or "PCP").
pub(super) async fn apply_peer_forward_range(
    ctx: &TunnelContext,
    source: SocketAddrV4,
    target: SocketAddrV4,
    count: u16,
    peer: Ipv4Addr,
    protocol_label: &str,
) -> Result<(), u16> {
    match current_forward(ctx, source).await {
        Some(PortForward::Dnat {
            target: t, count: c, ..
        }) if t != target || c != count => {
            return Err(718); // ConflictInMappingEntry
        }
        // The external port is held by an SNI-demuxed forward.
        Some(PortForward::Sni { .. }) => {
            return Err(718); // ConflictInMappingEntry
        }
        Some(PortForward::Dnat { .. }) => {
            // Idempotent re-assert (the client refreshes periodically): make
            // sure the nft forward is actually installed, then we're done.
            let active = ctx.active_forwards.mutate(|m| m.contains_key(&source));
            if !active {
                let prefix = prefix_for(ctx, target.ip()).await;
                let rc = ctx
                    .forward
                    .add_forward_range(source, target, count, prefix, None)
                    .await
                    .map_err(|_| 501u16)?;
                ctx.active_forwards.mutate(|m| {
                    m.insert(source, rc);
                });
            }
            return Ok(());
        }
        None => {}
    }

    let prefix = prefix_for(ctx, target.ip()).await;
    let rc = ctx
        .forward
        .add_forward_range(source, target, count, prefix, None)
        .await
        .map_err(|_| 501u16)?;
    ctx.active_forwards.mutate(|m| {
        m.insert(source, rc);
    });
    let entry = PortForward::Dnat {
        target,
        label: Some(format!("{protocol_label} ({peer})")),
        enabled: true,
        count,
    };
    ctx.db
        .mutate(|db| db.as_port_forwards_mut().insert(&source, &entry).map(|_| ()))
        .await
        .result
        .map_err(|_| 501u16)?;
    Ok(())
}

pub(super) async fn current_forward(ctx: &TunnelContext, source: SocketAddrV4) -> Option<PortForward> {
    ctx.db
        .peek()
        .await
        .as_port_forwards()
        .de()
        .ok()
        .and_then(|pf| pf.0.get(&source).cloned())
}

pub(super) async fn is_known_client(ctx: &TunnelContext, peer: Ipv4Addr) -> bool {
    subnet_gateway_for(ctx, peer).await.is_some()
}

pub(super) async fn external_ipv4(ctx: &TunnelContext) -> Option<Ipv4Addr> {
    ctx.net_iface.peek(|ifaces| {
        ifaces.iter().find_map(|(id, info)| {
            if id.as_str() == WIREGUARD_INTERFACE_NAME {
                return None;
            }
            let ip_info = info.ip_info.as_ref()?;
            if ip_info.device_type == Some(NetworkInterfaceType::Loopback) {
                return None;
            }
            ip_info.wan_ip.or_else(|| {
                ip_info.subnets.iter().find_map(|s| match s.addr() {
                    IpAddr::V4(v4) if !v4.is_loopback() && !v4.is_unspecified() => Some(v4),
                    _ => None,
                })
            })
        })
    })
}

async fn prefix_for(ctx: &TunnelContext, target_ip: &Ipv4Addr) -> u8 {
    ctx.net_iface
        .peek(|ifaces| {
            ifaces.iter().find_map(|(_, info)| {
                info.ip_info.as_ref().and_then(|i| {
                    i.subnets
                        .iter()
                        .find(|s| s.contains(&IpAddr::V4(*target_ip)))
                        .map(|s| s.prefix_len())
                })
            })
        })
        .unwrap_or(32)
}
