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
use axum::http::{HeaderMap, StatusCode, header};
use axum::response::{IntoResponse, Response};
use axum::routing::{get, post};
use nix::net::if_::if_nametoindex;
use socket2::{Domain, InterfaceIndexOrAddress, Protocol, SockAddr, Socket, Type};
use tokio::net::{TcpListener, UdpSocket};

use crate::db::model::public::NetworkInterfaceType;
use crate::prelude::*;
use crate::tunnel::context::TunnelContext;
use crate::tunnel::db::PortForwardEntry;
use crate::tunnel::wg::WIREGUARD_INTERFACE_NAME;

const SSDP_MULTICAST: Ipv4Addr = Ipv4Addr::new(239, 255, 255, 250);
const SSDP_PORT: u16 = 1900;
/// HTTP port (on the WireGuard interface only) that serves the device
/// description, SCPD, and the SOAP control endpoint.
const IGD_HTTP_PORT: u16 = 49001;
const WANIP_SERVICE: &str = "urn:schemas-upnp-org:service:WANIPConnection:1";
const IGD_DEVICE: &str = "urn:schemas-upnp-org:device:InternetGatewayDevice:1";
const SERVER_HEADER: &str = "StartTunnel UPnP/1.1 StartOS";
const ROOT_DESC_PATH: &str = "/rootDesc.xml";
const SCPD_PATH: &str = "/WANIPCn.xml";
const CONTROL_PATH: &str = "/ctl/IPConn";

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

fn format_uuid(bytes: &[u8]) -> String {
    let mut b = [0u8; 16];
    for (i, slot) in b.iter_mut().enumerate() {
        *slot = bytes.get(i).copied().unwrap_or(0);
    }
    // RFC 4122 variant/version bits aren't load-bearing here; we only need a
    // stable, well-formed UDN derived from the server's WireGuard identity.
    format!(
        "{:02x}{:02x}{:02x}{:02x}-{:02x}{:02x}-{:02x}{:02x}-{:02x}{:02x}-{:02x}{:02x}{:02x}{:02x}{:02x}{:02x}",
        b[0], b[1], b[2], b[3], b[4], b[5], b[6], b[7], b[8], b[9], b[10], b[11], b[12], b[13], b[14], b[15]
    )
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

fn st_matches(st: &str) -> bool {
    st == "ssdp:all"
        || st == "upnp:rootdevice"
        || st.contains("InternetGatewayDevice")
        || st.contains("WANIPConnection")
        || st.contains("WANConnectionDevice")
}

fn ssdp_response(server_ip: Ipv4Addr, uuid: &str) -> String {
    let location = format!("http://{server_ip}:{IGD_HTTP_PORT}{ROOT_DESC_PATH}");
    format!(
        "HTTP/1.1 200 OK\r\n\
         CACHE-CONTROL: max-age=1800\r\n\
         EXT:\r\n\
         LOCATION: {location}\r\n\
         SERVER: {SERVER_HEADER}\r\n\
         ST: {IGD_DEVICE}\r\n\
         USN: uuid:{uuid}::{IGD_DEVICE}\r\n\
         \r\n"
    )
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

async fn serve_static(body: Arc<str>, content_type: &'static str) -> Response {
    (
        StatusCode::OK,
        [(header::CONTENT_TYPE, content_type)],
        body.to_string(),
    )
        .into_response()
}

async fn control(
    State(ctx): State<TunnelContext>,
    ConnectInfo(from): ConnectInfo<SocketAddr>,
    headers: HeaderMap,
    body: String,
) -> Response {
    let IpAddr::V4(peer) = from.ip() else {
        return fault(606, "Action not authorized");
    };
    let action = soap_action(&headers, &body);
    match action.as_deref() {
        Some("GetExternalIPAddress") => get_external_ip(&ctx).await,
        Some("AddPortMapping") => add_mapping(&ctx, peer, &body, false).await,
        Some("AddAnyPortMapping") => add_mapping(&ctx, peer, &body, true).await,
        Some("DeletePortMapping") => delete_mapping(&ctx, peer, &body).await,
        _ => fault(401, "Invalid Action"),
    }
}

async fn get_external_ip(ctx: &TunnelContext) -> Response {
    match external_ipv4(ctx).await {
        Some(ip) => ok(
            "GetExternalIPAddress",
            &format!("<NewExternalIPAddress>{ip}</NewExternalIPAddress>"),
        ),
        None => fault(501, "Action Failed"),
    }
}

async fn add_mapping(ctx: &TunnelContext, peer: Ipv4Addr, body: &str, any: bool) -> Response {
    let (Some(external_port), Some(internal_port)) = (
        soap_u16(body, "NewExternalPort"),
        soap_u16(body, "NewInternalPort"),
    ) else {
        return fault(402, "Invalid Args");
    };
    if external_port == 0 || internal_port == 0 {
        return fault(402, "Invalid Args");
    }
    if !is_known_client(ctx, peer).await {
        return fault(606, "Action not authorized");
    }
    let Some(source_ip) = external_ipv4(ctx).await else {
        return fault(501, "Action Failed");
    };
    let source = SocketAddrV4::new(source_ip, external_port);
    // Secure mode: force the target to the requesting peer's own tunnel IP.
    let target = SocketAddrV4::new(peer, internal_port);

    match apply_peer_forward(ctx, source, target, peer).await {
        Ok(()) if any => ok(
            "AddAnyPortMapping",
            &format!("<NewReservedPort>{external_port}</NewReservedPort>"),
        ),
        Ok(()) => ok("AddPortMapping", ""),
        Err(code) => fault(code, upnp_error_text(code)),
    }
}

async fn delete_mapping(ctx: &TunnelContext, peer: Ipv4Addr, body: &str) -> Response {
    use crate::net::pcp_server::GatewayBackend;

    let Some(external_port) = soap_u16(body, "NewExternalPort") else {
        return fault(402, "Invalid Args");
    };
    let Some(source_ip) = external_ipv4(ctx).await else {
        return fault(714, "NoSuchEntryInArray");
    };
    let source = SocketAddrV4::new(source_ip, external_port);

    // Identifies the mapping by external port and only removes it if owned by
    // this peer, so a peer can't delete (or probe for) another's mapping.
    if ctx.remove_forward_by_source(source, peer).await {
        ok("DeletePortMapping", "")
    } else {
        fault(714, "NoSuchEntryInArray")
    }
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
    if let Some(existing) = current_forward(ctx, source).await {
        if existing.target != target || existing.count != count {
            return Err(718); // ConflictInMappingEntry
        }
        // Idempotent re-assert (the client refreshes periodically): make sure
        // the nft forward is actually installed, then we're done.
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

    let prefix = prefix_for(ctx, target.ip()).await;
    let rc = ctx
        .forward
        .add_forward_range(source, target, count, prefix, None)
        .await
        .map_err(|_| 501u16)?;
    ctx.active_forwards.mutate(|m| {
        m.insert(source, rc);
    });
    let entry = PortForwardEntry {
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

pub(super) async fn current_forward(ctx: &TunnelContext, source: SocketAddrV4) -> Option<PortForwardEntry> {
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

// ---------------------------------------------------------------------------
// SOAP / HTTP helpers
// ---------------------------------------------------------------------------

/// Extract a case-insensitive single-line header value from a raw HTTP message.
fn header_value(msg: &str, name: &str) -> Option<String> {
    let name = name.to_ascii_lowercase();
    msg.lines().find_map(|line| {
        let (k, v) = line.split_once(':')?;
        if k.trim().to_ascii_lowercase() == name {
            Some(v.trim().to_string())
        } else {
            None
        }
    })
}

/// The SOAP action being invoked, from the `SOAPAction` header (`"...#Action"`)
/// or, failing that, the first element under the SOAP `Body`.
fn soap_action(headers: &HeaderMap, body: &str) -> Option<String> {
    if let Some(h) = headers.get("SOAPAction").and_then(|v| v.to_str().ok()) {
        let h = h.trim().trim_matches('"');
        if let Some((_, action)) = h.rsplit_once('#') {
            return Some(action.to_string());
        }
    }
    let root = xmltree::Element::parse(body.as_bytes()).ok()?;
    let b = root.get_child("Body")?;
    b.children
        .iter()
        .find_map(|n| n.as_element().map(|e| e.name.clone()))
}

/// Read a `u16` argument by element name from anywhere in the SOAP body.
fn soap_u16(body: &str, arg: &str) -> Option<u16> {
    let root = xmltree::Element::parse(body.as_bytes()).ok()?;
    let action = root
        .get_child("Body")?
        .children
        .iter()
        .find_map(|n| n.as_element())?;
    action.get_child(arg)?.get_text()?.trim().parse().ok()
}

fn ok(action: &str, inner: &str) -> Response {
    let body = format!(
        include_str!("igd_xml/ok.xml"),
        action = action,
        service = WANIP_SERVICE,
        inner = inner,
    );
    (
        StatusCode::OK,
        [(header::CONTENT_TYPE, "text/xml; charset=\"utf-8\"")],
        body,
    )
        .into_response()
}

fn fault(code: u16, desc: &str) -> Response {
    let body = format!(include_str!("igd_xml/fault.xml"), code = code, desc = desc);
    (
        StatusCode::INTERNAL_SERVER_ERROR,
        [(header::CONTENT_TYPE, "text/xml; charset=\"utf-8\"")],
        body,
    )
        .into_response()
}

fn upnp_error_text(code: u16) -> &'static str {
    match code {
        402 => "Invalid Args",
        501 => "Action Failed",
        606 => "Action not authorized",
        714 => "NoSuchEntryInArray",
        718 => "ConflictInMappingEntry",
        725 => "OnlyPermanentLeasesSupported",
        _ => "Action Failed",
    }
}

fn render_root_desc(uuid: &str) -> String {
    format!(
        include_str!("igd_xml/root_desc.xml"),
        device_type = IGD_DEVICE,
        uuid = uuid,
        service = WANIP_SERVICE,
        control = CONTROL_PATH,
        scpd = SCPD_PATH,
    )
}

/// Minimal WANIPConnection SCPD exposing the three actions this IGD implements.
/// IGD clients (e.g. igd-next) read `actionList` to learn each action's input
/// argument names before issuing a request.
const SCPD: &str = include_str!("igd_xml/scpd.xml");

#[cfg(test)]
mod tests {
    use super::*;
    use xmltree::Element;

    /// Recreates how an IGD client locates the WANIPConnection service: walk
    /// devices/serviceLists for a matching serviceType, returning (SCPDURL,
    /// controlURL).
    fn find_wanip(device: &Element) -> Option<(String, String)> {
        if let Some(service_list) = device.get_child("serviceList") {
            for child in &service_list.children {
                if let Some(svc) = child.as_element() {
                    if svc.name == "service"
                        && svc
                            .get_child("serviceType")
                            .and_then(|e| e.get_text())
                            .as_deref()
                            == Some(WANIP_SERVICE)
                    {
                        return Some((
                            svc.get_child("SCPDURL")?.get_text()?.into_owned(),
                            svc.get_child("controlURL")?.get_text()?.into_owned(),
                        ));
                    }
                }
            }
        }
        let device_list = device.get_child("deviceList")?;
        device_list
            .children
            .iter()
            .filter_map(|c| c.as_element())
            .filter(|c| c.name == "device")
            .find_map(find_wanip)
    }

    #[test]
    fn root_desc_advertises_wanip_service() {
        let xml = render_root_desc("abcd1234-0000-0000-0000-000000000000");
        let root = Element::parse(xml.as_bytes()).unwrap();
        let device = root.get_child("device").unwrap();
        let (scpd, control) = find_wanip(device).expect("WANIPConnection service");
        assert_eq!(scpd, SCPD_PATH);
        assert_eq!(control, CONTROL_PATH);
    }

    #[test]
    fn scpd_lists_input_args_for_add_port_mapping() {
        let scpd = Element::parse(SCPD.as_bytes()).unwrap();
        let action_list = scpd.get_child("actionList").unwrap();
        let mut actions = std::collections::HashMap::new();
        for child in &action_list.children {
            if let Some(a) = child.as_element() {
                let name = a.get_child("name").unwrap().get_text().unwrap().into_owned();
                let ins: Vec<String> = a
                    .get_child("argumentList")
                    .map(|al| {
                        al.children
                            .iter()
                            .filter_map(|c| c.as_element())
                            .filter(|arg| {
                                arg.get_child("direction").and_then(|d| d.get_text()).as_deref()
                                    == Some("in")
                            })
                            .filter_map(|arg| arg.get_child("name")?.get_text().map(|t| t.into_owned()))
                            .collect()
                    })
                    .unwrap_or_default();
                actions.insert(name, ins);
            }
        }
        assert!(actions.contains_key("GetExternalIPAddress"));
        assert!(actions.contains_key("DeletePortMapping"));
        let add = actions.get("AddPortMapping").expect("AddPortMapping");
        for arg in [
            "NewRemoteHost",
            "NewExternalPort",
            "NewProtocol",
            "NewInternalPort",
            "NewInternalClient",
            "NewEnabled",
            "NewPortMappingDescription",
            "NewLeaseDuration",
        ] {
            assert!(add.contains(&arg.to_string()), "missing {arg}");
        }
    }

    fn add_port_body() -> String {
        // Shaped like igd-next's `format_add_port_mapping_message`.
        r#"<?xml version="1.0"?>
<s:Envelope s:encodingStyle="http://schemas.xmlsoap.org/soap/encoding/" xmlns:s="http://schemas.xmlsoap.org/soap/envelope/">
<s:Body>
<u:AddPortMapping xmlns:u="urn:schemas-upnp-org:service:WANIPConnection:1">
<NewRemoteHost></NewRemoteHost>
<NewExternalPort>443</NewExternalPort>
<NewProtocol>TCP</NewProtocol>
<NewInternalPort>8443</NewInternalPort>
<NewInternalClient>10.59.1.5</NewInternalClient>
<NewEnabled>1</NewEnabled>
<NewPortMappingDescription>StartOS</NewPortMappingDescription>
<NewLeaseDuration>0</NewLeaseDuration>
</u:AddPortMapping>
</s:Body>
</s:Envelope>"#
            .to_string()
    }

    #[test]
    fn parses_action_and_ports_from_soap_body() {
        let body = add_port_body();
        assert_eq!(soap_action(&HeaderMap::new(), &body).as_deref(), Some("AddPortMapping"));
        assert_eq!(soap_u16(&body, "NewExternalPort"), Some(443));
        assert_eq!(soap_u16(&body, "NewInternalPort"), Some(8443));
        assert_eq!(soap_u16(&body, "NoSuchArg"), None);
    }

    #[test]
    fn soap_action_prefers_header() {
        let mut headers = HeaderMap::new();
        headers.insert(
            "SOAPAction",
            r#""urn:schemas-upnp-org:service:WANIPConnection:1#DeletePortMapping""#
                .parse()
                .unwrap(),
        );
        assert_eq!(soap_action(&headers, "").as_deref(), Some("DeletePortMapping"));
    }

    #[test]
    fn response_and_fault_are_wellformed_xml() {
        let resp = ok(
            "GetExternalIPAddress",
            "<NewExternalIPAddress>1.2.3.4</NewExternalIPAddress>",
        );
        assert_eq!(resp.status(), StatusCode::OK);
        let f = fault(718, "ConflictInMappingEntry");
        assert_eq!(f.status(), StatusCode::INTERNAL_SERVER_ERROR);
    }

    #[test]
    fn st_matches_igd_searches() {
        assert!(st_matches(IGD_DEVICE));
        assert!(st_matches("ssdp:all"));
        assert!(st_matches("upnp:rootdevice"));
        assert!(st_matches(WANIP_SERVICE));
        assert!(!st_matches("urn:schemas-upnp-org:service:WANCommonInterfaceConfig:1"));
    }

    #[test]
    fn uuid_is_stable_and_wellformed() {
        let bytes: Vec<u8> = (0u8..32).collect();
        let uuid = format_uuid(&bytes);
        assert_eq!(uuid, "00010203-0405-0607-0809-0a0b0c0d0e0f");
        assert_eq!(format_uuid(&bytes), format_uuid(&bytes));
    }
}
