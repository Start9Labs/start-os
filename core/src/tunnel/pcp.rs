//! Server-side PCP (RFC 6887) for StartTunnel, offered to connected WireGuard
//! peers alongside the UPnP IGD ([`crate::tunnel::igd`]).
//!
//! PCP is the modern, preferred port-control protocol, so StartOS clients try
//! it first (see [`crate::net::port_map`]). It is inherently secure for our use
//! because a MAP creates a mapping for the *requesting* host: we force the
//! target to the source (peer) address, so a peer can only forward to itself.
//!
//! The socket is `SO_BINDTODEVICE`-bound to the WireGuard interface, so the PCP
//! server is never reachable from the VPS's public interface, and only
//! configured peers are honored.

use std::net::{IpAddr, Ipv4Addr, SocketAddrV4};
use std::time::{Duration, Instant};

use socket2::{Domain, Protocol, SockAddr, Socket, Type};
use tokio::net::UdpSocket;

use crate::net::pcp_hostname::{
    RESULT_UNSUPP_HOSTNAME, encode_hostname_option, parse_hostname_options,
};
use crate::net::pcp_portset::{PortSet, encode_port_set_option, parse_port_set_options};
use crate::prelude::*;
use crate::tunnel::context::TunnelContext;
use crate::tunnel::igd::{
    apply_peer_forward, apply_peer_forward_range, bind_to_wireguard, external_ipv4, is_known_client,
};
use crate::tunnel::wg::WIREGUARD_INTERFACE_NAME;

const PCP_PORT: u16 = 5351;
const PCP_VERSION: u8 = 2;
const OPCODE_MAP: u8 = 1;
const RESPONSE_BIT: u8 = 0x80;
const MAP_REQUEST_LEN: usize = 60;
const MAP_RESPONSE_LEN: usize = 60;
const HEADER_LEN: usize = 24;
/// Cap the lease we grant; the client re-asserts well within this.
const MAX_LIFETIME_SECONDS: u32 = 3600;

// PCP result codes (RFC 6887 §7.4).
const SUCCESS: u8 = 0;
const UNSUPP_VERSION: u8 = 1;
const NOT_AUTHORIZED: u8 = 2;
const MALFORMED_REQUEST: u8 = 3;
const UNSUPP_OPCODE: u8 = 4;
const MALFORMED_OPTION: u8 = 6;
const NO_RESOURCES: u8 = 8;
const CANNOT_PROVIDE_EXTERNAL: u8 = 11;

/// PCP protocol field value for TCP (the only transport the SNI demux handles).
const PROTO_TCP: u8 = 6;
/// Largest PORT_SET range we will grant in one MAP (RFC 7753 lets us grant
/// fewer than requested; the client skips the range if it can't get them all).
const MAX_PORT_SET: u16 = 1024;

/// Run the PCP server for the life of the tunnel, self-restarting on error.
pub async fn run(ctx: TunnelContext) {
    let started = Instant::now();
    loop {
        if let Err(e) = serve(&ctx, started).await {
            tracing::warn!("PCP server failed, retrying: {e}");
            tokio::time::sleep(Duration::from_secs(5)).await;
        }
    }
}

fn socket() -> Result<UdpSocket, Error> {
    let socket =
        Socket::new(Domain::IPV4, Type::DGRAM, Some(Protocol::UDP)).with_kind(ErrorKind::Network)?;
    socket.set_reuse_address(true).with_kind(ErrorKind::Network)?;
    bind_to_wireguard(&socket)?;
    socket
        .bind(&SockAddr::from(SocketAddrV4::new(
            Ipv4Addr::UNSPECIFIED,
            PCP_PORT,
        )))
        .with_kind(ErrorKind::Network)?;
    socket.set_nonblocking(true).with_kind(ErrorKind::Network)?;
    UdpSocket::from_std(socket.into()).with_kind(ErrorKind::Network)
}

async fn serve(ctx: &TunnelContext, started: Instant) -> Result<(), Error> {
    let socket = socket()?;
    tracing::info!("PCP server listening on {WIREGUARD_INTERFACE_NAME}:{PCP_PORT}");
    let mut buf = [0u8; 1100];
    loop {
        let (n, from) = socket.recv_from(&mut buf).await.with_kind(ErrorKind::Network)?;
        let IpAddr::V4(peer) = from.ip() else {
            continue;
        };
        let epoch = started.elapsed().as_secs() as u32;
        if let Some(resp) = handle(ctx, peer, &buf[..n], epoch).await {
            socket.send_to(&resp, from).await.ok();
        }
    }
}

fn ipv4_mapped(ip: Ipv4Addr) -> [u8; 16] {
    let mut out = [0u8; 16];
    out[10] = 0xff;
    out[11] = 0xff;
    out[12..16].copy_from_slice(&ip.octets());
    out
}

/// A bare header-only response carrying just a result code (used for
/// version/opcode errors before the request body is trusted).
fn error_response(opcode: u8, result: u8, epoch: u32) -> Vec<u8> {
    let mut r = vec![0u8; HEADER_LEN];
    r[0] = PCP_VERSION;
    r[1] = RESPONSE_BIT | (opcode & 0x7f);
    r[3] = result;
    r[8..12].copy_from_slice(&epoch.to_be_bytes());
    r
}

/// A MAP response, echoing the request's nonce/protocol/internal port.
fn map_response(
    result: u8,
    req: &[u8],
    internal_port: u16,
    external_port: u16,
    external_ip: Ipv4Addr,
    lifetime: u32,
    epoch: u32,
) -> Vec<u8> {
    let mut r = vec![0u8; MAP_RESPONSE_LEN];
    r[0] = PCP_VERSION;
    r[1] = RESPONSE_BIT | OPCODE_MAP;
    r[3] = result;
    r[4..8].copy_from_slice(&lifetime.to_be_bytes());
    r[8..12].copy_from_slice(&epoch.to_be_bytes());
    // Echo the 12-byte mapping nonce and the protocol from the request.
    r[24..36].copy_from_slice(&req[24..36]);
    r[36] = req[36];
    r[40..42].copy_from_slice(&internal_port.to_be_bytes());
    r[42..44].copy_from_slice(&external_port.to_be_bytes());
    r[44..60].copy_from_slice(&ipv4_mapped(external_ip));
    r
}

/// A MAP response that echoes back the granted HOSTNAME options (RFC echoes
/// successfully-processed options). The base response is 32-bit aligned, so the
/// appended options stay aligned.
fn map_response_with_hostnames(
    result: u8,
    req: &[u8],
    internal_port: u16,
    external_port: u16,
    external_ip: Ipv4Addr,
    lifetime: u32,
    epoch: u32,
    hostnames: &[String],
) -> Vec<u8> {
    let mut r = map_response(
        result,
        req,
        internal_port,
        external_port,
        external_ip,
        lifetime,
        epoch,
    );
    for name in hostnames {
        encode_hostname_option(&mut r, name);
    }
    r
}

/// A MAP response echoing the granted PORT_SET (RFC 7753): the opcode's
/// external port is the first port of the granted range and the option carries
/// the granted size.
fn map_response_with_port_set(
    result: u8,
    req: &[u8],
    internal_port: u16,
    external_port: u16,
    external_ip: Ipv4Addr,
    lifetime: u32,
    epoch: u32,
    granted: u16,
) -> Vec<u8> {
    let mut r = map_response(
        result,
        req,
        internal_port,
        external_port,
        external_ip,
        lifetime,
        epoch,
    );
    encode_port_set_option(
        &mut r,
        &PortSet {
            size: granted,
            first_internal_port: internal_port,
            parity: false,
        },
    );
    r
}

async fn handle(ctx: &TunnelContext, peer: Ipv4Addr, req: &[u8], epoch: u32) -> Option<Vec<u8>> {
    if req.len() < HEADER_LEN {
        return None;
    }
    let opcode = req[1] & 0x7f;
    // Ignore responses (R bit set) and only handle requests.
    if req[1] & RESPONSE_BIT != 0 {
        return None;
    }
    if req[0] != PCP_VERSION {
        return Some(error_response(opcode, UNSUPP_VERSION, epoch));
    }
    if opcode != OPCODE_MAP {
        return Some(error_response(opcode, UNSUPP_OPCODE, epoch));
    }
    if req.len() < MAP_REQUEST_LEN {
        return Some(error_response(opcode, MALFORMED_REQUEST, epoch));
    }

    // Peer authorization: only a configured WireGuard client may map.
    if !is_known_client(ctx, peer).await {
        return Some(map_response(NOT_AUTHORIZED, req, 0, 0, Ipv4Addr::UNSPECIFIED, 0, epoch));
    }

    let lifetime = u32::from_be_bytes([req[4], req[5], req[6], req[7]]);
    let internal_port = u16::from_be_bytes([req[40], req[41]]);
    let suggested_external_port = u16::from_be_bytes([req[42], req[43]]);

    let Some(external_ip) = external_ipv4(ctx).await else {
        return Some(map_response(
            CANNOT_PROVIDE_EXTERNAL,
            req,
            internal_port,
            0,
            Ipv4Addr::UNSPECIFIED,
            0,
            epoch,
        ));
    };
    let external_port = if suggested_external_port != 0 {
        suggested_external_port
    } else {
        internal_port
    };

    if internal_port == 0 {
        return Some(map_response(
            MALFORMED_REQUEST,
            req,
            internal_port,
            external_port,
            external_ip,
            0,
            epoch,
        ));
    }

    // PCP HOSTNAME extension: if the request carries HOSTNAME option(s), this is
    // a SNI-demultiplexed binding on a shared external port — handled by the SNI
    // demux dataplane, not an nft DNAT.
    let hostnames = match parse_hostname_options(req.get(MAP_REQUEST_LEN..).unwrap_or(&[])) {
        Ok(h) => h,
        Err(()) => {
            return Some(map_response(
                MALFORMED_OPTION,
                req,
                internal_port,
                external_port,
                external_ip,
                0,
                epoch,
            ));
        }
    };
    if !hostnames.is_empty() {
        let nonce: [u8; 12] = req[24..36].try_into().unwrap();
        if req[36] != PROTO_TCP {
            return Some(map_response(
                RESULT_UNSUPP_HOSTNAME,
                req,
                internal_port,
                external_port,
                external_ip,
                0,
                epoch,
            ));
        }
        if lifetime == 0 {
            ctx.sni
                .unregister(external_ip, external_port, &hostnames, nonce);
            return Some(map_response_with_hostnames(
                SUCCESS,
                req,
                internal_port,
                external_port,
                external_ip,
                0,
                epoch,
                &hostnames,
            ));
        }
        let target = SocketAddrV4::new(peer, internal_port);
        let granted = lifetime.min(MAX_LIFETIME_SECONDS);
        return match ctx
            .sni
            .register(external_ip, external_port, &hostnames, target, nonce, granted)
        {
            Ok(()) => Some(map_response_with_hostnames(
                SUCCESS,
                req,
                internal_port,
                external_port,
                external_ip,
                granted,
                epoch,
                &hostnames,
            )),
            Err(code) => Some(map_response(
                code,
                req,
                internal_port,
                external_port,
                external_ip,
                0,
                epoch,
            )),
        };
    }

    // PCP PORT_SET extension (RFC 7753): map a contiguous range in one request.
    let port_set = match parse_port_set_options(req.get(MAP_REQUEST_LEN..).unwrap_or(&[])) {
        Ok(ps) => ps,
        Err(()) => {
            return Some(map_response(
                MALFORMED_OPTION,
                req,
                internal_port,
                external_port,
                external_ip,
                0,
                epoch,
            ));
        }
    };
    if let Some(ps) = port_set {
        if ps.size == 0 {
            return Some(map_response(
                MALFORMED_OPTION,
                req,
                internal_port,
                external_port,
                external_ip,
                0,
                epoch,
            ));
        }
        if ps.size > 1 {
            let granted = ps.size.min(MAX_PORT_SET);
            let source = SocketAddrV4::new(external_ip, external_port);
            let target = SocketAddrV4::new(peer, internal_port);
            if lifetime == 0 {
                remove_peer_forward(ctx, peer, internal_port).await;
                return Some(map_response_with_port_set(
                    SUCCESS,
                    req,
                    internal_port,
                    external_port,
                    external_ip,
                    0,
                    epoch,
                    granted,
                ));
            }
            return match apply_peer_forward_range(ctx, source, target, granted, peer, "PCP").await {
                Ok(()) => {
                    let granted_lifetime = lifetime.min(MAX_LIFETIME_SECONDS);
                    Some(map_response_with_port_set(
                        SUCCESS,
                        req,
                        internal_port,
                        external_port,
                        external_ip,
                        granted_lifetime,
                        epoch,
                        granted,
                    ))
                }
                Err(_) => Some(map_response(
                    NO_RESOURCES,
                    req,
                    internal_port,
                    external_port,
                    external_ip,
                    0,
                    epoch,
                )),
            };
        }
    }

    // Lifetime 0 deletes the mapping (RFC 6887 §15).
    if lifetime == 0 {
        remove_peer_forward(ctx, peer, internal_port).await;
        return Some(map_response(SUCCESS, req, internal_port, external_port, external_ip, 0, epoch));
    }

    // Secure: force the target to the requesting peer's own tunnel IP.
    let source = SocketAddrV4::new(external_ip, external_port);
    let target = SocketAddrV4::new(peer, internal_port);
    match apply_peer_forward(ctx, source, target, peer).await {
        Ok(()) => {
            let granted = lifetime.min(MAX_LIFETIME_SECONDS);
            Some(map_response(
                SUCCESS,
                req,
                internal_port,
                external_port,
                external_ip,
                granted,
                epoch,
            ))
        }
        // The external port is taken by another mapping; the client may retry.
        Err(_) => Some(map_response(
            NO_RESOURCES,
            req,
            internal_port,
            external_port,
            external_ip,
            0,
            epoch,
        )),
    }
}

/// Remove the peer's forward to `(peer, internal_port)`, if any. PCP identifies
/// a mapping by (protocol, internal port, client); we forward both protocols on
/// one entry, so match by target.
async fn remove_peer_forward(ctx: &TunnelContext, peer: Ipv4Addr, internal_port: u16) {
    let target = SocketAddrV4::new(peer, internal_port);
    let source = ctx
        .db
        .peek()
        .await
        .as_port_forwards()
        .de()
        .ok()
        .and_then(|pf| {
            pf.0.iter()
                .find(|(_, entry)| entry.target == target)
                .map(|(source, _)| *source)
        });
    let Some(source) = source else {
        return;
    };
    ctx.db
        .mutate(|db| db.as_port_forwards_mut().remove(&source).map(|_| ()))
        .await
        .result
        .log_err();
    if let Some(rc) = ctx.active_forwards.mutate(|m| m.remove(&source)) {
        drop(rc);
        ctx.forward.gc().await.log_err();
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    fn map_request(nonce: [u8; 12], lifetime: u32, internal: u16, external: u16) -> Vec<u8> {
        let mut r = vec![0u8; MAP_REQUEST_LEN];
        r[0] = PCP_VERSION;
        r[1] = OPCODE_MAP;
        r[4..8].copy_from_slice(&lifetime.to_be_bytes());
        r[24..36].copy_from_slice(&nonce);
        r[36] = 6; // TCP
        r[40..42].copy_from_slice(&internal.to_be_bytes());
        r[42..44].copy_from_slice(&external.to_be_bytes());
        r
    }

    #[test]
    fn map_response_echoes_nonce_and_encodes_external() {
        let nonce = [1u8, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12];
        let req = map_request(nonce, 3600, 8443, 443);
        let resp = map_response(SUCCESS, &req, 8443, 443, Ipv4Addr::new(203, 0, 113, 7), 3600, 42);
        assert_eq!(resp.len(), MAP_RESPONSE_LEN);
        assert_eq!(resp[0], PCP_VERSION);
        assert_eq!(resp[1], RESPONSE_BIT | OPCODE_MAP);
        assert_eq!(resp[3], SUCCESS);
        assert_eq!(u32::from_be_bytes([resp[4], resp[5], resp[6], resp[7]]), 3600);
        assert_eq!(u32::from_be_bytes([resp[8], resp[9], resp[10], resp[11]]), 42);
        assert_eq!(&resp[24..36], &nonce);
        assert_eq!(resp[36], 6);
        assert_eq!(u16::from_be_bytes([resp[40], resp[41]]), 8443);
        assert_eq!(u16::from_be_bytes([resp[42], resp[43]]), 443);
        assert_eq!(&resp[44..60], &ipv4_mapped(Ipv4Addr::new(203, 0, 113, 7)));
    }

    #[test]
    fn ipv4_mapped_is_rfc_format() {
        assert_eq!(
            ipv4_mapped(Ipv4Addr::new(192, 0, 2, 1)),
            [0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0xff, 0xff, 192, 0, 2, 1]
        );
    }

    #[test]
    fn error_response_carries_code() {
        let r = error_response(OPCODE_MAP, UNSUPP_VERSION, 7);
        assert_eq!(r.len(), HEADER_LEN);
        assert_eq!(r[0], PCP_VERSION);
        assert_eq!(r[1], RESPONSE_BIT | OPCODE_MAP);
        assert_eq!(r[3], UNSUPP_VERSION);
    }
}
