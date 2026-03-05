use crate::utils::DeserializeStdin;
use crate::utils::HandlerExtSerde;
use crate::CtrlContext;
use crate::Error;
use rpc_toolkit::{from_fn, ParentHandler};
use serde::{Deserialize, Serialize};
use std::process::Command;
use uciedit::openwrt::{InterfaceProto, NetworkInterface};
use uciedit::{dump_all, parse_all, Arena};

pub const WAN6_INTERFACE: &str = "wan6";

#[derive(Debug, Clone, Serialize, Deserialize, PartialEq, Eq)]
#[serde(rename_all = "lowercase")]
pub enum WanIpv6Mode {
    Disabled,
    Slaac,
    #[serde(rename = "dhcpv6")]
    Dhcpv6,
    Static,
    #[serde(rename = "6rd")]
    SixRd,
}

#[derive(Debug, Serialize, Deserialize)]
pub struct WanIpv6Response {
    pub mode: WanIpv6Mode,
    /// Static: IPv6 address
    pub address: Option<String>,
    /// Static: prefix length, e.g. "/64"
    pub prefix: Option<String>,
    /// Static: gateway
    pub gateway: Option<String>,
    /// 6RD: peer IPv4 address
    pub peer_ipv4: Option<String>,
    /// 6RD: prefix length, e.g. "/32"
    pub mask: Option<String>,
    /// 6RD: border relay prefix
    pub border_relay: Option<String>,
}

#[derive(Debug, Serialize, Deserialize)]
pub struct WanIpv6SetRequest {
    pub mode: WanIpv6Mode,
    pub address: Option<String>,
    pub prefix: Option<String>,
    pub gateway: Option<String>,
    pub peer_ipv4: Option<String>,
    pub mask: Option<String>,
    pub border_relay: Option<String>,
}

pub fn wan<C: CtrlContext + Clone>() -> ParentHandler<C> {
    ParentHandler::new()
        .subcommand("ipv6-get", from_fn(ipv6_get::<C>).with_display_serializable())
        .subcommand("ipv6-set", from_fn(ipv6_set::<C>).with_display_serializable())
}

pub fn ipv6_get<C: CtrlContext>(ctx: C) -> Result<WanIpv6Response, Error> {
    let arena = Arena::new();
    let cfgs = parse_all(ctx.uci_root(), &arena, &["network"])?;

    // Find wan6 interface
    let mut wan6: Option<NetworkInterface> = None;
    for section in &cfgs["network"].sections {
        if section.name().as_deref() == Some(WAN6_INTERFACE) {
            if let Some(iface) = section.get_typed::<NetworkInterface>()? {
                wan6 = Some(iface);
                break;
            }
        }
    }

    let Some(iface) = wan6 else {
        return Ok(WanIpv6Response {
            mode: WanIpv6Mode::Disabled,
            address: None,
            prefix: None,
            gateway: None,
            peer_ipv4: None,
            mask: None,
            border_relay: None,
        });
    };

    let mode = match iface.proto {
        InterfaceProto::NONE => WanIpv6Mode::Disabled,
        InterfaceProto::DHCPV6 => {
            if iface.reqaddress.as_deref() == Some("force") {
                WanIpv6Mode::Dhcpv6
            } else {
                WanIpv6Mode::Slaac
            }
        }
        InterfaceProto::STATIC => WanIpv6Mode::Static,
        InterfaceProto::SIXRD => WanIpv6Mode::SixRd,
        _ => WanIpv6Mode::Disabled,
    };

    // Parse ip6addr into address + prefix
    let (address, prefix) = match &iface.ip6addr {
        Some(ip6addr) if !ip6addr.is_empty() => {
            let parts: Vec<&str> = ip6addr.splitn(2, '/').collect();
            (
                Some(parts[0].to_string()),
                parts.get(1).map(|p| format!("/{}", p)),
            )
        }
        _ => (None, None),
    };

    Ok(WanIpv6Response {
        mode,
        address,
        prefix,
        gateway: iface.ip6gw,
        peer_ipv4: iface.peeraddr,
        mask: iface.ip6prefixlen.map(|p| format!("/{}", p)),
        border_relay: iface.ip6prefix,
    })
}

pub fn ipv6_set<C: CtrlContext>(
    ctx: C,
    DeserializeStdin(req): DeserializeStdin<WanIpv6SetRequest>,
) -> Result<(), Error> {
    let mut retries = 4;
    loop {
        let arena = Arena::new();
        let mut cfgs = parse_all(ctx.uci_root(), &arena, &["network"])?;

        // Find wan6 interface section index
        let wan6_idx = cfgs["network"]
            .sections
            .iter()
            .position(|s| s.name().as_deref() == Some(WAN6_INTERFACE));

        if req.mode == WanIpv6Mode::Disabled {
            // If disabling and section exists, set proto to none and clear IPv6 options
            if let Some(idx) = wan6_idx {
                let section = &mut cfgs["network"].sections[idx];
                if let Some(mut iface) = section.get_typed::<NetworkInterface>()? {
                    iface.proto = InterfaceProto::NONE;
                    iface.reqaddress = None;
                    iface.reqprefix = None;
                    iface.ip6addr = None;
                    iface.ip6gw = None;
                    iface.peeraddr = None;
                    iface.ip6prefix = None;
                    iface.ip6prefixlen = None;
                    section.set(&iface)?;
                }
            }
            // If no wan6 section exists and we're disabling, nothing to do
        } else {
            // Build the new interface config
            let new_iface = match &req.mode {
                WanIpv6Mode::Slaac => NetworkInterface {
                    device: "@wan".to_string(),
                    proto: InterfaceProto::DHCPV6,
                    reqaddress: Some("try".to_string()),
                    reqprefix: Some("auto".to_string()),
                    ..Default::default()
                },
                WanIpv6Mode::Dhcpv6 => NetworkInterface {
                    device: "@wan".to_string(),
                    proto: InterfaceProto::DHCPV6,
                    reqaddress: Some("force".to_string()),
                    reqprefix: Some("auto".to_string()),
                    ..Default::default()
                },
                WanIpv6Mode::Static => {
                    let ip6addr = match (&req.address, &req.prefix) {
                        (Some(addr), Some(pfx)) => Some(format!("{}{}", addr, pfx)),
                        (Some(addr), None) => Some(addr.clone()),
                        _ => None,
                    };
                    NetworkInterface {
                        device: "@wan".to_string(),
                        proto: InterfaceProto::STATIC,
                        ip6addr,
                        ip6gw: req.gateway.clone(),
                        ..Default::default()
                    }
                }
                WanIpv6Mode::SixRd => NetworkInterface {
                    device: "@wan".to_string(),
                    proto: InterfaceProto::SIXRD,
                    peeraddr: req.peer_ipv4.clone(),
                    ip6prefix: req.border_relay.clone(),
                    ip6prefixlen: req.mask.as_ref().map(|m| m.trim_start_matches('/').to_string()),
                    ..Default::default()
                },
                WanIpv6Mode::Disabled => unreachable!(),
            };

            if let Some(idx) = wan6_idx {
                // Update existing section, always preserving device (managed by ethernet.rs)
                let section = &mut cfgs["network"].sections[idx];
                let mut updated = new_iface;
                if let Some(existing) = section.get_typed::<NetworkInterface>()? {
                    updated.device = existing.device;
                }
                section.set(&updated)?;
            } else {
                // Create new wan6 section
                cfgs["network"].append(&new_iface, Some(WAN6_INTERFACE))?;
            }
        }

        match dump_all(ctx.uci_root(), cfgs) {
            Err(uciedit::Error::Conflict { .. }) if retries > 0 => {
                retries -= 1;
                continue;
            }
            Err(err) => return Err(err.into()),
            Ok(()) => {
                if ctx.effectful() {
                    let _ = Command::new("/etc/init.d/network")
                        .arg("restart")
                        .spawn()
                        .and_then(|mut c| c.wait());
                    let _ = Command::new("/etc/init.d/odhcpd")
                        .arg("restart")
                        .spawn()
                        .and_then(|mut c| c.wait());
                }
                return Ok(());
            }
        }
    }
}
