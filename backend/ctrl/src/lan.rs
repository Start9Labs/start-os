use crate::profiles;
use crate::utils::DeserializeStdin;
use crate::utils::HandlerExtSerde;
use crate::CtrlContext;
use crate::Error;
use rpc_toolkit::{from_fn, ParentHandler};
use serde::{Deserialize, Serialize};
use std::collections::HashSet;
use std::process::Command;
use uciedit::openwrt::{Dhcp, NetworkInterface};
use uciedit::{dump_all, parse_all, Arena};

pub const LAN_INTERFACE: &str = "lan";
pub const WAN6_INTERFACE: &str = "wan6";

#[derive(Debug, Serialize, Deserialize)]
pub struct LanIpv6Response {
    pub slaac: bool,
    pub dhcpv6: bool,
    /// Prefix delegation length, e.g. 64
    pub prefix: u8,
    /// Current IPv6 address (if assigned)
    pub ip6addr: Option<String>,
    /// WAN prefix length (read-only context for the UI)
    pub wan_prefix: u8,
}

#[derive(Debug, Serialize, Deserialize)]
pub struct LanIpv6SetRequest {
    pub slaac: bool,
    pub dhcpv6: bool,
    pub prefix: u8,
}

pub fn lan<C: CtrlContext + Clone>() -> ParentHandler<C> {
    ParentHandler::new()
        .subcommand("ipv6-get", from_fn(ipv6_get::<C>).with_display_serializable())
        .subcommand("ipv6-set", from_fn(ipv6_set::<C>).with_display_serializable())
}

pub fn ipv6_get<C: CtrlContext>(ctx: C) -> Result<LanIpv6Response, Error> {
    let arena = Arena::new();
    let cfgs = parse_all(ctx.uci_root(), &arena, &["network", "dhcp"])?;

    // Read LAN interface for ip6assign and ip6addr
    let mut ip6assign: Option<String> = None;
    let mut ip6addr: Option<String> = None;
    for section in &cfgs["network"].sections {
        if section.name().as_deref() == Some(LAN_INTERFACE) {
            if let Some(iface) = section.get_typed::<NetworkInterface>()? {
                ip6assign = iface.ip6assign;
                ip6addr = iface.ip6addr;
                break;
            }
        }
    }

    // Read WAN6 interface for reqprefix
    let mut wan_prefix: u8 = 48;
    for section in &cfgs["network"].sections {
        if section.name().as_deref() == Some(WAN6_INTERFACE) {
            if let Some(iface) = section.get_typed::<NetworkInterface>()? {
                if let Some(rp) = &iface.reqprefix {
                    if rp != "auto" {
                        if let Ok(p) = rp.parse::<u8>() {
                            wan_prefix = p;
                        }
                    }
                }
                break;
            }
        }
    }

    // Read DHCP LAN section for RA/DHCPv6
    let mut slaac = false;
    let mut dhcpv6 = false;
    for section in &cfgs["dhcp"].sections {
        if section.name().as_deref() == Some(LAN_INTERFACE) {
            if let Some(dhcp) = section.get_typed::<Dhcp>()? {
                slaac = dhcp.ra.as_deref() == Some("server");
                dhcpv6 = dhcp.dhcpv6.as_deref() == Some("server");
                break;
            }
        }
    }

    let prefix = ip6assign
        .and_then(|s| s.parse::<u8>().ok())
        .unwrap_or(64);

    Ok(LanIpv6Response {
        slaac,
        dhcpv6,
        prefix,
        ip6addr,
        wan_prefix,
    })
}

pub fn ipv6_set<C: CtrlContext>(
    ctx: C,
    DeserializeStdin(req): DeserializeStdin<LanIpv6SetRequest>,
) -> Result<(), Error> {
    // DHCPv6 requires SLAAC (RA) — normalize to prevent inconsistent state
    let req = LanIpv6SetRequest {
        dhcpv6: req.slaac && req.dhcpv6,
        ..req
    };

    let mut retries = 4;
    loop {
        let arena = Arena::new();
        let mut cfgs = parse_all(ctx.uci_root(), &arena, &["network", "dhcp", "startwrt"])?;

        // Update LAN network interface ip6assign
        for section in &mut cfgs["network"].sections {
            if section.name().as_deref() == Some(LAN_INTERFACE) {
                if let Some(mut iface) = section.get_typed::<NetworkInterface>()? {
                    if req.slaac || req.dhcpv6 {
                        iface.ip6assign = Some(req.prefix.to_string());
                    } else {
                        iface.ip6assign = None;
                    }
                    section.set(&iface)?;
                    break;
                }
            }
        }

        // Update DHCP LAN section RA/DHCPv6
        for section in &mut cfgs["dhcp"].sections {
            if section.name().as_deref() == Some(LAN_INTERFACE) {
                if let Some(mut dhcp) = section.get_typed::<Dhcp>()? {
                    dhcp.ra = Some(if req.slaac { "server" } else { "disabled" }.to_string());
                    dhcp.dhcpv6 =
                        Some(if req.dhcpv6 { "server" } else { "disabled" }.to_string());
                    section.set(&dhcp)?;
                    break;
                }
            }
        }

        // Update all profile DHCP/network sections to match global IPv6 state
        let profile_interfaces: HashSet<String> = profiles::list_config(ctx.clone(), &cfgs)?
            .into_iter()
            .map(|p| p.interface)
            .collect();

        for section in &mut cfgs["network"].sections {
            if let Some(name) = section.name() {
                if profile_interfaces.contains(name.as_ref()) {
                    if let Some(mut iface) = section.get_typed::<NetworkInterface>()? {
                        iface.ip6assign = if req.slaac || req.dhcpv6 {
                            Some(req.prefix.to_string())
                        } else {
                            None
                        };
                        section.set(&iface)?;
                    }
                }
            }
        }

        for section in &mut cfgs["dhcp"].sections {
            if let Some(name) = section.name() {
                if profile_interfaces.contains(name.as_ref()) {
                    if let Some(mut dhcp) = section.get_typed::<Dhcp>()? {
                        dhcp.ra = Some(if req.slaac { "server" } else { "disabled" }.to_string());
                        dhcp.dhcpv6 =
                            Some(if req.dhcpv6 { "server" } else { "disabled" }.to_string());
                        section.set(&dhcp)?;
                    }
                }
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
                    // Restart odhcpd first so it can send a deprecation RA
                    // (prefix lifetimes=0) while the network is still up.
                    // Without this, disabling IPv6 leaves clients with stale
                    // SLAAC addresses until they naturally expire.
                    let _ = Command::new("/etc/init.d/odhcpd")
                        .arg("restart")
                        .spawn()
                        .and_then(|mut c| c.wait());
                    let _ = Command::new("/etc/init.d/network")
                        .arg("restart")
                        .spawn()
                        .and_then(|mut c| c.wait());
                }
                return Ok(());
            }
        }
    }
}
