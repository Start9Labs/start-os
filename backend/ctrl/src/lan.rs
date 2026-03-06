use crate::profiles;
use crate::utils::DeserializeStdin;
use crate::utils::HandlerExtSerde;
use crate::CtrlContext;
use crate::Error;
use rpc_toolkit::{from_fn, ParentHandler};
use serde::{Deserialize, Serialize};
use std::collections::HashSet;
use std::net::Ipv4Addr;
use std::process::Command;
use uciedit::openwrt::{Dhcp, NetworkInterface, NetworkRule};
use uciedit::{dump_all, parse_all, Arena};

pub const LAN_INTERFACE: &str = "lan";
pub const WAN6_INTERFACE: &str = "wan6";
pub const LAN_NETMASK: Ipv4Addr = Ipv4Addr::new(255, 255, 255, 0);

#[derive(Debug, Serialize, Deserialize)]
pub struct LanIpv4Response {
    pub address: String,
    pub netmask: String,
}

#[derive(Debug, Serialize, Deserialize)]
pub struct LanIpv4SetRequest {
    pub address: String,
}

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
        .subcommand("ipv4-get", from_fn(ipv4_get::<C>).with_display_serializable())
        .subcommand("ipv4-set", from_fn(ipv4_set::<C>).with_display_serializable())
        .subcommand("ipv6-get", from_fn(ipv6_get::<C>).with_display_serializable())
        .subcommand("ipv6-set", from_fn(ipv6_set::<C>).with_display_serializable())
}

pub fn ipv4_get<C: CtrlContext>(ctx: C) -> Result<LanIpv4Response, Error> {
    let arena = Arena::new();
    let cfgs = parse_all(ctx.uci_root(), &arena, &["network"])?;

    for section in &cfgs["network"].sections {
        if section.name().as_deref() == Some(LAN_INTERFACE) {
            if let Some(iface) = section.get_typed::<NetworkInterface>()? {
                return Ok(LanIpv4Response {
                    address: iface
                        .ipaddr
                        .map(|ip| ip.to_string())
                        .unwrap_or_else(|| "192.168.1.1".to_string()),
                    netmask: iface
                        .netmask
                        .map(|m| m.to_string())
                        .unwrap_or_else(|| LAN_NETMASK.to_string()),
                });
            }
        }
    }

    Ok(LanIpv4Response {
        address: "192.168.1.1".to_string(),
        netmask: LAN_NETMASK.to_string(),
    })
}

pub fn ipv4_set<C: CtrlContext>(
    ctx: C,
    DeserializeStdin(req): DeserializeStdin<LanIpv4SetRequest>,
) -> Result<(), Error> {
    let address: Ipv4Addr = req
        .address
        .parse()
        .map_err(|_| Error::other(format!("Invalid IPv4 address: {}", req.address)))?;

    let mut retries = 4;
    loop {
        let arena = Arena::new();
        let mut cfgs = parse_all(ctx.uci_root(), &arena, &["network", "startwrt"])?;

        // Update LAN interface, capturing old IP to detect network block changes
        let mut old_address: Option<Ipv4Addr> = None;
        let mut found = false;
        for section in &mut cfgs["network"].sections {
            if section.name().as_deref() == Some(LAN_INTERFACE) {
                if let Some(mut iface) = section.get_typed::<NetworkInterface>()? {
                    old_address = iface.ipaddr;
                    iface.ipaddr = Some(address);
                    iface.netmask = Some(LAN_NETMASK);
                    section.set(&iface)?;
                    found = true;
                    break;
                }
            }
        }
        if !found {
            return Err(Error::other("LAN interface not found in network config"));
        }

        // Detect network block change (first two octets differ)
        let block_changed = match old_address {
            Some(old) => {
                old.octets()[0] != address.octets()[0]
                    || old.octets()[1] != address.octets()[1]
            }
            None => false,
        };

        // If the network block changed, update all profile interfaces and routing rules
        if block_changed {
            let profile_interfaces: HashSet<String> = profiles::list_config(ctx.clone(), &cfgs)?
                .into_iter()
                .filter(|p| p.interface != LAN_INTERFACE)
                .map(|p| p.interface)
                .collect();

            update_profile_ips_for_block_change(&mut cfgs, address, &profile_interfaces)?;
        }

        match dump_all(ctx.uci_root(), cfgs) {
            Err(uciedit::Error::Conflict { .. }) if retries > 0 => {
                retries -= 1;
                continue;
            }
            Err(err) => return Err(err.into()),
            Ok(()) => {
                if ctx.effectful() {
                    restart_network_services(block_changed);
                }
                return Ok(());
            }
        }
    }
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
        let mut found_network = false;
        for section in &mut cfgs["network"].sections {
            if section.name().as_deref() == Some(LAN_INTERFACE) {
                if let Some(mut iface) = section.get_typed::<NetworkInterface>()? {
                    if req.slaac || req.dhcpv6 {
                        iface.ip6assign = Some(req.prefix.to_string());
                    } else {
                        iface.ip6assign = None;
                    }
                    section.set(&iface)?;
                    found_network = true;
                    break;
                }
            }
        }
        if !found_network {
            return Err(Error::other("LAN interface not found in network config"));
        }

        // Update DHCP LAN section RA/DHCPv6
        let mut found_dhcp = false;
        for section in &mut cfgs["dhcp"].sections {
            if section.name().as_deref() == Some(LAN_INTERFACE) {
                if let Some(mut dhcp) = section.get_typed::<Dhcp>()? {
                    dhcp.ra = Some(if req.slaac { "server" } else { "disabled" }.to_string());
                    dhcp.dhcpv6 =
                        Some(if req.dhcpv6 { "server" } else { "disabled" }.to_string());
                    section.set(&dhcp)?;
                    found_dhcp = true;
                    break;
                }
            }
        }
        if !found_dhcp {
            return Err(Error::other("LAN section not found in DHCP config"));
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

/// Update all profile network interface IPs and routing rules when the network
/// block (first two octets) changes. Preserves each profile's 3rd/4th octets.
pub fn update_profile_ips_for_block_change(
    cfgs: &mut uciedit::Configs,
    new_address: Ipv4Addr,
    profile_interfaces: &HashSet<String>,
) -> Result<(), Error> {
    let new_oct = new_address.octets();

    // Update profile network interface IPs
    for section in &mut cfgs["network"].sections {
        if let Some(name) = section.name() {
            if profile_interfaces.contains(name.as_ref()) {
                if let Some(mut iface) = section.get_typed::<NetworkInterface>()? {
                    if let Some(old_ip) = iface.ipaddr {
                        let old_oct = old_ip.octets();
                        iface.ipaddr = Some(Ipv4Addr::new(
                            new_oct[0], new_oct[1], old_oct[2], old_oct[3],
                        ));
                        section.set(&iface)?;
                    }
                }
            }
        }
    }

    // Update policy routing rules (prr_<interface>) src field
    for section in &mut cfgs["network"].sections {
        if let Some(name) = section.name() {
            if let Some(iface_name) = name.strip_prefix("prr_") {
                if profile_interfaces.contains(iface_name) {
                    if let Some(mut rule) = section.get_typed::<NetworkRule>()? {
                        if let Some(ip_part) = rule.src.split('/').next() {
                            if let Ok(old_src) = ip_part.parse::<Ipv4Addr>() {
                                let old_oct = old_src.octets();
                                rule.src = format!(
                                    "{}.{}.{}.0/24",
                                    new_oct[0], new_oct[1], old_oct[2]
                                );
                                section.set(&rule)?;
                            }
                        }
                    }
                }
            }
        }
    }

    Ok(())
}

/// Synchronous restart of network services after an IP change.
/// Clears DHCP leases, restarts network, optionally restarts WiFi
/// (when the network block changed), then restarts dnsmasq.
pub fn restart_network_services(block_changed: bool) {
    // Clear DHCP leases — old leases reference the previous
    // subnet and would cause clients to receive stale IPs.
    let _ = std::fs::remove_file("/tmp/dhcp.leases");
    let _ = Command::new("/etc/init.d/network")
        .arg("restart")
        .spawn()
        .and_then(|mut c| c.wait());
    if block_changed {
        // Restart WiFi so hostapd re-reads vlan_file and
        // re-establishes dynamic VLAN assignments after
        // network interfaces were torn down and recreated.
        let _ = Command::new("wifi")
            .spawn()
            .and_then(|mut c| c.wait());
    }
    // Restart dnsmasq AFTER network and wifi so it
    // binds to interfaces with their new IPs and
    // generates correct DHCP pools for each subnet.
    let _ = Command::new("/etc/init.d/dnsmasq")
        .arg("restart")
        .spawn()
        .and_then(|mut c| c.wait());
}

#[cfg(test)]
mod tests {
    use super::*;
    use rpc_toolkit::Context;
    use std::path::PathBuf;
    use std::sync::Arc;
    use tokio::runtime::Runtime;

    #[derive(Clone)]
    struct TestContext(PathBuf);

    impl Context for TestContext {
        fn runtime(&self) -> Option<Arc<Runtime>> {
            None
        }
    }

    impl CtrlContext for TestContext {
        fn uci_root(&self) -> PathBuf {
            self.0.clone()
        }
        fn effectful(&self) -> bool {
            false
        }
    }

    /// Write minimal network + dhcp UCI configs for testing.
    fn setup_fixtures(dir: &std::path::Path) {
        std::fs::write(
            dir.join("network"),
            "\
config interface 'lan'
\toption device 'br-lan'
\toption proto 'static'
\toption ipaddr '192.168.1.1'
\toption netmask '255.255.0.0'
\toption ip6assign '60'

config interface 'wan6'
\toption device '@wan'
\toption proto 'dhcpv6'
",
        )
        .unwrap();

        std::fs::write(
            dir.join("dhcp"),
            "\
config dhcp 'lan'
\toption interface 'lan'
\toption start '100'
\toption limit '150'
\toption leasetime '12h'
\toption ra 'server'
\toption dhcpv6 'server'
",
        )
        .unwrap();
    }

    /// Write a minimal startwrt config (needed by ipv6_set's profile sync).
    fn setup_startwrt(dir: &std::path::Path) {
        std::fs::write(
            dir.join("startwrt"),
            "\
config profile lan
\toption fullname 'Admin'
\toption interface 'lan'
\toption vlan_tag '99'
",
        )
        .unwrap();
    }

    // ── IPv4 tests ──────────────────────────────────────────────

    #[test]
    fn ipv4_get_returns_address() {
        let dir = tempfile::tempdir().unwrap();
        setup_fixtures(dir.path());
        let ctx = TestContext(dir.path().to_path_buf());

        let res = ipv4_get(ctx).unwrap();
        assert_eq!(res.address, "192.168.1.1");
        assert_eq!(res.netmask, "255.255.0.0");
    }

    #[test]
    fn ipv4_get_defaults_when_missing() {
        let dir = tempfile::tempdir().unwrap();
        std::fs::write(dir.path().join("network"), "").unwrap();
        let ctx = TestContext(dir.path().to_path_buf());

        let res = ipv4_get(ctx).unwrap();
        assert_eq!(res.address, "192.168.1.1");
        assert_eq!(res.netmask, "255.255.255.0");
    }

    #[test]
    fn ipv4_set_updates_address() {
        let dir = tempfile::tempdir().unwrap();
        setup_fixtures(dir.path());
        setup_startwrt(dir.path());
        let ctx = TestContext(dir.path().to_path_buf());

        ipv4_set(
            ctx.clone(),
            DeserializeStdin(LanIpv4SetRequest {
                address: "10.0.0.1".to_string(),
            }),
        )
        .unwrap();

        let res = ipv4_get(ctx).unwrap();
        assert_eq!(res.address, "10.0.0.1");
        assert_eq!(res.netmask, "255.255.255.0");
    }

    #[test]
    fn ipv4_set_errors_when_lan_missing() {
        let dir = tempfile::tempdir().unwrap();
        std::fs::write(dir.path().join("network"), "").unwrap();
        setup_startwrt(dir.path());
        let ctx = TestContext(dir.path().to_path_buf());

        let err = ipv4_set(
            ctx,
            DeserializeStdin(LanIpv4SetRequest {
                address: "10.0.0.1".to_string(),
            }),
        )
        .unwrap_err();

        assert!(
            err.to_string().contains("LAN interface not found"),
            "unexpected error: {err}"
        );
    }

    #[test]
    fn ipv4_set_propagates_block_change_to_profiles() {
        let dir = tempfile::tempdir().unwrap();
        // Network config with LAN + a Guest profile interface + routing rule
        std::fs::write(
            dir.path().join("network"),
            "\
config interface 'lan'
\toption device 'br-lan'
\toption proto 'static'
\toption ipaddr '192.168.1.1'
\toption netmask '255.255.0.0'

config interface 'guest'
\toption device 'br-lan.3'
\toption proto 'static'
\toption ipaddr '192.168.2.1'
\toption netmask '255.255.255.0'

config rule 'prr_guest'
\toption src '192.168.2.0/24'
\toption lookup '200'
",
        )
        .unwrap();

        // startwrt config with LAN + Guest profiles
        std::fs::write(
            dir.path().join("startwrt"),
            "\
config profile lan
\toption fullname 'Admin'
\toption interface 'lan'
\toption vlan_tag '99'

config profile guest
\toption fullname 'Guest'
\toption interface 'guest'
\toption vlan_tag '3'
",
        )
        .unwrap();

        let ctx = TestContext(dir.path().to_path_buf());

        // Change from 192.168.x.x to 10.0.x.x
        ipv4_set(
            ctx.clone(),
            DeserializeStdin(LanIpv4SetRequest {
                address: "10.0.1.1".to_string(),
            }),
        )
        .unwrap();

        // Verify LAN IP updated
        let res = ipv4_get(ctx.clone()).unwrap();
        assert_eq!(res.address, "10.0.1.1");

        // Re-read and verify Guest interface and routing rule updated
        let arena = Arena::new();
        let cfgs = parse_all(ctx.uci_root(), &arena, &["network"]).unwrap();

        for section in &cfgs["network"].sections {
            if section.name().as_deref() == Some("guest") {
                let iface = section.get_typed::<NetworkInterface>().unwrap().unwrap();
                assert_eq!(
                    iface.ipaddr.unwrap(),
                    Ipv4Addr::new(10, 0, 2, 1),
                    "Guest interface IP should be updated to new block"
                );
            }
            if section.name().as_deref() == Some("prr_guest") {
                let rule = section.get_typed::<NetworkRule>().unwrap().unwrap();
                assert_eq!(
                    rule.src, "10.0.2.0/24",
                    "Guest routing rule src should be updated to new block"
                );
            }
        }
    }

    #[test]
    fn ipv4_set_same_block_does_not_touch_profiles() {
        let dir = tempfile::tempdir().unwrap();
        std::fs::write(
            dir.path().join("network"),
            "\
config interface 'lan'
\toption device 'br-lan'
\toption proto 'static'
\toption ipaddr '192.168.1.1'
\toption netmask '255.255.0.0'

config interface 'guest'
\toption device 'br-lan.3'
\toption proto 'static'
\toption ipaddr '192.168.2.1'
\toption netmask '255.255.255.0'
",
        )
        .unwrap();

        std::fs::write(
            dir.path().join("startwrt"),
            "\
config profile lan
\toption fullname 'Admin'
\toption interface 'lan'
\toption vlan_tag '99'

config profile guest
\toption fullname 'Guest'
\toption interface 'guest'
\toption vlan_tag '3'
",
        )
        .unwrap();

        let ctx = TestContext(dir.path().to_path_buf());

        // Change within same /16 block
        ipv4_set(
            ctx.clone(),
            DeserializeStdin(LanIpv4SetRequest {
                address: "192.168.5.1".to_string(),
            }),
        )
        .unwrap();

        // Guest should be untouched
        let arena = Arena::new();
        let cfgs = parse_all(ctx.uci_root(), &arena, &["network"]).unwrap();

        for section in &cfgs["network"].sections {
            if section.name().as_deref() == Some("guest") {
                let iface = section.get_typed::<NetworkInterface>().unwrap().unwrap();
                assert_eq!(
                    iface.ipaddr.unwrap(),
                    Ipv4Addr::new(192, 168, 2, 1),
                    "Guest interface IP should remain unchanged for same-block change"
                );
            }
        }
    }

    // ── IPv6 tests ──────────────────────────────────────────────

    #[test]
    fn ipv6_get_returns_settings() {
        let dir = tempfile::tempdir().unwrap();
        setup_fixtures(dir.path());
        let ctx = TestContext(dir.path().to_path_buf());

        let res = ipv6_get(ctx).unwrap();
        assert!(res.slaac);
        assert!(res.dhcpv6);
        assert_eq!(res.prefix, 60);
    }

    #[test]
    fn ipv6_get_defaults() {
        let dir = tempfile::tempdir().unwrap();
        std::fs::write(dir.path().join("network"), "").unwrap();
        std::fs::write(dir.path().join("dhcp"), "").unwrap();
        let ctx = TestContext(dir.path().to_path_buf());

        let res = ipv6_get(ctx).unwrap();
        assert!(!res.slaac);
        assert!(!res.dhcpv6);
        assert_eq!(res.prefix, 64);
    }

    #[test]
    fn ipv6_set_enables_slaac() {
        let dir = tempfile::tempdir().unwrap();
        setup_fixtures(dir.path());
        setup_startwrt(dir.path());
        let ctx = TestContext(dir.path().to_path_buf());

        ipv6_set(
            ctx.clone(),
            DeserializeStdin(LanIpv6SetRequest {
                slaac: true,
                dhcpv6: false,
                prefix: 60,
            }),
        )
        .unwrap();

        let res = ipv6_get(ctx).unwrap();
        assert!(res.slaac);
        assert!(!res.dhcpv6);
        assert_eq!(res.prefix, 60);
    }

    #[test]
    fn ipv6_set_disables_all() {
        let dir = tempfile::tempdir().unwrap();
        setup_fixtures(dir.path());
        setup_startwrt(dir.path());
        let ctx = TestContext(dir.path().to_path_buf());

        ipv6_set(
            ctx.clone(),
            DeserializeStdin(LanIpv6SetRequest {
                slaac: false,
                dhcpv6: false,
                prefix: 64,
            }),
        )
        .unwrap();

        let res = ipv6_get(ctx).unwrap();
        assert!(!res.slaac);
        assert!(!res.dhcpv6);
        assert_eq!(res.prefix, 64);
    }

    #[test]
    fn ipv6_set_errors_when_lan_missing() {
        let dir = tempfile::tempdir().unwrap();
        std::fs::write(dir.path().join("network"), "").unwrap();
        std::fs::write(dir.path().join("dhcp"), "").unwrap();
        setup_startwrt(dir.path());
        let ctx = TestContext(dir.path().to_path_buf());

        let err = ipv6_set(
            ctx,
            DeserializeStdin(LanIpv6SetRequest {
                slaac: true,
                dhcpv6: true,
                prefix: 60,
            }),
        )
        .unwrap_err();

        assert!(
            err.to_string().contains("LAN interface not found")
                || err.to_string().contains("LAN section not found"),
            "unexpected error: {err}"
        );
    }
}
