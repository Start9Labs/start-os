use std::{fmt, net::Ipv4Addr};

use inpt::Inpt;
use uciedit_macros::UciSection;

#[derive(strum::EnumString, strum::Display, Default, PartialEq, Eq, Debug)]
pub enum FirewallTarget {
    #[default]
    ACCEPT,
    REJECT,
    DROP,
    MARK,
    NOTRACK,
}

#[derive(Debug, UciSection, Default)]
#[uci(ty = "zone")]
pub struct FirewallZone {
    pub name: String,
    pub input: FirewallTarget,
    pub output: FirewallTarget,
    pub forward: FirewallTarget,
    pub network: Vec<String>,
}

#[derive(Debug, UciSection)]
#[uci(ty = "rule")]
pub struct FirewallRule {
    /*
    option	name		'Reject LAN to WAN for custom IP'
    option	src		'lan'
    option	src_ip		'192.168.1.2'
    option	src_mac		'00:11:22:33:44:55'
    option	src_port	'80'
    option	dest		'wan'
    option	dest_ip		'194.25.2.129'
    option	dest_port	'120'
    option	proto		'tcp'
    option	target		'REJECT'
    */
    pub name: String,
    pub src: String,
    pub src_ip: Option<String>,
    pub src_mac: Option<String>,
    pub src_port: Option<String>,
    pub dest: String,
    pub dest_ip: Option<String>,
    pub dest_port: Option<String>,
    pub proto: Option<String>,
    pub target: FirewallTarget,
}

#[derive(Debug, UciSection)]
#[uci(ty = "forwarding")]
pub struct FirewallForwarding {
    pub src: String,
    pub dest: String,
}

#[derive(strum::EnumString, strum::Display, Default, PartialEq, Eq, Debug)]
#[strum(serialize_all = "lowercase")]
pub enum InterfaceProto {
    #[default]
    NONE,
    STATIC,
    DHCP,
    DHCPV6,
}

#[derive(Debug, UciSection)]
#[uci(ty = "interface")]
pub struct NetworkInterface {
    pub device: String,
    #[uci(default)]
    pub proto: InterfaceProto,
    pub ipaddr: Option<Ipv4Addr>,
    pub netmask: Option<Ipv4Addr>,
}

#[derive(strum::EnumString, strum::Display, PartialEq, Eq, Debug)]
#[strum(serialize_all = "lowercase")]
pub enum DeviceType {
    BRIDGE,
}

#[derive(Debug, UciSection)]
#[uci(ty = "device")]
pub struct NetworkDevice {
    pub name: String,
    #[uci(rename = "type")]
    pub ty: Option<DeviceType>,
    pub ports: Vec<String>,
}

#[derive(Inpt, Debug)]
#[inpt(regex = "([^:]+):?([ut*]+)?")]
pub struct NetworkVlanPort {
    pub port: String,
    #[inpt(from_str)]
    pub tagging: Option<NetworkVlanPortTagging>,
}

impl fmt::Display for NetworkVlanPort {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match &self.tagging {
            Some(tagging) => write!(f, "{}:{}", self.port, tagging),
            None => write!(f, "{}", self.port),
        }
    }
}

#[derive(strum::EnumString, strum::Display, PartialEq, Eq, Debug)]
#[strum(serialize_all = "lowercase")]
pub enum NetworkVlanPortTagging {
    #[strum(serialize = "u")]
    UNTAGGED,
    #[strum(serialize = "u*")]
    PRIMARY,
    #[strum(serialize = "t")]
    TAGGED,
}

#[derive(Debug, UciSection)]
#[uci(ty = "bridge-vlan")]
pub struct NetworkBridgeVlan {
    pub device: String,
    pub vlan: u16,
    #[uci(inpt)]
    pub ports: Vec<NetworkVlanPort>,
}

#[derive(strum::FromRepr, strum::EnumString, Debug, strum::Display, Default, PartialEq, Eq)]
pub enum WifiDynamicVlan {
    #[strum(serialize = "0")]
    #[default]
    OFF = 0,
    #[strum(serialize = "1")]
    ALLOWED = 1,
    #[strum(serialize = "2")]
    REQUIRED = 2,
}

#[derive(strum::EnumString, Debug, strum::Display, Default, PartialEq, Eq)]
pub enum WifiMode {
    #[default]
    #[strum(serialize = "ap")]
    AP,
    #[strum(serialize = "sta")]
    STA,
    #[strum(serialize = "adhoc")]
    ADHOC,
    #[strum(serialize = "monitor")]
    MONITOR,
    #[strum(serialize = "mesh")]
    MESH,
}

#[derive(Debug, UciSection)]
#[uci(ty = "wifi-device")]
pub struct WifiDevice {
    #[uci(default_value = false)]
    pub disabled: bool,
    pub band: String,
}

#[derive(Debug, UciSection)]
#[uci(ty = "wifi-iface")]
pub struct WifiInterface {
    pub device: String,
    #[uci(default)]
    pub mode: WifiMode,
    pub ssid: String,
    #[uci(default_value = false)]
    pub hidden: bool,
    #[uci(default_value = "none".to_string())]
    pub encryption: String,
    pub key: Option<String>,
    #[uci(default)]
    pub dynamic_vlan: WifiDynamicVlan,
}

#[derive(Debug, UciSection)]
#[uci(ty = "wifi-vlan")]
pub struct WifiVlan {
    pub name: String,
    pub network: String,
    pub vid: u16,
    pub iface: Option<String>,
}

#[derive(Debug, UciSection)]
#[uci(ty = "wifi-station")]
pub struct WifiStation {
    pub key: String,
    pub vid: Option<u16>,
    pub iface: Option<String>,
}
