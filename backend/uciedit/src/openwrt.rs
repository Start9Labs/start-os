use std::{fmt, net::Ipv4Addr};

use inpt::Inpt;
use uciedit_macros::TypedSection;

#[derive(strum::EnumString, strum::Display, Default, PartialEq, Eq, Debug)]
pub enum FirewallTarget {
    #[default]
    ACCEPT,
    REJECT,
    DROP,
    MARK,
    NOTRACK,
}

#[derive(Debug, TypedSection, Default)]
#[uci(ty = "zone")]
pub struct FirewallZone {
    pub name: String,
    pub input: FirewallTarget,
    pub output: FirewallTarget,
    pub forward: FirewallTarget,
    pub network: Vec<String>,
}

#[derive(Debug, TypedSection, Default)]
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
    pub dest: Option<String>,
    pub dest_ip: Option<String>,
    pub dest_port: Option<String>,
    pub proto: Vec<String>,
    pub target: FirewallTarget,
    pub family: Option<String>,
}

#[derive(Debug, TypedSection, Default)]
#[uci(ty = "redirect")]
pub struct FirewallRedirect {
    pub name: String,
    pub src: String,
    pub dest: Option<String>,
    pub proto: Vec<String>,
    pub src_dport: Option<String>,
    pub dest_ip: Option<String>,
    pub dest_port: Option<String>,
    pub target: String,
}

#[derive(Debug, TypedSection)]
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

#[derive(Debug, TypedSection)]
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

#[derive(Debug, TypedSection)]
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

#[derive(Debug, TypedSection)]
#[uci(ty = "bridge-vlan")]
pub struct NetworkBridgeVlan {
    pub device: String,
    pub vlan: u16,
    #[uci(inpt)]
    pub ports: Vec<NetworkVlanPort>,
}

#[derive(
    Clone, Copy, strum::FromRepr, strum::EnumString, Debug, strum::Display, Default, PartialEq, Eq,
)]
pub enum WifiDynamicVlan {
    #[strum(serialize = "0")]
    #[default]
    OFF = 0,
    #[strum(serialize = "1")]
    ALLOWED = 1,
    #[strum(serialize = "2")]
    REQUIRED = 2,
}

#[derive(Clone, Copy, strum::EnumString, Debug, strum::Display, Default, PartialEq, Eq)]
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

#[derive(Clone, Copy, Inpt, Debug, Default)]
pub enum WifiChannel {
    #[default]
    #[inpt(regex = "auto")]
    Auto,
    Int(u32),
}

impl fmt::Display for WifiChannel {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            WifiChannel::Auto => write!(f, "auto"),
            WifiChannel::Int(i) => write!(f, "{}", i),
        }
    }
}

#[derive(Clone, Debug, TypedSection)]
#[uci(ty = "wifi-device")]
pub struct WifiDevice {
    #[uci(rename = "type")]
    pub device_type: String,
    pub path: Option<String>,
    #[uci(default_value = false)]
    pub disabled: bool,
    pub band: String,
    #[uci(inpt)]
    pub channel: WifiChannel,
}

#[derive(Clone, Debug, TypedSection)]
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
    pub maxassoc: Option<u32>,
}

#[derive(Debug, TypedSection)]
#[uci(ty = "wifi-vlan")]
pub struct WifiVlan {
    pub name: String,
    pub network: String,
    pub vid: u16,
    pub iface: Option<String>,
}

#[derive(Debug, TypedSection)]
#[uci(ty = "wifi-station")]
pub struct WifiStation {
    pub key: String,
    pub vid: Option<u16>,
    pub iface: Option<String>,
    #[uci(default)]
    pub label: Option<String>,
}

#[derive(Debug, TypedSection, Default)]
#[uci(ty = "route")]
pub struct NetworkRoute {
    pub interface: String,
    pub target: String,
    #[uci(default)]
    pub gateway: Option<String>,
    #[uci(default)]
    pub netmask: Option<String>,
    #[uci(default)]
    pub table: Option<u32>,
}

#[derive(Debug, TypedSection, Default)]
#[uci(ty = "rule")]
pub struct NetworkRule {
    pub src: String,
    pub lookup: u32,
}

#[derive(Debug, TypedSection)]
#[uci(ty = "dhcp")]
pub struct Dhcp {
    pub interface: String,
    pub start: u32,
    pub limit: u32,
    pub leasetime: String,
}
