use std::net::Ipv4Addr;

use uciedit_macros::UciSection;

#[derive(strum::EnumString, strum::Display, Default, PartialEq, Eq)]
pub enum FirewallTarget {
    #[default]
    ACCEPT,
    REJECT,
    DROP,
    MARK,
    NOTRACK,
}

#[derive(UciSection, Default)]
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

#[derive(strum::EnumString, strum::Display, Default, PartialEq, Eq)]
#[strum(serialize_all = "lowercase")]
pub enum InterfaceProto {
    #[default]
    NONE,
    STATIC,
    DHCP,
    DHCPV6,
}

#[derive(UciSection, Default)]
#[uci(ty = "interface")]
pub struct NetworkInterface {
    pub device: String,
    pub proto: InterfaceProto,
    pub ipaddr: Option<Ipv4Addr>,
    pub netmask: Option<Ipv4Addr>,
}
