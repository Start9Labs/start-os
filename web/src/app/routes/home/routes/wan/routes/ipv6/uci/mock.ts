import { UciSection } from 'src/app/services/api/types'

// Mock 1: SLAAC (auto-configuration)
export const wanIpv6Slaac: UciSection = {
  type: 'interface',
  name: 'wan6',
  options: {
    proto: 'dhcpv6',
    reqaddress: 'try',
    reqprefix: 'auto',
    device: '@wan',
    // Dynamically assigned values
    ip6addr: '2001:db8:1234:5678::2/64',
    ip6gw: '2001:db8:1234:5678::1',
  },
  lists: {},
}

// Mock 2: DHCPv6 (stateful)
export const wanIpv6Dhcpv6: UciSection = {
  type: 'interface',
  name: 'wan6',
  options: {
    proto: 'dhcpv6',
    reqaddress: 'force',
    reqprefix: 'auto',
    device: '@wan',
    // Dynamically assigned values
    ip6addr: '2001:db8:abcd:ef01::100/128',
  },
  lists: {},
}

// Mock 3: Static IPv6
export const wanIpv6Static: UciSection = {
  type: 'interface',
  name: 'wan6',
  options: {
    proto: 'static',
    ip6addr: '2001:db8:cafe:babe::10/64',
    ip6gw: '2001:db8:cafe:babe::1',
    device: '@wan',
  },
  lists: {},
}

// Mock 4: 6RD (IPv6 Rapid Deployment tunnel)
export const wanIpv6_6rd: UciSection = {
  type: 'interface',
  name: 'wan6',
  options: {
    proto: '6rd',
    peeraddr: '203.0.113.1', // IPv4 gateway
    ip6prefix: '2001:db8::/32', // 6RD prefix from ISP
    ip6prefixlen: '32', // Prefix length
    device: '@wan',
  },
  lists: {},
}

// Mock 5: Disabled IPv6
export const wanIpv6Disabled: UciSection = {
  type: 'interface',
  name: 'wan6',
  options: {
    proto: 'none',
    device: '@wan',
  },
  lists: {},
}

// Mock 6: IPv6 not configured at all (no wan6 interface)
export const wanIpv6NotConfigured: UciSection = {
  type: 'interface',
  name: 'wan',
  options: {
    proto: 'dhcp',
    device: 'eth0.2',
  },
  lists: {},
}
