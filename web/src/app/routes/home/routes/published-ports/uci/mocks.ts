import {
  FirewallRedirectSection,
  FirewallRuleSection,
} from 'src/app/services/api/types'

/**
 * Mock published ports using standard OpenWRT firewall sections.
 *
 * IPv4 uses 'redirect' sections with DNAT target.
 * IPv6 uses 'rule' sections with ACCEPT target and family='ipv6'.
 *
 * Our custom options (_pp_id, _pp_mac) link related entries and
 * store device information.
 */

// IPv4 port forwards (redirects)
export const mockPublishedPortRedirects: FirewallRedirectSection[] = [
  {
    type: 'redirect',
    name: 'pp_home_assistant',
    options: {
      name: 'Home Assistant',
      src: 'wan',
      dest: 'lan',
      target: 'DNAT',
      proto: 'tcp',
      src_dport: '8123',
      dest_ip: '192.168.1.100',
      dest_port: '8123',
      enabled: '1',
      _pp_id: 'home_assistant',
      _pp_mac: '00:1A:2B:3C:4D:5E',
    },
    lists: {},
  },
  {
    type: 'redirect',
    name: 'pp_minecraft',
    options: {
      name: 'Minecraft Server',
      src: 'wan',
      dest: 'lan',
      target: 'DNAT',
      proto: 'tcp udp',
      src_dport: '25565',
      dest_ip: '192.168.1.101',
      dest_port: '25565',
      enabled: '1',
      _pp_id: 'minecraft',
      _pp_mac: '00:1A:2B:3C:4D:5F',
    },
    lists: {},
  },
  {
    type: 'redirect',
    name: 'pp_ssh_access',
    options: {
      name: 'SSH Access',
      src: 'wan',
      dest: 'lan',
      target: 'DNAT',
      proto: 'tcp',
      src_dport: '2222',
      dest_ip: '192.168.1.103',
      dest_port: '22',
      src_ip: '203.0.113.0/24', // Source restriction
      enabled: '0',
      _pp_id: 'ssh_access',
      _pp_mac: 'DE:AD:BE:EF:CA:FF',
    },
    lists: {},
  },
]

// IPv6 port forwards (rules with family='ipv6')
export const mockPublishedPortRules: FirewallRuleSection[] = [
  {
    type: 'rule',
    name: 'pp_home_assistant_v6',
    options: {
      name: 'Home Assistant',
      src: 'wan',
      dest: 'lan',
      target: 'ACCEPT',
      proto: 'tcp',
      dest_ip: '2001:db8:abcd:1::100',
      dest_port: '8123',
      family: 'ipv6',
      enabled: '1',
      _pp_id: 'home_assistant',
      _pp_mac: '00:1A:2B:3C:4D:5E',
    },
    lists: {},
  },
  // Minecraft has no IPv6 rule (ipv4 only)
  {
    type: 'rule',
    name: 'pp_ssh_access_v6',
    options: {
      name: 'SSH Access',
      src: 'wan',
      dest: 'lan',
      target: 'ACCEPT',
      proto: 'tcp',
      dest_ip: '2001:db8:abcd:1::103',
      dest_port: '22',
      src_ip: '203.0.113.0/24', // Source restriction
      family: 'ipv6',
      enabled: '0',
      _pp_id: 'ssh_access',
      _pp_mac: 'DE:AD:BE:EF:CA:FF',
    },
    lists: {},
  },
]

// Combined export for easy use in mock-api
export const mockPublishedPorts = [
  ...mockPublishedPortRedirects,
  ...mockPublishedPortRules,
]
