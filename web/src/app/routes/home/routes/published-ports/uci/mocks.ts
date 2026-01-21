import { FirewallRedirectSection } from 'src/app/services/api/types'

// Mock published ports encoded in firewall redirects
// Format for name: "label|mac|ipv4|ipv6|source"
export const mockPublishedPorts: FirewallRedirectSection[] = [
  {
    type: 'redirect',
    name: 'pp_1',
    options: {
      name: 'Home Assistant|00:1A:2B:3C:4D:5E|1|1|any',
      src: 'wan',
      dest: 'lan',
      target: 'DNAT',
      proto: 'tcp',
      src_dport: '8123',
      dest_ip: '192.168.1.100',
      dest_port: '8123',
      enabled: '1',
    },
    lists: {},
  },
  {
    type: 'redirect',
    name: 'pp_2',
    options: {
      name: 'Minecraft Server|00:1A:2B:3C:4D:5F|1|0|any',
      src: 'wan',
      dest: 'lan',
      target: 'DNAT',
      proto: 'tcp udp',
      src_dport: '25565',
      dest_ip: '192.168.1.101',
      dest_port: '25565',
      enabled: '1',
    },
    lists: {},
  },
  {
    type: 'redirect',
    name: 'pp_3',
    options: {
      name: 'SSH Access|DE:AD:BE:EF:CA:FF|1|1|203.0.113.0/24',
      src: 'wan',
      dest: 'lan',
      target: 'DNAT',
      proto: 'tcp',
      src_dport: '2222',
      dest_ip: '192.168.1.103',
      dest_port: '22',
      enabled: '0',
    },
    lists: {},
  },
]
