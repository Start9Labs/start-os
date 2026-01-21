import { FirewallRedirectSection } from 'src/app/services/api/types'

export const mockPortForwarding: FirewallRedirectSection[] = [
  {
    type: 'redirect',
    name: null,
    options: {
      name: 'Web Server',
      src: 'wan',
      dest: 'lan',
      target: 'DNAT',
      proto: 'tcp',
      src_dport: '80',
      dest_ip: '192.168.1.100',
      dest_port: '80',
      enabled: '1',
    },
    lists: {},
  },
  {
    type: 'redirect',
    name: null,
    options: {
      name: 'SSH Access',
      src: 'wan',
      dest: 'lan',
      target: 'DNAT',
      proto: 'tcp',
      src_dport: '2222',
      dest_ip: '192.168.1.101',
      dest_port: '22',
      enabled: '1',
    },
    lists: {},
  },
  {
    type: 'redirect',
    name: null,
    options: {
      name: 'Game Server',
      src: 'wan',
      dest: 'lan',
      target: 'DNAT',
      proto: 'tcp udp',
      src_dport: '27015-27020',
      dest_ip: '192.168.1.103',
      dest_port: '27015-27020',
      enabled: '0',
    },
    lists: {},
  },
]
