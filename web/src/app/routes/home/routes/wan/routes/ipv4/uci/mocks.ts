import { UciSection } from 'src/app/services/api/types'

// Mock 1: DHCP configuration
export const wanIpv4Dhcp: UciSection = {
  type: 'interface',
  name: 'wan',
  options: {
    proto: 'dhcp',
    device: 'eth0.2',
    peerdns: '1', // Using ISP DNS
    ipaddr: '203.0.113.45',
    netmask: '255.255.255.0',
    gateway: '203.0.113.1',
  },
  lists: {
    dns: [], // Empty because using ISP DNS
  },
}

// Mock 2: Static IP configuration
export const wanIpv4Static: UciSection = {
  type: 'interface',
  name: 'wan',
  options: {
    proto: 'static',
    device: 'eth0.2',
    ipaddr: '203.0.113.10',
    netmask: '255.255.255.248',
    gateway: '203.0.113.9',
    peerdns: '0', // Using custom DNS
  },
  lists: {
    dns: ['1.1.1.1', '8.8.8.8'], // Custom DNS servers
  },
}

// Mock 3: PPPoE configuration
export const wanIpv4PPPoE: UciSection = {
  type: 'interface',
  name: 'wan',
  options: {
    proto: 'pppoe',
    device: 'eth1',
    username: 'user@isp.com',
    password: 'mySecretPassword123',
    peerdns: '0', // Using custom DNS
  },
  lists: {
    dns: ['84.200.69.80@853', '84.200.70.40@853'], // DNS over TLS
  },
}
