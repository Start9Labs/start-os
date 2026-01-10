import { NetworkInterfaceSection } from 'src/app/services/api/types'

// Mock 1: Default 192.168.0.1 with IPv6
export const lanDefault: NetworkInterfaceSection = {
  type: 'interface',
  name: 'lan',
  options: {
    proto: 'static',
    ipaddr: '192.168.0.1',
    netmask: '255.255.255.0',
    ip6assign: '64',
    ip6addr: 'fd00::1/64',
  },
  lists: {},
}

// Mock 2: 192.168.1.1 (common alternative)
export const lan192_168_1: NetworkInterfaceSection = {
  type: 'interface',
  name: 'lan',
  options: {
    proto: 'static',
    ipaddr: '192.168.1.1',
    netmask: '255.255.255.0',
    ip6assign: '64',
  },
  lists: {},
}

// Mock 3: 10.0.0.1 (Class A private)
export const lan10_0_0: NetworkInterfaceSection = {
  type: 'interface',
  name: 'lan',
  options: {
    proto: 'static',
    ipaddr: '10.0.0.1',
    netmask: '255.255.255.0',
    ip6assign: '64',
  },
  lists: {},
}

// Mock 4: 172.16.0.1 (Class B private)
export const lan172_16_0: NetworkInterfaceSection = {
  type: 'interface',
  name: 'lan',
  options: {
    proto: 'static',
    ipaddr: '172.16.0.1',
    netmask: '255.255.255.0',
    ip6assign: '64',
  },
  lists: {},
}
