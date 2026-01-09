import {
  NetworkDeviceSection,
  NetworkInterfaceSection,
} from 'src/app/services/api/types'

// Mock 1: Router default MAC (no custom macaddr set)
export const macRouter: NetworkInterfaceSection = {
  type: 'interface',
  name: 'wan',
  options: {
    proto: 'dhcp',
    device: 'eth0.2',
  },
  lists: {},
}

export const macRouterDevice: NetworkDeviceSection = {
  type: 'device',
  name: null,
  options: {
    name: 'eth0.2',
    macaddr: '94:83:C4:3B:D2:2B',
  },
  lists: {},
}

// Mock 2: Custom MAC address
export const macCustom: NetworkInterfaceSection = {
  type: 'interface',
  name: 'wan',
  options: {
    proto: 'dhcp',
    device: 'eth0.2',
    macaddr: 'AA:BB:CC:DD:EE:FF',
  },
  lists: {},
}

export const macCustomDevice: NetworkDeviceSection = {
  type: 'device',
  name: null,
  options: {
    name: 'eth0.2',
    macaddr: '94:83:C4:3B:D2:2B', // Original device MAC
  },
  lists: {},
}
