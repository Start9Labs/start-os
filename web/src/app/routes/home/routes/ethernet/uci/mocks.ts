import { NetworkDeviceSection } from 'src/app/services/api/types'

/**
 * Mock bridge device containing LAN ports
 * In OpenWRT, physical ports are grouped into a bridge for LAN
 */
export const mockBrLan: NetworkDeviceSection = {
  type: 'device',
  name: 'br_lan',
  options: {
    name: 'br-lan',
    type: 'bridge',
  },
  lists: {
    ports: ['eth1', 'eth2', 'eth3'],
  },
}

/**
 * Mock WAN device
 * The WAN interface references eth0.2 (VLAN 2 on eth0)
 * We track eth0 as the physical WAN port
 */
export const mockWanDevice: NetworkDeviceSection = {
  type: 'device',
  name: 'wan_eth0',
  options: {
    name: 'eth0',
  },
  lists: {},
}

/**
 * All physical ports available on the router
 * This would typically come from hardware detection
 */
export const mockPhysicalPorts = ['eth0', 'eth1', 'eth2', 'eth3']

/**
 * Stub for security profiles (not yet implemented)
 * Maps port name to profile name
 */
export const mockPortProfiles: Record<string, string> = {
  eth1: 'Admin',
  eth2: 'Admin',
  eth3: 'Guest',
}

/**
 * Available security profiles (stub)
 */
export const mockSecurityProfiles = ['Admin', 'Guest']
