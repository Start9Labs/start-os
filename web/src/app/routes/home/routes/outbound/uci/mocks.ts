import {
  WireGuardInterfaceSection,
  WireGuardPeerSection,
} from 'src/app/services/api/types'

// Mock 1: Proton VPN - targets Internet directly
export const protonInterface: WireGuardInterfaceSection = {
  type: 'interface',
  name: 'wg_proton',
  options: {
    proto: 'wireguard',
    private_key: 'GIaR3JZnLqEm8nR5Hp5bVrGVQfuVqVBdXqC5dLpmblM=',
    disabled: '0',
    label: 'Proton',
    target: 'Internet',
  },
  lists: {
    addresses: ['10.2.0.2/32'],
    dns: ['10.2.0.1'],
  },
}

export const protonPeer: WireGuardPeerSection = {
  type: 'wireguard_peer',
  name: 'proton_peer0',
  options: {
    public_key: 'bmXOC+F1FxEMF9dyiK2H5/1SUtzH0JuVo51h2wPqg18=',
    endpoint_host: 'us-ny-101.protonvpn.net',
    endpoint_port: '51820',
    persistent_keepalive: '25',
    route_allowed_ips: '1',
  },
  lists: {
    allowed_ips: ['0.0.0.0/0', '::/0'],
  },
}

// Mock 2: Mullvad VPN - chains through Proton
export const mullvadInterface: WireGuardInterfaceSection = {
  type: 'interface',
  name: 'wg_mullvad',
  options: {
    proto: 'wireguard',
    private_key: 'WGn3Mfx9bRgHqKrC8LmJQpXYZ1tN5aFvDsE7uP0oI2k=',
    disabled: '0',
    label: 'Mullvad',
    target: 'Proton',
  },
  lists: {
    addresses: ['10.66.212.42/32', 'fc00:bbbb:bbbb:bb01::3:d429/128'],
    dns: ['100.64.0.63'],
  },
}

export const mullvadPeer: WireGuardPeerSection = {
  type: 'wireguard_peer',
  name: 'mullvad_peer0',
  options: {
    public_key: 'Kn+kvhCZM7V7VQvFWyDuDfwF5zKRmKqXqBY8X+7yPFQ=',
    endpoint_host: 'se-sto-wg-001.relays.mullvad.net',
    endpoint_port: '51820',
    persistent_keepalive: '25',
    route_allowed_ips: '1',
  },
  lists: {
    allowed_ips: ['0.0.0.0/0', '::/0'],
  },
}

// Combined mock data for network file
export const mockWireGuardSections = [
  protonInterface,
  protonPeer,
  mullvadInterface,
  mullvadPeer,
]
