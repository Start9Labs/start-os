import { T } from '@start9labs/start-sdk'

export type PortForwardEntry = {
  target: string
  label: string
  enabled: boolean
}

export type TunnelData = {
  wg: WgServer
  portForwards: Record<string, PortForwardEntry>
  gateways: Record<string, T.NetworkInterfaceInfo>
}

export type WgServer = {
  subnets: Record<string, WgSubnet>
}

export type WgSubnet = {
  name: string
  clients: Record<string, WgClient>
}

export type WgClient = {
  name: string
}

export const mockTunnelData: TunnelData = {
  wg: {
    subnets: {
      '10.59.0.0/24': {
        name: 'Family',
        clients: {
          '10.59.0.2': {
            name: 'Start9 Server',
          },
          '10.59.0.3': {
            name: 'Phone',
          },
          '10.59.0.4': {
            name: 'Laptop',
          },
        },
      },
    },
  },
  portForwards: {
    '69.1.1.42:443': { target: '10.59.0.2:443', label: 'HTTPS', enabled: true },
    '69.1.1.42:3000': {
      target: '10.59.0.2:3000',
      label: 'Grafana',
      enabled: true,
    },
  },
  gateways: {
    eth0: {
      name: null,
      secure: null,
      type: null,
      ipInfo: {
        name: 'Wired Connection 1',
        scopeId: 1,
        deviceType: 'ethernet',
        subnets: ['69.1.1.42/24'],
        wanIp: null,
        ntpServers: [],
        lanIp: ['10.59.0.1'],
        dnsServers: [],
      },
    },
  },
}
