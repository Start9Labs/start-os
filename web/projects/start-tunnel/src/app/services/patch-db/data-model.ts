import { T } from '@start9labs/start-sdk'

export type TunnelData = Pick<
  T.Tunnel.TunnelDatabase,
  'wg' | 'portForwards' | 'gateways'
>

export const mockTunnelData: TunnelData = {
  wg: {
    port: 51820,
    key: '',
    subnets: {
      '10.59.0.0/24': {
        name: 'Family',
        clients: {
          '10.59.0.2': { name: 'Start9 Server', key: '', psk: '' },
          '10.59.0.3': { name: 'Phone', key: '', psk: '' },
          '10.59.0.4': { name: 'Laptop', key: '', psk: '' },
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
