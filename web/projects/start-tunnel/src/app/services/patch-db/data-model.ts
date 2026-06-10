import { T } from '@start9labs/start-sdk'

export type TunnelData = Pick<
  T.Tunnel.TunnelDatabase,
  'wg' | 'portForwards' | 'gateways' | 'dnsRecords'
>

export const mockTunnelData: TunnelData = {
  wg: {
    port: 51820,
    key: '',
    subnets: {
      '10.59.0.0/24': {
        name: 'Family',
        clients: {
          '10.59.0.2': {
            name: 'Start9 Server',
            key: '',
            psk: '',
            allowDnsInjection: true,
          },
          '10.59.0.3': {
            name: 'Phone',
            key: '',
            psk: '',
            allowDnsInjection: false,
          },
          '10.59.0.4': {
            name: 'Laptop',
            key: '',
            psk: '',
            allowDnsInjection: false,
          },
        },
        dns: { type: 'default' },
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
  dnsRecords: [
    {
      name: 'home.example.com',
      type: 'A',
      value: '10.59.0.2',
      ttl: 300,
      source: '10.59.0.2',
    },
  ],
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
