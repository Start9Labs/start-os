export type TunnelData = {
  wg: WgServer
  portForwards: Record<string, string>
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
      '10.59.0.1/24': {
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
    '69.1.1.42:443': '10.59.0.2:5443',
    '69.1.1.42:3000': '10.59.0.2:3000',
  },
}
