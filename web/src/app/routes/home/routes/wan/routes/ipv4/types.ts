import {
  DnsmasqSection,
  HttpsDnsProxySection,
  NetworkInterfaceSection,
  UciFile,
} from 'src/app/services/api/types'

export type WanIpv4 = {
  network: UciFile<NetworkInterfaceSection>
  dhcp: UciFile<DnsmasqSection>
  'https-dns-proxy': UciFile<HttpsDnsProxySection>
}

export interface WanIpv4Form {
  ip: {
    mode: 'dhcp' | 'static' | 'pppoe'
    wan: string
    prefix: string
    mask: string
    gateway: string
    password: string
    vlan: string
  }
  dns: {
    mode: 'isp' | 'tls' | 'custom'
    server: string
    1: string
    2: string
    tls1: boolean
    tls2: boolean
    proxy: boolean
  }
}
