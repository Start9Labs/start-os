import {
  DnsmasqSection,
  HttpsDnsProxySection,
  NetworkInterfaceSection,
  UciFile,
} from 'src/app/services/api/types'

export type IpMode = 'dhcp' | 'static' | 'pppoe'
export type DnsMode = 'isp' | 'tls' | 'custom'
export type WanIpv4 = {
  network: UciFile<NetworkInterfaceSection>
  dhcp: UciFile<DnsmasqSection>
  'https-dns-proxy': UciFile<HttpsDnsProxySection>
}

export interface Ip {
  wan: string
  prefix: string
  mask: string
  gateway: string
}

export interface Ipv4 {
  ip: {
    mode: IpMode
    dhcp: Ip
    static: Ip
    pppoe: {
      wan: string
      password: string
      vlan: string
    }
  }
  dns: {
    mode: DnsMode
    isp: { server: string }
    tls: { server: string }
    custom: {
      server: string
      1: string
      2: string
      tls1: boolean
      tls2: boolean
    }
    proxy: boolean
  }
}
