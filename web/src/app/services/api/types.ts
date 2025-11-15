export type UciSection =
  | NetworkInterfaceSection
  | DnsmasqSection
  | HttpsDnsProxySection

export type NetworkInterfaceSection = {
  type: 'interface'
  name: string | null
  options: {
    proto: 'dhcp' | 'static' | 'pppoe'
    ipaddr?: string
    netmask?: string
    gateway?: string
    username?: string
    password?: string
    device?: string
    ifname?: string
  }
  lists: {}
}

export type DnsmasqSection = {
  type: 'dnsmasq'
  name: string | null
  options: {}
  lists: {
    server?: string[]
  }
}

export type HttpsDnsProxySection = {
  type: 'https-dns-proxy'
  name: string | null
  options: {
    bootstrap_dns?: string
    resolver_url?: string
    listen_addr?: string
    listen_port?: string
  }
  lists: {}
}

export type UciFile<T extends UciSection> = {
  sections: T[]
  modified: string
}
