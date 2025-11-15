export type UciSectionType = 'interface' | 'dnsmasq' | 'https-dns-proxy'

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
    [key: string]: string | undefined
  }
  lists: Record<string, string[]>
}

export type DnsmasqSection = {
  type: 'dnsmasq'
  name: string | null
  options: Record<string, string>
  lists: {
    server?: string[]
    [key: string]: string[] | undefined
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
    [key: string]: string | undefined
  }
  lists: Record<string, string[]>
}

export type UciFile<T extends UciSection> = {
  sections: T[]
  modified: string
}
