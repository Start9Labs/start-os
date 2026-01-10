export type UciSection =
  | NetworkInterfaceSection
  | NetworkDeviceSection
  | DdnsSection
  | DhcpSection

export type NetworkInterfaceSection = {
  type: 'interface'
  name: string | null
  options: {
    proto: 'dhcp' | 'static' | 'pppoe' | 'dhcpv6' | '6rd' | 'none'
    peerdns?: '0' | '1' // 0=use custom DNS, 1=use ISP DNS
    // DHCPv6/SLAAC options
    reqaddress?: 'try' | 'force'
    reqprefix?: 'auto' | string
    // IPv4 options
    ipaddr?: string
    netmask?: string
    gateway?: string
    // PPPoE options
    username?: string
    password?: string
    // Interface/device
    device?: string
    ifname?: string
    // MAC address
    macaddr?: string
    // IPv6 static options
    ip6addr?: string
    ip6gw?: string
    ip6prefix?: string
    ip6ifaceid?: string
    ip6assign?: string // Prefix delegation length
    // 6RD options
    peeraddr?: string
    ip6prefixlen?: string
  }
  lists: {
    dns?: string[] // Custom DNS servers for this interface
  }
}

export type NetworkDeviceSection = {
  type: 'device'
  name: string | null
  options: {
    name?: string
    type?: 'bridge' | string
    macaddr?: string
    mtu?: string
    ports?: string
  }
  lists: {
    ports?: string[]
  }
}

export type DdnsSection = {
  type: 'service'
  name: string | null
  options: {
    enabled?: '0' | '1'
    service_name?: string
    lookup_host?: string
    domain?: string
    username?: string
    password?: string
    ip_source?: 'network' | 'interface' | 'web'
    ip_network?: string
    interface?: string
  }
  lists: {}
}

export type DhcpSection = {
  type: 'dhcp'
  name: string | null
  options: {
    interface?: string
    start?: string
    limit?: string
    leasetime?: string
    // IPv6 options
    ra?: 'disabled' | 'server' | 'relay' | 'hybrid'
    dhcpv6?: 'disabled' | 'server' | 'relay' | 'hybrid'
    ra_management?: '0' | '1' | '2'
    ra_default?: '0' | '1' | '2'
  }
  lists: {}
}

export type UciFile<T extends UciSection> = {
  sections: T[]
  modified: string
}
