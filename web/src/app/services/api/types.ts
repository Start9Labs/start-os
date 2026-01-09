export type UciSection = NetworkInterfaceSection

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
    // IPv6 static options
    ip6addr?: string
    ip6gw?: string
    ip6prefix?: string
    ip6ifaceid?: string
    // 6RD options
    peeraddr?: string
    ip6prefixlen?: string
  }
  lists: {
    dns?: string[] // Custom DNS servers for this interface
  }
}

export type UciFile<T extends UciSection> = {
  sections: T[]
  modified: string
}
