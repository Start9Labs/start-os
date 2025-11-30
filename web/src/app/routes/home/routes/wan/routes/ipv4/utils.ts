export const LABELS: Record<string, string> = {
  // ipv4
  dhcp: 'DHCP',
  static: 'Static',
  pppoe: 'PPPoE',
  wan: 'WAN IP Address',
  prefix: 'Subnet Prefix',
  mask: 'Subnet Mask',
  gateway: 'Gateway IP Address',
  password: 'Password*',
  vlan: 'VLAN ID',
  // dns
  isp: 'Get from ISP',
  tls: 'DNS over TLS',
  custom: 'Custom',
}

export function calculatePrefix(netmask: string | undefined): string {
  if (!netmask) return ''

  // Convert netmask to CIDR prefix
  const parts = netmask.split('.').map(Number)
  const binary = parts.map(p => p.toString(2).padStart(8, '0')).join('')
  const prefix = binary.split('1').length - 1

  return `/${prefix}`
}

export function mapResolverUrlToFriendlyName(url: string): string {
  // Map common resolver URLs to friendly names
  if (url.includes('cloudflare-dns.com') || url.includes('1.1.1.1')) {
    return 'Cloudflare (1.1.1.1)'
  }
  if (url.includes('dns.google')) {
    return 'Google (8.8.8.8)'
  }
  if (url.includes('quad9.net')) {
    return 'Quad9 (9.9.9.9)'
  }
  // Add more mappings as needed
  return url
}

export function mapFriendlyNameToResolverUrl(friendlyName: string): string {
  if (friendlyName.includes('Cloudflare')) {
    return 'https://cloudflare-dns.com/dns-query'
  }
  if (friendlyName.includes('Google')) {
    return 'https://dns.google/dns-query'
  }
  if (friendlyName.includes('Quad9')) {
    return 'https://dns.quad9.net/dns-query'
  }
  return friendlyName
}

export function getBootstrapDnsForResolver(friendlyName: string): string {
  if (friendlyName.includes('Cloudflare')) {
    return '1.1.1.1'
  }
  if (friendlyName.includes('Google')) {
    return '8.8.8.8'
  }
  if (friendlyName.includes('Quad9')) {
    return '9.9.9.9'
  }
  return '1.1.1.1'
}
