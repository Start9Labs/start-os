import { inject, Injectable } from '@angular/core'
import { T, utils } from '@start9labs/start-sdk'
import { ConfigService } from 'src/app/services/config.service'
import { GatewayPlus } from 'src/app/services/gateway.service'
import { PublicDomain } from './public-domains/pd.service'
import { i18nKey, i18nPipe } from '@start9labs/shared'

type AddressWithInfo = {
  url: string
  info: T.HostnameInfo
  gateway?: GatewayPlus
  showSsl: boolean
  masked: boolean
  ui: boolean
}

function cmpWithRankedPredicates<T extends AddressWithInfo>(
  a: T,
  b: T,
  preds: ((x: T) => boolean)[],
): -1 | 0 | 1 {
  for (const pred of preds) {
    for (let [x, y, sign] of [[a, b, 1] as const, [b, a, -1] as const]) {
      if (pred(y) && !pred(x)) return sign
    }
  }
  return 0
}

type LanAddress = AddressWithInfo & { info: { kind: 'ip'; public: false } }
function filterLan(a: AddressWithInfo): a is LanAddress {
  return a.info.kind === 'ip' && !a.info.public
}
function cmpLan(host: T.Host, a: LanAddress, b: LanAddress): -1 | 0 | 1 {
  return cmpWithRankedPredicates(a, b, [
    x =>
      x.info.hostname.kind === 'domain' &&
      !!host.privateDomains.find(d => d === x.info.hostname.value), // private domain
    x => x.info.hostname.kind === 'local', // .local
    x => x.info.hostname.kind === 'ipv4', // ipv4
    x => x.info.hostname.kind === 'ipv6', // ipv6
    // remainder: public domains accessible privately
  ])
}

type VpnAddress = AddressWithInfo & {
  info: {
    kind: 'ip'
    public: false
    hostname: { kind: 'ipv4' | 'ipv6' | 'domain' }
  }
}
function filterVpn(a: AddressWithInfo): a is VpnAddress {
  return (
    a.info.kind === 'ip' && !a.info.public && a.info.hostname.kind !== 'local'
  )
}
function cmpVpn(host: T.Host, a: VpnAddress, b: VpnAddress): -1 | 0 | 1 {
  return cmpWithRankedPredicates(a, b, [
    x =>
      x.info.hostname.kind === 'domain' &&
      !!host.privateDomains.find(d => d === x.info.hostname.value), // private domain
    x => x.info.hostname.kind === 'ipv4', // ipv4
    x => x.info.hostname.kind === 'ipv6', // ipv6
    // remainder: public domains accessible privately
  ])
}

type ClearnetAddress = AddressWithInfo & {
  info: {
    kind: 'ip'
    public: true
    hostname: { kind: 'ipv4' | 'ipv6' | 'domain' }
  }
}
function filterClearnet(a: AddressWithInfo): a is ClearnetAddress {
  return a.info.kind === 'ip' && a.info.public
}
function cmpClearnet(
  host: T.Host,
  a: ClearnetAddress,
  b: ClearnetAddress,
): -1 | 0 | 1 {
  return cmpWithRankedPredicates(a, b, [
    x =>
      x.info.hostname.kind === 'domain' &&
      x.info.gateway.id === host.publicDomains[x.info.hostname.value]?.gateway, // public domain for this gateway
    x => x.gateway?.public ?? false, // public gateway
    x => x.info.hostname.kind === 'ipv4', // ipv4
    x => x.info.hostname.kind === 'ipv6', // ipv6
    // remainder: private domains / domains public on other gateways
  ])
}

export function getPublicDomains(
  publicDomains: Record<string, T.PublicDomainConfig>,
  gateways: GatewayPlus[],
): PublicDomain[] {
  return Object.entries(publicDomains).map(([fqdn, info]) => ({
    fqdn,
    acme: info.acme,
    gateway: gateways.find(g => g.id === info.gateway) || null,
  }))
}

@Injectable({
  providedIn: 'root',
})
export class InterfaceService {
  private readonly config = inject(ConfigService)
  private readonly i18n = inject(i18nPipe)

  getAddresses(
    serviceInterface: T.ServiceInterface,
    host: T.Host,
    gateways: GatewayPlus[],
  ): MappedServiceInterface['addresses'] {
    const hostnamesInfos = this.hostnameInfo(serviceInterface, host)

    const addresses = {
      common: [],
      uncommon: [],
    }

    if (!hostnamesInfos.length) return addresses

    const masked = serviceInterface.masked
    const ui = serviceInterface.type === 'ui'

    const allAddressesWithInfo: AddressWithInfo[] = hostnamesInfos.flatMap(
      h => {
        const { url, sslUrl } = utils.addressHostToUrl(
          serviceInterface.addressInfo,
          h,
        )
        const info = h
        const gateway =
          h.kind === 'ip'
            ? gateways.find(g => h.gateway.id === g.id)
            : undefined
        const res = []
        if (url) {
          res.push({
            url,
            info,
            gateway,
            showSsl: false,
            masked,
            ui,
          })
        }
        if (sslUrl) {
          res.push({
            url: sslUrl,
            info,
            gateway,
            showSsl: !!url,
            masked,
            ui,
          })
        }
        return res
      },
    )

    const lanAddrs = allAddressesWithInfo
      .filter(filterLan)
      .sort((a, b) => cmpLan(host, a, b))
    const vpnAddrs = allAddressesWithInfo
      .filter(filterVpn)
      .sort((a, b) => cmpVpn(host, a, b))
    const clearnetAddrs = allAddressesWithInfo
      .filter(filterClearnet)
      .sort((a, b) => cmpClearnet(host, a, b))

    let bestAddrs = [
      (clearnetAddrs[0]?.gateway?.public ||
        clearnetAddrs[0]?.info.hostname.kind === 'domain') &&
        clearnetAddrs[0],
      lanAddrs[0],
      vpnAddrs[0],
    ]
      .filter(a => !!a)
      .reduce((acc, x) => {
        if (!acc.includes(x)) acc.push(x)
        return acc
      }, [] as AddressWithInfo[])

    return {
      common: bestAddrs.map(a => this.toDisplayAddress(a, host.publicDomains)),
      uncommon: allAddressesWithInfo
        .filter(a => !bestAddrs.includes(a))
        .map(a => this.toDisplayAddress(a, host.publicDomains)),
    }
  }

  /** ${scheme}://${username}@${host}:${externalPort}${suffix} */
  launchableAddress(ui: T.ServiceInterface, host: T.Host): string {
    const addresses = utils.filledAddress(host, ui.addressInfo)

    if (!addresses.hostnames.length) return ''

    const publicDomains = addresses.filter({
      kind: 'domain',
      visibility: 'public',
    })
    const wanIp = addresses.filter({ kind: 'ipv4', visibility: 'public' })
    const bestPublic = [publicDomains, wanIp].flatMap(h =>
      h.format('urlstring'),
    )[0]
    const privateDomains = addresses.filter({
      kind: 'domain',
      visibility: 'private',
    })
    const mdns = addresses.filter({ kind: 'mdns' })
    const bestPrivate = [privateDomains, mdns].flatMap(h =>
      h.format('urlstring'),
    )[0]

    let matching
    let onLan = false
    switch (this.config.accessType) {
      case 'ipv4':
        matching = addresses.nonLocal
          .filter({
            kind: 'ipv4',
            predicate: h => h.hostname.value === this.config.hostname,
          })
          .format('urlstring')[0]
        onLan = true
        break
      case 'ipv6':
        matching = addresses.nonLocal
          .filter({
            kind: 'ipv6',
            predicate: h => h.hostname.value === this.config.hostname,
          })
          .format('urlstring')[0]
        break
      case 'localhost':
        matching = addresses
          .filter({ kind: 'localhost' })
          .format('urlstring')[0]
        onLan = true
        break
      case 'mdns':
        matching = mdns.format('urlstring')[0]
        onLan = true
        break
    }

    if (matching) return matching
    if (onLan && bestPrivate) return bestPrivate
    if (bestPublic) return bestPublic
    return ''
  }

  private hostnameInfo(
    serviceInterface: T.ServiceInterface,
    host: T.Host,
  ): T.HostnameInfo[] {
    let hostnameInfo =
      host.hostnameInfo[serviceInterface.addressInfo.internalPort]
    return (
      hostnameInfo?.filter(
        h =>
          this.config.accessType === 'localhost' ||
          !(
            h.kind === 'ip' &&
            ((h.hostname.kind === 'ipv6' &&
              utils.IPV6_LINK_LOCAL.contains(h.hostname.value)) ||
              h.gateway.id === 'lo')
          ),
      ) || []
    )
  }

  private toDisplayAddress(
    { info, url, gateway, showSsl, masked, ui }: AddressWithInfo,
    publicDomains: Record<string, T.PublicDomainConfig>,
  ): DisplayAddress {
    let access: DisplayAddress['access']
    let gatewayName: DisplayAddress['gatewayName']
    let type: DisplayAddress['type']
    let bullets: any[]

    const rootCaRequired = this.i18n.transform(
      "Requires trusting your server's Root CA",
    )

    {
      const port = info.hostname.sslPort || info.hostname.port
      gatewayName = info.gateway.name

      const gatewayLanIpv4 = gateway?.lanIpv4[0]
      const isWireguard = gateway?.ipInfo.deviceType === 'wireguard'

      const localIdeal = this.i18n.transform('Ideal for local access')
      const lanRequired = this.i18n.transform(
        'Requires being connected to the same Local Area Network (LAN) as your server, either physically or via VPN',
      )
      const staticRequired = `${this.i18n.transform('Requires setting a static IP address for')} ${gatewayLanIpv4} ${this.i18n.transform('in your gateway')}`
      const vpnAccess = this.i18n.transform('Ideal for VPN access via')
      const routerWireguard = this.i18n.transform(
        "your router's Wireguard server",
      )
      const portForwarding = this.i18n.transform(
        'Requires port forwarding in gateway',
      )
      const dnsFor = this.i18n.transform('Requires a DNS record for')
      const resolvesTo = this.i18n.transform('that resolves to')

      // * Local *
      if (info.hostname.kind === 'local') {
        type = this.i18n.transform('Local')
        access = 'private'
        bullets = [
          localIdeal,
          this.i18n.transform(
            'Not recommended for VPN access. VPNs do not support ".local" domains without advanced configuration',
          ),
          lanRequired,
          rootCaRequired,
        ]
        // * IPv4 *
      } else if (info.hostname.kind === 'ipv4') {
        type = 'IPv4'
        if (info.public) {
          access = 'public'
          bullets = [
            this.i18n.transform('Can be used for clearnet access'),
            this.i18n.transform(
              'Not recommended in most cases. Using a public domain is more common and preferred',
            ),
            rootCaRequired,
          ]
          if (!info.gateway.public) {
            bullets.push(
              `${portForwarding} "${gatewayName}": ${port} -> ${port}`,
            )
          }
        } else {
          access = 'private'
          if (isWireguard) {
            bullets = [`${vpnAccess} StartTunnel`, rootCaRequired]
          } else {
            bullets = [
              localIdeal,
              `${vpnAccess} ${routerWireguard}`,
              lanRequired,
              rootCaRequired,
              staticRequired,
            ]
          }
        }
        // * IPv6 *
      } else if (info.hostname.kind === 'ipv6') {
        type = 'IPv6'
        access = 'private'
        bullets = [
          this.i18n.transform('Can be used for local access'),
          lanRequired,
          rootCaRequired,
        ]
        // * Domain *
      } else {
        type = this.i18n.transform('Domain')
        if (info.public) {
          access = 'public'
          bullets = [
            `${dnsFor} ${info.hostname.value} ${resolvesTo} ${gateway?.ipInfo.wanIp}`,
          ]

          if (!info.gateway.public) {
            bullets.push(
              `${portForwarding} "${gatewayName}": ${port} -> ${port === 443 ? 5443 : port}`,
            )
          }

          if (publicDomains[info.hostname.value]?.acme) {
            bullets.unshift(
              this.i18n.transform('Ideal for public access via the Internet'),
            )
          } else {
            bullets = [
              this.i18n.transform(
                'Can be used for personal access via the public Internet, but a VPN is more private and secure',
              ),
              this.i18n.transform(
                `Not good for public access, since the certificate is signed by your Server's Root CA`,
              ),
              rootCaRequired,
              ...bullets,
            ]
          }
        } else {
          access = 'private'
          const ipPortBad = this.i18n.transform(
            'when using IP addresses and ports is undesirable',
          )
          const customDnsRequired = `${dnsFor} ${info.hostname.value} ${resolvesTo} ${gatewayLanIpv4}`
          if (isWireguard) {
            bullets = [
              `${vpnAccess} StartTunnel ${ipPortBad}`,
              customDnsRequired,
              rootCaRequired,
            ]
          } else {
            bullets = [
              `${localIdeal} ${ipPortBad}`,
              `${vpnAccess} ${routerWireguard} ${ipPortBad}`,
              customDnsRequired,
              rootCaRequired,
              lanRequired,
              staticRequired,
            ]
          }
        }
      }
    }

    if (showSsl) {
      type = `${type} (SSL)`

      bullets.unshift(
        this.i18n.transform('Should only needed for apps that enforce SSL'),
      )
    }

    return {
      url,
      access,
      gatewayName,
      type,
      bullets,
      masked,
      ui,
    }
  }
}

export type MappedServiceInterface = T.ServiceInterface & {
  gateways: InterfaceGateway[]
  publicDomains: PublicDomain[]
  privateDomains: string[]
  addresses: {
    common: DisplayAddress[]
    uncommon: DisplayAddress[]
  }
  addSsl: boolean
}

export type InterfaceGateway = GatewayPlus & {
  enabled: boolean
}

export type DisplayAddress = {
  type: string
  access: 'public' | 'private' | null
  gatewayName: string | null
  url: string
  bullets: i18nKey[]
  masked: boolean
  ui: boolean
}
