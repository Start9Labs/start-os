import { inject, Injectable } from '@angular/core'
import { T, utils } from '@start9labs/start-sdk'
import { ConfigService } from 'src/app/services/config.service'
import { GatewayPlus } from 'src/app/services/gateway.service'
import {
  PrimaryStatus,
  renderPkgStatus,
} from 'src/app/services/pkg-status-rendering.service'
import { toAuthorityName } from 'src/app/utils/acme'
import { getManifest } from 'src/app/utils/get-package-data'

function isPublicIp(h: T.HostnameInfo): boolean {
  return h.public && (h.metadata.kind === 'ipv4' || h.metadata.kind === 'ipv6')
}

export function isLanIp(h: T.HostnameInfo): boolean {
  return !h.public && (h.metadata.kind === 'ipv4' || h.metadata.kind === 'ipv6')
}

function isEnabled(addr: T.DerivedAddressInfo, h: T.HostnameInfo): boolean {
  if (isPublicIp(h)) {
    if (h.port === null) return true
    const sa =
      h.metadata.kind === 'ipv6'
        ? `[${h.hostname}]:${h.port}`
        : `${h.hostname}:${h.port}`
    return addr.enabled.includes(sa)
  } else {
    return !addr.disabled.some(
      ([hostname, port]) => hostname === h.hostname && port === (h.port ?? 0),
    )
  }
}

function getGatewayIds(h: T.HostnameInfo): string[] {
  switch (h.metadata.kind) {
    case 'ipv4':
    case 'ipv6':
    case 'public-domain':
      return [h.metadata.gateway]
    case 'mdns':
    case 'private-domain':
      return h.metadata.gateways
    case 'plugin':
      return []
  }
}

function getCertificate(
  h: T.HostnameInfo,
  host: T.Host,
  addSsl: T.AddSslOptions | null,
  secure: T.Security | null,
): string {
  if (!h.ssl) return '-'

  if (h.metadata.kind === 'public-domain') {
    const config = host.publicDomains[h.hostname]
    return config ? toAuthorityName(config.acme) : toAuthorityName(null)
  }

  if (addSsl) return toAuthorityName(null)
  if (secure?.ssl) return 'Self signed'

  return '-'
}

function sortDomainsFirst(a: GatewayAddress, b: GatewayAddress): number {
  const isDomain = (addr: GatewayAddress) =>
    addr.hostnameInfo.metadata.kind === 'public-domain' ||
    (addr.hostnameInfo.metadata.kind === 'private-domain' &&
      !addr.hostnameInfo.hostname.endsWith('.local'))
  return Number(isDomain(b)) - Number(isDomain(a))
}

function getAddressType(h: T.HostnameInfo): string {
  switch (h.metadata.kind) {
    case 'ipv4':
      return 'IPv4'
    case 'ipv6':
      return 'IPv6'
    case 'public-domain':
    case 'private-domain':
      return 'Domain'
    case 'mdns':
      return 'mDNS'
    case 'plugin':
      return 'Plugin'
  }
}

@Injectable({
  providedIn: 'root',
})
export class InterfaceService {
  private readonly config = inject(ConfigService)

  getGatewayGroups(
    serviceInterface: T.ServiceInterface,
    host: T.Host,
    gateways: GatewayPlus[],
  ): GatewayAddressGroup[] {
    const binding = host.bindings[serviceInterface.addressInfo.internalPort]
    if (!binding) return []

    const addr = binding.addresses
    const masked = serviceInterface.masked
    const ui = serviceInterface.type === 'ui'
    const { addSsl, secure } = binding.options

    const groupMap = new Map<string, GatewayAddress[]>()
    const gatewayMap = new Map<string, GatewayPlus>()

    for (const gateway of gateways) {
      groupMap.set(gateway.id, [])
      gatewayMap.set(gateway.id, gateway)
    }

    const available =
      this.config.accessType === 'localhost'
        ? addr.available
        : utils.filterNonLocal(addr.available)

    for (const h of available) {
      const gatewayIds = getGatewayIds(h)
      for (const gid of gatewayIds) {
        const list = groupMap.get(gid)
        if (!list) continue
        list.push({
          enabled: isEnabled(addr, h),
          type: getAddressType(h),
          access: h.public ? 'public' : 'private',
          url: utils.addressHostToUrl(serviceInterface.addressInfo, h),
          hostnameInfo: h,
          masked,
          ui,
          deletable:
            h.metadata.kind === 'private-domain' ||
            h.metadata.kind === 'public-domain',
          certificate: getCertificate(h, host, addSsl, secure),
        })
      }
    }

    return gateways
      .filter(g => (groupMap.get(g.id)?.length ?? 0) > 0)
      .map(g => {
        const addresses = groupMap.get(g.id)!.sort(sortDomainsFirst)

        // Derive mDNS enabled state from LAN IPs on this gateway
        const lanIps = addresses.filter(a => isLanIp(a.hostnameInfo))
        for (const a of addresses) {
          if (a.hostnameInfo.metadata.kind === 'mdns') {
            a.enabled = lanIps.some(ip => ip.enabled)
          }
        }

        return {
          gatewayId: g.id,
          gatewayName: g.name,
          addresses,
        }
      })
  }

  getPluginGroups(
    serviceInterface: T.ServiceInterface,
    host: T.Host,
    allPackageData?: Record<string, T.PackageDataEntry>,
  ): PluginAddressGroup[] {
    const binding = host.bindings[serviceInterface.addressInfo.internalPort]
    if (!binding) return []

    const addr = binding.addresses
    const masked = serviceInterface.masked
    const groupMap = new Map<string, PluginAddress[]>()

    for (const h of addr.available) {
      if (h.metadata.kind !== 'plugin') continue

      const url = utils.addressHostToUrl(serviceInterface.addressInfo, h)
      const pluginId = h.metadata.packageId

      if (!groupMap.has(pluginId)) {
        groupMap.set(pluginId, [])
      }

      groupMap.get(pluginId)!.push({
        url,
        hostnameInfo: h,
        masked,
      })
    }

    // Also include URL plugins that have no addresses yet
    if (allPackageData) {
      for (const [pkgId, pkg] of Object.entries(allPackageData)) {
        if (pkg.plugin?.url && !groupMap.has(pkgId)) {
          groupMap.set(pkgId, [])
        }
      }
    }

    return Array.from(groupMap.entries()).map(([pluginId, addresses]) => {
      const pluginPkg = allPackageData?.[pluginId]
      const pluginActions = pluginPkg?.actions ?? {}
      const tableActionId = pluginPkg?.plugin?.url?.tableAction ?? null
      const tableActionMeta = tableActionId
        ? pluginActions[tableActionId]
        : undefined
      const tableAction =
        tableActionId && tableActionMeta
          ? { id: tableActionId, metadata: tableActionMeta }
          : null

      let pluginPkgInfo: PluginPkgInfo | null = null
      if (pluginPkg) {
        const manifest = getManifest(pluginPkg)
        pluginPkgInfo = {
          id: manifest.id,
          title: manifest.title,
          icon: pluginPkg.icon,
          status: renderPkgStatus(pluginPkg).primary,
        }
      }

      return {
        pluginId,
        pluginName:
          pluginPkgInfo?.title ??
          pluginId.charAt(0).toUpperCase() + pluginId.slice(1),
        addresses,
        tableAction,
        pluginPkgInfo,
        pluginActions,
      }
    })
  }

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
            predicate: h => h.hostname === this.config.hostname,
          })
          .format('urlstring')[0]
        onLan = true
        break
      case 'ipv6':
        matching = addresses.nonLocal
          .filter({
            kind: 'ipv6',
            predicate: h => h.hostname === this.config.hostname,
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
}

export type GatewayAddress = {
  enabled: boolean
  type: string
  access: 'public' | 'private'
  url: string
  hostnameInfo: T.HostnameInfo
  masked: boolean
  ui: boolean
  deletable: boolean
  certificate: string
}

export type GatewayAddressGroup = {
  gatewayId: string
  gatewayName: string
  addresses: GatewayAddress[]
}

export type PluginAddress = {
  url: string
  hostnameInfo: T.HostnameInfo
  masked: boolean
}

export type PluginPkgInfo = {
  id: string
  title: string
  icon: string
  status: PrimaryStatus
}

export type PluginAddressGroup = {
  pluginId: string
  pluginName: string
  addresses: PluginAddress[]
  tableAction: { id: string; metadata: T.ActionMetadata } | null
  pluginPkgInfo: PluginPkgInfo | null
  pluginActions: Record<string, T.ActionMetadata>
}

export type MappedServiceInterface = T.ServiceInterface & {
  gatewayGroups: GatewayAddressGroup[]
  pluginGroups: PluginAddressGroup[]
  addSsl: boolean
  sharedHostNames: string[]
}
