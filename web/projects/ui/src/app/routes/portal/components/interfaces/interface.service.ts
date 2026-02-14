import { inject, Injectable } from '@angular/core'
import { T, utils } from '@start9labs/start-sdk'
import { ConfigService } from 'src/app/services/config.service'
import { GatewayPlus } from 'src/app/services/gateway.service'

function isPublicIp(h: T.HostnameInfo): boolean {
  return (
    h.public && (h.metadata.kind === 'ipv4' || h.metadata.kind === 'ipv6')
  )
}

function isEnabled(addr: T.DerivedAddressInfo, h: T.HostnameInfo): boolean {
  if (isPublicIp(h)) {
    if (h.port === null) return true
    const sa =
      h.metadata.kind === 'ipv6'
        ? `[${h.host}]:${h.port}`
        : `${h.host}:${h.port}`
    return addr.enabled.includes(sa)
  } else {
    return !addr.disabled.some(
      ([host, port]) => host === h.host && port === (h.port ?? 0),
    )
  }
}

function getGatewayIds(h: T.HostnameInfo): string[] {
  switch (h.metadata.kind) {
    case 'ipv4':
    case 'ipv6':
    case 'public-domain':
      return [h.metadata.gateway]
    case 'private-domain':
      return h.metadata.gateways
    case 'plugin':
      return []
  }
}

function getAddressType(h: T.HostnameInfo): string {
  switch (h.metadata.kind) {
    case 'ipv4':
      return 'IPv4'
    case 'ipv6':
      return 'IPv6'
    case 'public-domain':
      return 'Public Domain'
    case 'private-domain':
      return h.host.endsWith('.local') ? 'mDNS' : 'Private Domain'
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
    const binding =
      host.bindings[serviceInterface.addressInfo.internalPort]
    if (!binding) return []

    const addr = binding.addresses
    const masked = serviceInterface.masked
    const ui = serviceInterface.type === 'ui'

    const groupMap = new Map<string, GatewayAddress[]>()

    for (const gateway of gateways) {
      groupMap.set(gateway.id, [])
    }

    for (const h of addr.available) {
      const enabled = isEnabled(addr, h)
      const url = utils.addressHostToUrl(serviceInterface.addressInfo, h)
      const type = getAddressType(h)
      const isDomain =
        h.metadata.kind === 'private-domain' ||
        h.metadata.kind === 'public-domain'
      const isMdns =
        h.metadata.kind === 'private-domain' && h.host.endsWith('.local')

      const address: GatewayAddress = {
        enabled,
        type,
        access: h.public ? 'public' : 'private',
        url,
        hostnameInfo: h,
        masked,
        ui,
        deletable: isDomain && !isMdns,
      }

      const gatewayIds = getGatewayIds(h)
      for (const gid of gatewayIds) {
        const list = groupMap.get(gid)
        if (list) {
          list.push(address)
        }
      }
    }

    return gateways
      .filter(g => (groupMap.get(g.id)?.length ?? 0) > 0)
      .map(g => ({
        gatewayId: g.id,
        gatewayName: g.name,
        addresses: groupMap.get(g.id)!,
      }))
  }

  getPluginGroups(
    serviceInterface: T.ServiceInterface,
    host: T.Host,
  ): PluginAddressGroup[] {
    const binding =
      host.bindings[serviceInterface.addressInfo.internalPort]
    if (!binding) return []

    const addr = binding.addresses
    const masked = serviceInterface.masked
    const groupMap = new Map<string, PluginAddress[]>()

    for (const h of addr.available) {
      if (h.metadata.kind !== 'plugin') continue

      const url = utils.addressHostToUrl(serviceInterface.addressInfo, h)
      const pluginId = h.metadata.package

      if (!groupMap.has(pluginId)) {
        groupMap.set(pluginId, [])
      }

      groupMap.get(pluginId)!.push({
        url,
        hostnameInfo: h,
        masked,
      })
    }

    return Array.from(groupMap.entries()).map(([pluginId, addresses]) => ({
      pluginId,
      pluginName: pluginId.charAt(0).toUpperCase() + pluginId.slice(1),
      addresses,
    }))
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
            predicate: h => h.host === this.config.hostname,
          })
          .format('urlstring')[0]
        onLan = true
        break
      case 'ipv6':
        matching = addresses.nonLocal
          .filter({
            kind: 'ipv6',
            predicate: h => h.host === this.config.hostname,
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

export type PluginAddressGroup = {
  pluginId: string
  pluginName: string
  addresses: PluginAddress[]
}

export type MappedServiceInterface = T.ServiceInterface & {
  gatewayGroups: GatewayAddressGroup[]
  pluginGroups: PluginAddressGroup[]
  addSsl: boolean
}
