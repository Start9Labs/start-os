import { DOCUMENT } from '@angular/common'
import { Inject, Injectable } from '@angular/core'
import { WorkspaceConfig } from '@start9labs/shared'
import { T, utils } from '@start9labs/start-sdk'
import { PackageDataEntry } from './patch-db/data-model'

const {
  gitHash,
  useMocks,
  ui: { api, marketplace, mocks },
} = require('../../../../../config.json') as WorkspaceConfig

@Injectable({
  providedIn: 'root',
})
export class ConfigService {
  constructor(@Inject(DOCUMENT) private readonly document: Document) {}

  hostname = this.document.location.hostname
  // includes port
  host = this.document.location.host
  // includes ":" (e.g. "http:")
  protocol = this.document.location.protocol
  version = require('../../../../../package.json').version as string
  useMocks = useMocks
  mocks = mocks
  gitHash = gitHash
  api = api
  marketplace = marketplace
  skipStartupAlerts = useMocks && mocks.skipStartupAlerts
  supportsWebSockets = !!window.WebSocket

  isTor(): boolean {
    return useMocks ? mocks.maskAs === 'tor' : this.hostname.endsWith('.onion')
  }

  isLocal(): boolean {
    return useMocks
      ? mocks.maskAs === 'local'
      : this.hostname.endsWith('.local')
  }

  isLocalhost(): boolean {
    return useMocks
      ? mocks.maskAs === 'localhost'
      : this.hostname === 'localhost'
  }

  isIpv4(): boolean {
    return useMocks
      ? mocks.maskAs === 'ipv4'
      : new RegExp(utils.Patterns.ipv4.regex).test(this.hostname)
  }

  isIpv6(): boolean {
    return useMocks
      ? mocks.maskAs === 'ipv6'
      : new RegExp(utils.Patterns.ipv6.regex).test(this.hostname)
  }

  isLanHttp(): boolean {
    return !this.isTor() && !this.isLocalhost() && !this.isHttps()
  }

  isClearnet(): boolean {
    return useMocks
      ? mocks.maskAs === 'clearnet'
      : this.isHttps() &&
          !this.isTor() &&
          !this.isLocal() &&
          !this.isLocalhost() &&
          !this.isIpv4() &&
          !this.isIpv6()
  }

  isHttps(): boolean {
    return useMocks ? mocks.maskAsHttps : this.protocol === 'https:'
  }

  isSecure(): boolean {
    return window.isSecureContext || this.isTor()
  }

  isLaunchable(
    state: T.PackageState['state'],
    status: T.MainStatus['main'],
  ): boolean {
    return state === 'installed' && status === 'running'
  }

  /** ${scheme}://${username}@${host}:${externalPort}${suffix} */
  launchableAddress(
    interfaces: PackageDataEntry['serviceInterfaces'],
    hosts: PackageDataEntry['hosts'],
  ): string {
    const ui = Object.values(interfaces).find(
      i =>
        i.type === 'ui' &&
        (i.addressInfo.scheme === 'http' ||
          i.addressInfo.sslScheme === 'https'),
    )

    if (!ui) return ''

    const host = hosts[ui.addressInfo.hostId]

    if (!host) return ''

    const hostnameInfo = host.hostnameInfo[ui.addressInfo.internalPort]

    if (!hostnameInfo) return ''

    const addressInfo = ui.addressInfo
    const scheme = this.isHttps()
      ? ui.addressInfo.sslScheme === 'https'
        ? 'https'
        : 'http'
      : ui.addressInfo.scheme === 'http'
      ? 'http'
      : 'https'
    const username = addressInfo.username ? addressInfo.username + '@' : ''
    const suffix = addressInfo.suffix || ''
    const url = new URL(`${scheme}://${username}placeholder${suffix}`)

    const ipHostnames = hostnameInfo
      .filter(h => h.kind === 'ip')
      .map(h => h.hostname) as T.IpHostname[]
    const domainHostname = ipHostnames.find(h => h.kind === 'domain') as {
      kind: 'domain'
      domain: string
      subdomain: string | null
      port: number | null
      sslPort: number | null
    }
    const onionHostname = hostnameInfo.find(h => h.kind === 'onion')
      ?.hostname as T.OnionHostname | undefined

    if (this.isClearnet() && domainHostname) {
      url.hostname = domainHostname.domain
    } else if (this.isTor() && onionHostname) {
      url.hostname = onionHostname.value
    } else if (this.isIpv6()) {
      const ipv6Hostname = ipHostnames.find(h => h.kind === 'ipv6') as {
        kind: 'ipv6'
        value: string
        scopeId: number
        port: number | null
        sslPort: number | null
      }

      if (!ipv6Hostname) return ''

      url.hostname = ipv6Hostname.value
      url.port = String(ipv6Hostname.sslPort || ipv6Hostname.port)
    } else {
      // ipv4 or .local or localhost
      const localHostname = ipHostnames.find(h => h.kind === 'local')

      if (!localHostname) return ''

      url.hostname = this.hostname
      url.port = String(localHostname.sslPort || localHostname.port)
    }

    return url.href
  }

  getHost(): string {
    return this.host
  }
}

export function hasUi(
  interfaces: PackageDataEntry['serviceInterfaces'],
): boolean {
  return Object.values(interfaces).some(iface => iface.type === 'ui')
}
