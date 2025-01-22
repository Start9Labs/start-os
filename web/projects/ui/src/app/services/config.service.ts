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
      : this.hostname === 'localhost' || this.hostname === '127.0.0.1'
  }

  isIpv4(): boolean {
    return useMocks
      ? mocks.maskAs === 'ipv4'
      : new RegExp(utils.Patterns.ipv4.regex).test(this.hostname)
  }

  isLanIpv4(): boolean {
    return useMocks
      ? mocks.maskAs === 'ipv4'
      : new RegExp(utils.Patterns.ipv4.regex).test(this.hostname) &&
          (this.hostname.startsWith('192.168.') ||
            this.hostname.startsWith('10.') ||
            (this.hostname.startsWith('172.') &&
              !![this.hostname.split('.').map(Number)[1]].filter(
                n => n >= 16 && n < 32,
              ).length))
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
          !this.isLanIpv4() &&
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
    hosts: T.Hosts,
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

    let hostnameInfo = host.hostnameInfo[ui.addressInfo.internalPort]
    hostnameInfo = hostnameInfo.filter(
      h =>
        this.isLocalhost() ||
        h.kind !== 'ip' ||
        h.hostname.kind !== 'ipv6' ||
        !h.hostname.value.startsWith('fe80::'),
    )
    if (this.isLocalhost()) {
      const local = hostnameInfo.find(
        h => h.kind === 'ip' && h.hostname.kind === 'local',
      )
      if (local) {
        hostnameInfo.unshift({
          kind: 'ip',
          networkInterfaceId: 'lo',
          public: false,
          hostname: {
            kind: 'local',
            port: local.hostname.port,
            sslPort: local.hostname.sslPort,
            value: 'localhost',
          },
        })
      }
    }

    if (!hostnameInfo) return ''

    const addressInfo = ui.addressInfo
    const username = addressInfo.username ? addressInfo.username + '@' : ''
    const suffix = addressInfo.suffix || ''
    const url = new URL(`https://${username}placeholder${suffix}`)
    const use = (hostname: {
      value: string
      port: number | null
      sslPort: number | null
    }) => {
      url.hostname = hostname.value
      const useSsl =
        hostname.port && hostname.sslPort ? this.isHttps() : !!hostname.sslPort
      url.protocol = useSsl
        ? `${addressInfo.sslScheme || 'https'}:`
        : `${addressInfo.scheme || 'http'}:`
      const port = useSsl ? hostname.sslPort : hostname.port
      const omitPort = useSsl
        ? ui.addressInfo.sslScheme === 'https' && port === 443
        : ui.addressInfo.scheme === 'http' && port === 80
      if (!omitPort && port) url.port = String(port)
    }
    const useFirst = (
      hostnames: (
        | {
            value: string
            port: number | null
            sslPort: number | null
          }
        | undefined
      )[],
    ) => {
      const first = hostnames.find(h => h)
      if (first) {
        use(first)
      }
      return !!first
    }

    const ipHostnames = hostnameInfo
      .filter(h => h.kind === 'ip')
      .map(h => h.hostname) as T.IpHostname[]
    const domainHostname = ipHostnames
      .filter(h => h.kind === 'domain')
      .map(h => h as T.IpHostname & { kind: 'domain' })
      .map(h => ({
        value: h.domain,
        sslPort: h.sslPort,
        port: h.port,
      }))[0]
    const wanIpHostname = hostnameInfo
      .filter(h => h.kind === 'ip' && h.public && h.hostname.kind !== 'domain')
      .map(h => h.hostname as Exclude<T.IpHostname, { kind: 'domain' }>)
      .map(h => ({
        value: h.value,
        sslPort: h.sslPort,
        port: h.port,
      }))[0]
    const onionHostname = hostnameInfo
      .filter(h => h.kind === 'onion')
      .map(h => h as T.HostnameInfo & { kind: 'onion' })
      .map(h => ({
        value: h.hostname.value,
        sslPort: h.hostname.sslPort,
        port: h.hostname.port,
      }))[0]
    const localHostname = ipHostnames
      .filter(h => h.kind === 'local')
      .map(h => h as T.IpHostname & { kind: 'local' })
      .map(h => ({ value: h.value, sslPort: h.sslPort, port: h.port }))[0]

    if (this.isClearnet()) {
      if (
        !useFirst([domainHostname, wanIpHostname, onionHostname, localHostname])
      ) {
        return ''
      }
    } else if (this.isTor()) {
      if (
        !useFirst([onionHostname, domainHostname, wanIpHostname, localHostname])
      ) {
        return ''
      }
    } else if (this.isIpv6()) {
      const ipv6Hostname = ipHostnames.find(h => h.kind === 'ipv6') as {
        kind: 'ipv6'
        value: string
        scopeId: number
        port: number | null
        sslPort: number | null
      }

      if (!useFirst([ipv6Hostname, localHostname])) {
        return ''
      }
    } else {
      // ipv4 or .local or localhost

      if (!localHostname) return ''

      use({
        value: this.hostname,
        port: localHostname.port,
        sslPort: localHostname.sslPort,
      })
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
