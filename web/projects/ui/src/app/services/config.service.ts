import { DOCUMENT } from '@angular/common'
import { Inject, Injectable } from '@angular/core'
import { WorkspaceConfig } from '@start9labs/shared'
import { T } from '@start9labs/start-sdk'
import { PackageDataEntry } from 'src/app/services/patch-db/data-model'

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

  isTor(): boolean {
    return useMocks ? mocks.maskAs === 'tor' : this.hostname.endsWith('.onion')
  }

  isLocal(): boolean {
    return (
      this.hostname.endsWith('.local') || (useMocks && mocks.maskAs === 'local')
    )
  }

  isLocalhost(): boolean {
    return (
      this.hostname === 'localhost' ||
      (useMocks && mocks.maskAs === 'localhost')
    )
  }

  isIpv4(): boolean {
    return isValidIpv4(this.hostname) || (useMocks && mocks.maskAs === 'ipv4')
  }

  isIpv6(): boolean {
    return isValidIpv6(this.hostname) || (useMocks && mocks.maskAs === 'ipv6')
  }

  isClearnet(): boolean {
    return (
      (useMocks && mocks.maskAs === 'clearnet') ||
      (!this.isTor() &&
        !this.isLocal() &&
        !this.isLocalhost() &&
        !this.isIpv4() &&
        !this.isIpv6())
    )
  }

  isLanHttp(): boolean {
    return !this.isTor() && !this.isLocalhost() && !this.isHttps()
  }

  isSecure(): boolean {
    return window.isSecureContext || this.isTor()
  }

  isLaunchable(
    state: T.PackageState['state'],
    status: T.MainStatus['status'],
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
    ) // TODO select if multiple

    if (!ui) return ''

    const hostnameInfo =
      hosts[ui.addressInfo.hostId]?.hostnameInfo[ui.addressInfo.internalPort]

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

    const onionHostname = hostnameInfo.find(h => h.kind === 'onion')
      ?.hostname as T.OnionHostname | undefined

    if (this.isTor() && onionHostname) {
      url.hostname = onionHostname.value
    } else {
      const ipHostname = hostnameInfo.find(h => h.kind === 'ip')?.hostname as
        | T.IpHostname
        | undefined

      if (!ipHostname) return ''

      url.hostname = this.hostname
      url.port = String(ipHostname.sslPort || ipHostname.port)
    }

    return url.href
  }

  getHost(): string {
    return this.host
  }

  private isHttps(): boolean {
    return useMocks ? mocks.maskAsHttps : this.protocol === 'https:'
  }
}

export function isValidIpv4(address: string): boolean {
  const regexExp =
    /^(25[0-5]|2[0-4][0-9]|[01]?[0-9][0-9]?)\.(25[0-5]|2[0-4][0-9]|[01]?[0-9][0-9]?)\.(25[0-5]|2[0-4][0-9]|[01]?[0-9][0-9]?)\.(25[0-5]|2[0-4][0-9]|[01]?[0-9][0-9]?)$/
  return regexExp.test(address)
}

export function isValidIpv6(address: string): boolean {
  const regexExp =
    /(([0-9a-fA-F]{1,4}:){7,7}[0-9a-fA-F]{1,4}|([0-9a-fA-F]{1,4}:){1,7}:|([0-9a-fA-F]{1,4}:){1,6}:[0-9a-fA-F]{1,4}|([0-9a-fA-F]{1,4}:){1,5}(:[0-9a-fA-F]{1,4}){1,2}|([0-9a-fA-F]{1,4}:){1,4}(:[0-9a-fA-F]{1,4}){1,3}|([0-9a-fA-F]{1,4}:){1,3}(:[0-9a-fA-F]{1,4}){1,4}|([0-9a-fA-F]{1,4}:){1,2}(:[0-9a-fA-F]{1,4}){1,5}|[0-9a-fA-F]{1,4}:((:[0-9a-fA-F]{1,4}){1,6})|:((:[0-9a-fA-F]{1,4}){1,7}|:)|fe80:(:[0-9a-fA-F]{0,4}){0,4}%[0-9a-zA-Z]{1,}|::(ffff(:0{1,4}){0,1}:){0,1}((25[0-5]|(2[0-4]|1{0,1}[0-9]){0,1}[0-9])\.){3,3}(25[0-5]|(2[0-4]|1{0,1}[0-9]){0,1}[0-9])|([0-9a-fA-F]{1,4}:){1,4}:((25[0-5]|(2[0-4]|1{0,1}[0-9]){0,1}[0-9])\.){3,3}(25[0-5]|(2[0-4]|1{0,1}[0-9]){0,1}[0-9]))/gi
  return regexExp.test(address)
}

export function removeProtocol(str: string): string {
  if (str.startsWith('http://')) return str.slice(7)
  if (str.startsWith('https://')) return str.slice(8)
  return str
}

export function removePort(str: string): string {
  return str.split(':')[0]
}

export function hasUi(
  interfaces: PackageDataEntry['serviceInterfaces'],
): boolean {
  return Object.values(interfaces).some(iface => iface.type === 'ui')
}
