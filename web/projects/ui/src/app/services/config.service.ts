import { DOCUMENT } from '@angular/common'
import { Inject, Injectable } from '@angular/core'
import { WorkspaceConfig } from '@start9labs/shared'
import { T } from '@start9labs/start-sdk'
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
  isConsulate = (window as any)['platform'] === 'ios'
  supportsWebSockets = !!window.WebSocket || this.isConsulate

  isTor(): boolean {
    return useMocks ? mocks.maskAs === 'tor' : this.hostname.endsWith('.onion')
  }

  isLocal(): boolean {
    return useMocks
      ? mocks.maskAs === 'local'
      : this.hostname.endsWith('.local')
  }

  isLanHttp(): boolean {
    return !this.isTor() && !this.isLocalhost() && !this.isHttps()
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
    ) // TODO: select if multiple

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

  private isLocalhost(): boolean {
    return useMocks
      ? mocks.maskAs === 'localhost'
      : this.hostname === 'localhost'
  }

  private isHttps(): boolean {
    return useMocks ? mocks.maskAsHttps : this.protocol === 'https:'
  }
}

export function hasUi(
  interfaces: PackageDataEntry['serviceInterfaces'],
): boolean {
  return Object.values(interfaces).some(iface => iface.type === 'ui')
}
