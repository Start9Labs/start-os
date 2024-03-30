import { DOCUMENT } from '@angular/common'
import { Inject, Injectable } from '@angular/core'
import { WorkspaceConfig } from '@start9labs/shared'
import { types } from '@start9labs/start-sdk'
import { PackageDataEntry } from 'src/app/services/patch-db/data-model'
import { PackageState } from '../../../../../../core/startos/bindings/PackageState'
import { MainStatus } from '../../../../../../core/startos/bindings/MainStatus'
import { ExportedOnionHostname } from '../../../../../../core/startos/bindings/ExportedOnionHostname'
import { ExportedIpHostname } from '../../../../../../core/startos/bindings/ExportedIpHostname'
import { ExportedHostnameInfo } from '../../../../../../core/startos/bindings/ExportedHostnameInfo'

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
    state: PackageState['state'],
    status: MainStatus['status'],
  ): boolean {
    return state === 'installed' && status === 'running'
  }

  /** ${scheme}://${username}@${host}:${externalPort}${suffix} */
  launchableAddress(interfaces: PackageDataEntry['serviceInterfaces']): string {
    const ui = Object.values(interfaces).find(i => i.type === 'ui')

    if (!ui) return ''

    const host = ui.hostInfo
    const addressInfo = ui.addressInfo
    const scheme = this.isHttps() ? 'https' : 'http'
    const username = addressInfo.username ? addressInfo.username + '@' : ''
    const suffix = addressInfo.suffix || ''
    const url = new URL(`${scheme}://${username}placeholder${suffix}`)

    if (host.kind === 'multi') {
      const onionHostname = host.hostnames.find(h => h.kind === 'onion')
        ?.hostname as ExportedOnionHostname

      if (this.isTor() && onionHostname) {
        url.hostname = onionHostname.value
      } else {
        const ipHostname = host.hostnames.find(h => h.kind === 'ip')
          ?.hostname as ExportedIpHostname

        if (!ipHostname) return ''

        url.hostname = this.hostname
        url.port = String(ipHostname.sslPort || ipHostname.port)
      }
    } else {
      throw new Error('unimplemented')
      const hostname = {} as ExportedHostnameInfo // host.hostname

      if (!hostname) return ''

      if (this.isTor() && hostname.kind === 'onion') {
        url.hostname = (hostname.hostname as ExportedOnionHostname).value
      } else {
        url.hostname = this.hostname
        url.port = String(hostname.hostname.sslPort || hostname.hostname.port)
      }
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
