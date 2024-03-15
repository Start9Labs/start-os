import { DOCUMENT } from '@angular/common'
import { Inject, Injectable } from '@angular/core'
import { WorkspaceConfig } from '@start9labs/shared'
import { types } from '@start9labs/start-sdk'
import {
  InstalledPackageDataEntry,
  PackageMainStatus,
  PackageState,
} from 'src/app/services/patch-db/data-model'

type HostnameInfoIp = types.HostnameInfoIp
type HostnameInfoOnion = types.HostnameInfoOnion

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

  isLaunchable(state: PackageState, status: PackageMainStatus): boolean {
    return (
      state === PackageState.Installed && status === PackageMainStatus.Running
    )
  }

  /** ${scheme}://${username}@${host}:${externalPort}${suffix} */
  launchableAddress(
    interfaces: InstalledPackageDataEntry['service-interfaces'],
  ): string {
    const ui = Object.values(interfaces).find(i => i.type === 'ui')

    if (!ui) return ''

    const host = ui.hostInfo
    const addressInfo = ui.addressInfo
    const scheme = this.isHttps() ? 'https' : 'http'
    const username = addressInfo.username ? addressInfo.username + '@' : ''
    const suffix = addressInfo.suffix || ''
    const url = new URL(`${scheme}://${username}placeholder${suffix}`)

    if (host.kind === 'multi') {
      const onionHostname = host.hostnames.find(
        (h: any) => h.kind === 'onion',
      ) as HostnameInfoOnion

      if (this.isTor() && onionHostname) {
        url.hostname = onionHostname.hostname.value
      } else {
        const ipHostname = host.hostnames.find(
          (h: any) => h.kind === 'ip',
        ) as HostnameInfoIp

        if (!ipHostname) return ''

        url.hostname = this.hostname
        url.port = String(
          ipHostname.hostname.sslPort || ipHostname.hostname.port,
        )
      }
    } else {
      const hostname = host.hostname

      if (!hostname) return ''

      if (this.isTor() && hostname.kind === 'onion') {
        url.hostname = hostname.hostname.value
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
  interfaces: InstalledPackageDataEntry['service-interfaces'],
): boolean {
  return Object.values(interfaces).some(iface => iface.type === 'ui')
}
