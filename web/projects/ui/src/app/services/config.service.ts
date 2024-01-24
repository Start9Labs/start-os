import { DOCUMENT } from '@angular/common'
import { Inject, Injectable } from '@angular/core'
import { WorkspaceConfig } from '@start9labs/shared'
import {
  InstalledPackageDataEntry,
  PackageMainStatus,
  PackageState,
} from 'src/app/services/patch-db/data-model'

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

  launchableAddress(
    interfaces: InstalledPackageDataEntry['network-interfaces'],
  ): string {
    const ui = interfaces['ui']
    const localUrlStr: string | undefined = ui.addresses['local']?.url
    const localUrl = localUrlStr ? new URL(localUrlStr) : null

    if (this.isTor() || !localUrl) {
      return ui.addresses['tor'].url
    }

    localUrl.hostname = this.hostname

    return localUrl.href
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
  interfaces: InstalledPackageDataEntry['network-interfaces'],
): boolean {
  return Object.values(interfaces).some(iface => iface.type === 'ui')
}

export function removeProtocol(str: string): string {
  if (str.startsWith('http://')) return str.slice(7)
  if (str.startsWith('https://')) return str.slice(8)
  return str
}

export function removePort(str: string): string {
  return str.split(':')[0]
}
