import { DOCUMENT } from '@angular/common'
import { Inject, Injectable } from '@angular/core'
import { WorkspaceConfig } from '@start9labs/shared'
import {
  InstalledPackageInfo,
  PackageMainStatus,
} from 'src/app/services/patch-db/data-model'

const {
  packageArch,
  osArch,
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
  packageArch = packageArch
  osArch = osArch
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

  isLocalhost(): boolean {
    return useMocks
      ? mocks.maskAs === 'localhost'
      : this.hostname === 'localhost'
  }

  isLan(): boolean {
    // @TODO will not work once clearnet arrives
    return !this.isTor()
  }

  isTorHttp(): boolean {
    return this.isTor() && !this.isHttps()
  }

  isLocalHttp(): boolean {
    return this.isLocal() && !this.isHttps()
  }

  isSecure(): boolean {
    return window.isSecureContext || this.isTor()
  }

  private isHttps(): boolean {
    return useMocks ? mocks.maskAsHttps : this.protocol === 'https:'
  }
}

export function hasUi(
  addressInfo: InstalledPackageInfo['address-info'],
): boolean {
  return !!Object.values(addressInfo).find(a => a.ui)
}

export function removeProtocol(str: string): string {
  if (str.startsWith('http://')) return str.slice(7)
  if (str.startsWith('https://')) return str.slice(8)
  return str
}

export function removePort(str: string): string {
  return str.split(':')[0]
}
