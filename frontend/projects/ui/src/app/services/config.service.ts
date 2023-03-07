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
    return (
      this.hostname.endsWith('.onion') || (useMocks && mocks.maskAs === 'tor')
    )
  }

  isLan(): boolean {
    return (
      this.hostname === 'localhost' ||
      this.hostname.endsWith('.local') ||
      (useMocks && mocks.maskAs === 'lan')
    )
  }

  isSecure(): boolean {
    return window.isSecureContext || this.isTor()
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
