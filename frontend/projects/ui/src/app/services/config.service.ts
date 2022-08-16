import { Injectable } from '@angular/core'
import { WorkspaceConfig } from '@start9labs/shared'
import {
  InterfaceDef,
  PackageDataEntry,
  PackageMainStatus,
  PackageState,
} from 'src/app/services/patch-db/data-model'

const {
  targetArch,
  gitHash,
  useMocks,
  ui: { api, mocks, marketplace },
} = require('../../../../../config.json') as WorkspaceConfig

@Injectable({
  providedIn: 'root',
})
export class ConfigService {
  origin = removePort(removeProtocol(window.origin))
  version = require('../../../../../package.json').version
  useMocks = useMocks
  mocks = mocks
  targetArch = targetArch
  gitHash = gitHash
  api = api
  marketplace = marketplace
  skipStartupAlerts = useMocks && mocks.skipStartupAlerts
  isConsulate = (window as any)['platform'] === 'ios'
  supportsWebSockets = !!window.WebSocket || this.isConsulate

  isTor(): boolean {
    return (
      (useMocks && mocks.maskAs === 'tor') || this.origin.endsWith('.onion')
    )
  }

  isLan(): boolean {
    return (
      (useMocks && mocks.maskAs === 'lan') || this.origin.endsWith('.local')
    )
  }

  isLaunchable(
    state: PackageState,
    status: PackageMainStatus,
    interfaces: Record<string, InterfaceDef>,
  ): boolean {
    return (
      state === PackageState.Installed &&
      status === PackageMainStatus.Running &&
      hasUi(interfaces)
    )
  }

  launchableURL(pkg: PackageDataEntry): string {
    if (this.isLan() && hasLanUi(pkg.manifest.interfaces)) {
      return `https://${lanUiAddress(pkg)}`
    } else {
      return `http://${torUiAddress(pkg)}`
    }
  }
}

export function hasTorUi(interfaces: Record<string, InterfaceDef>): boolean {
  const int = getUiInterfaceValue(interfaces)
  return !!int?.['tor-config']
}

export function hasLanUi(interfaces: Record<string, InterfaceDef>): boolean {
  const int = getUiInterfaceValue(interfaces)
  return !!int?.['lan-config']
}

export function torUiAddress({
  manifest,
  installed,
}: PackageDataEntry): string {
  const key = getUiInterfaceKey(manifest.interfaces)
  return installed ? installed['interface-addresses'][key]['tor-address'] : ''
}

export function lanUiAddress({
  manifest,
  installed,
}: PackageDataEntry): string {
  const key = getUiInterfaceKey(manifest.interfaces)
  return installed ? installed['interface-addresses'][key]['lan-address'] : ''
}

export function hasUi(interfaces: Record<string, InterfaceDef>): boolean {
  return hasTorUi(interfaces) || hasLanUi(interfaces)
}

export function removeProtocol(str: string): string {
  if (str.startsWith('http://')) return str.slice(7)
  if (str.startsWith('https://')) return str.slice(8)
  return str
}

export function removePort(str: string): string {
  return str.split(':')[0]
}

export function getUiInterfaceKey(
  interfaces: Record<string, InterfaceDef>,
): string {
  return Object.keys(interfaces).find(key => interfaces[key].ui) || ''
}

export function getUiInterfaceValue(
  interfaces: Record<string, InterfaceDef>,
): InterfaceDef | null {
  return Object.values(interfaces).find(i => i.ui) || null
}
