import { Injectable } from '@angular/core'
import {
  InterfaceDef,
  PackageDataEntry,
  PackageMainStatus,
  PackageState,
} from './patch-db/data-model'
import { WorkspaceConfig } from '@shared'

const {
  useMocks,
  ui: { gitHash, patchDb, api, mocks },
} = require('../../../../../config.json') as WorkspaceConfig

@Injectable({
  providedIn: 'root',
})
export class ConfigService {
  origin = removePort(removeProtocol(window.origin))
  version = require('../../../../../package.json').version

  useMocks = useMocks
  mocks = mocks

  gitHash = gitHash
  patchDb = patchDb
  api = api

  skipStartupAlerts = useMocks && mocks.skipStartupAlerts
  isConsulate = window['platform'] === 'ios'
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
    if (state !== PackageState.Installed) {
      return false
    }

    return (
      status === PackageMainStatus.Running &&
      ((hasTorUi(interfaces) && this.isTor()) ||
        (hasLanUi(interfaces) && !this.isTor()))
    )
  }

  launchableURL(pkg: PackageDataEntry): string {
    return this.isTor()
      ? `http://${torUiAddress(pkg)}`
      : `https://${lanUiAddress(pkg)}`
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

export function torUiAddress(pkg: PackageDataEntry): string {
  const key = getUiInterfaceKey(pkg.manifest.interfaces)
  return pkg.installed['interface-addresses'][key]['tor-address']
}

export function lanUiAddress(pkg: PackageDataEntry): string {
  const key = getUiInterfaceKey(pkg.manifest.interfaces)
  return pkg.installed['interface-addresses'][key]['lan-address']
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
  return Object.keys(interfaces).find(key => interfaces[key].ui)
}

export function getUiInterfaceValue(
  interfaces: Record<string, InterfaceDef>,
): InterfaceDef {
  return Object.values(interfaces).find(i => i.ui)
}
