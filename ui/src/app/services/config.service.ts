import { Injectable } from '@angular/core'
import { InterfaceDef, PackageDataEntry, PackageMainStatus, PackageState } from './patch-db/data-model'

const { gitHash, patchDb, api, mocks } = require('../../../config.json') as UiConfig

type UiConfig = {
  gitHash: string
  patchDb: {
    poll: {
      cooldown: number /* in ms */
    }
  }
  api: {
    url: string
    version: string
  }
  mocks: {
    enabled: boolean
    connection: 'ws' | 'poll'
    rpcPort: number
    wsPort: number
    maskAs: 'tor' | 'lan'
    skipStartupAlerts: boolean
  }
}

@Injectable({
  providedIn: 'root',
})
export class ConfigService {
  origin = removePort(removeProtocol(window.origin))
  version = require('../../../package.json').version

  gitHash = gitHash
  patchDb = patchDb
  api = api
  mocks = mocks

  skipStartupAlerts  = mocks.enabled && mocks.skipStartupAlerts
  supportsWebSockets = 'WebSocket' in window || 'MozWebSocket' in window

  isTor (): boolean {
    return (mocks.enabled && mocks.maskAs === 'tor') || this.origin.endsWith('.onion')
  }

  isLan (): boolean {
    return (mocks.enabled && mocks.maskAs === 'lan') || this.origin.endsWith('.local')
  }

  usePoll (): boolean {
    return !this.supportsWebSockets || (mocks.enabled && mocks.connection === 'poll')
  }

  isLaunchable (state: PackageState, status: PackageMainStatus, interfaces: { [id: string]: InterfaceDef }): boolean {
    if (state !== PackageState.Installed) {
      return false
    }

    return status === PackageMainStatus.Running &&
    (
      (hasTorUi(interfaces) && this.isTor()) ||
      (hasLanUi(interfaces) && !this.isTor())
    )
  }

  launchableURL (pkg: PackageDataEntry): string {
    return this.isTor() ? `http://${torUiAddress(pkg)}` : `https://${lanUiAddress(pkg)}`
  }
}

export function hasTorUi (interfaces: { [id: string]: InterfaceDef }): boolean {
  const int = getUiInterfaceValue(interfaces)
  return !!int?.['tor-config']
}

export function hasLanUi (interfaces: { [id: string]: InterfaceDef }): boolean {
  const int = getUiInterfaceValue(interfaces)
  return !!int?.['lan-config']
}

export function torUiAddress (pkg: PackageDataEntry): string {
  const key = getUiInterfaceKey(pkg.manifest.interfaces)
  return pkg.installed['interface-addresses'][key]['tor-address']
}

export function lanUiAddress (pkg: PackageDataEntry): string {
  const key = getUiInterfaceKey(pkg.manifest.interfaces)
  return pkg.installed['interface-addresses'][key]['lan-address']
}

export function hasUi (interfaces: { [id: string]: InterfaceDef }): boolean {
  return hasTorUi(interfaces) || hasLanUi(interfaces)
}

export function removeProtocol (str: string): string {
  if (str.startsWith('http://')) return str.slice(7)
  if (str.startsWith('https://')) return str.slice(8)
  return str
}

export function removePort (str: string): string {
  return str.split(':')[0]
}

export function getUiInterfaceKey (interfaces: { [id: string]: InterfaceDef }): string {
  return Object.keys(interfaces).find(key => interfaces[key].ui)
}

export function getUiInterfaceValue (interfaces: { [id: string]: InterfaceDef }): InterfaceDef {
  return Object.values(interfaces).find(i => i.ui)
}
