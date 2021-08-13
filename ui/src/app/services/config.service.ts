import { Injectable } from '@angular/core'
import { InterfaceDef, PackageDataEntry, PackageMainStatus, PackageState } from './patch-db/data-model'

const { patchDb, api, mocks } = require('../../../config.json') as UiConfig

type UiConfig = {
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

  patchDb = patchDb
  api = api
  mocks = mocks

  skipStartupAlerts  = mocks.enabled && mocks.skipStartupAlerts
  isConsulate        = window['platform'] === 'ios'

  isTor (): boolean {
    return (mocks.enabled && mocks.maskAs === 'tor') || this.origin.endsWith('.onion')
  }

  isLan (): boolean {
    return (mocks.enabled && mocks.maskAs === 'lan') || this.origin.endsWith('.local')
  }

  usePoll (): boolean {
    return this.isConsulate || (mocks.enabled && mocks.connection === 'poll')
  }

  isLaunchable (state: PackageState, status: PackageMainStatus, interfaces: { [id: string]: InterfaceDef }): boolean {
    if (this.isConsulate || state !== PackageState.Installed) {
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
  return !!Object.values(interfaces).find(i => i.ui && i['tor-config'])
}

export function hasLanUi (interfaces: { [id: string]: InterfaceDef }): boolean {
  return !!Object.values(interfaces).find(i => i.ui && i['lan-config'])
}

export function torUiAddress (pkg: PackageDataEntry): string {
  const interfaces = pkg.manifest.interfaces
  const id = Object.keys(interfaces).find(key => {
    const val = interfaces[key]
    return val.ui && val['tor-config']
  })
  return pkg.installed['interface-info'].addresses[id]['tor-address']
}

export function lanUiAddress (pkg: PackageDataEntry): string {
  const interfaces = pkg.manifest.interfaces
  const id = Object.keys(interfaces).find(key => {
    const val = interfaces[key]
    return val.ui && val['lan-config']
  })
  return pkg.installed['interface-info'].addresses[id]['lan-address']
}

export function hasUi (interfaces: { [id: string]: InterfaceDef }): boolean {
  return hasTorUi(interfaces) || hasLanUi(interfaces)
}

function removeProtocol (str: string): string {
  if (str.startsWith('http://')) return str.slice(7)
  if (str.startsWith('https://')) return str.slice(8)
  return str
}

function removePort (str: string): string {
  return str.split(':')[0]
}
