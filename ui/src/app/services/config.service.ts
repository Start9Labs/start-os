import { Injectable } from '@angular/core'
import { InstalledPackageDataEntry, InterfaceDef, Manifest, PackageDataEntry, PackageMainStatus, PackageState } from '../models/patch-db/data-model'

const { patchDb, maskAs, api, skipStartupAlerts } = require('../../../ui-config.json') as UiConfig

type UiConfig = {
  patchDb: {
    // If this is false (the default), poll will be used if in consulate only. If true it will be on regardless of env. This is useful in concert with api mocks.
    usePollOverride: boolean
    poll: {
      cooldown: number /* in ms */
    }
    websocket: {
      type: 'ws'
      url: string
      version: number
    }
    // Wait this long (ms) before asking BE for a dump when out of order messages are received
    timeoutForMissingRevision: number
  }
  api: {
    mocks: boolean
    url: string
    version: string
    root: string
  }
  maskAs: 'tor' | 'lan' | 'none'
  skipStartupAlerts: boolean
}
@Injectable({
  providedIn: 'root',
})
export class ConfigService {
  origin = removePort(removeProtocol(window.origin))
  version = require('../../../package.json').version

  patchDb = patchDb
  api = api

  skipStartupAlerts  = skipStartupAlerts
  isConsulate        = window['platform'] === 'ios'

  isTor () : boolean {
    return (maskAs === 'tor') || this.origin.endsWith('.onion')
  }

  isLan () : boolean {
    return (maskAs === 'lan') || this.origin.endsWith('.local')
  }

  isLaunchable (pkg: PackageDataEntry): boolean {
    if (this.isConsulate || pkg.state !== PackageState.Installed) {
      return false
    }

    const installed = pkg.installed

    return installed.status.main.status === PackageMainStatus.Running &&
    (
      (hasTorUi(installed.manifest.interfaces) && this.isTor()) ||
      (hasLanUi(installed.manifest.interfaces) && !this.isTor())
    )
  }

  launchableURL (pkg: InstalledPackageDataEntry): string {
    return this.isTor() ? `http://${torUiAddress(pkg)}` : `https://${lanUiAddress(pkg)}`
  }
}

export function hasTorUi (interfaces: { [id: string]: InterfaceDef }): boolean {
  return !!Object.values(interfaces).find(i => i.ui && i['tor-config'])
}

export function hasLanUi (interfaces: { [id: string]: InterfaceDef }): boolean {
  return !!Object.values(interfaces).find(i => i.ui && i['lan-config'])
}

export function torUiAddress (pkg: InstalledPackageDataEntry): string {
  const interfaces = pkg.manifest.interfaces
  const id = Object.keys(interfaces).find(key => {
    const val = interfaces[key]
    return val.ui && val['tor-config']
  })
  return pkg['interface-info'].addresses[id]['tor-address']
}

export function lanUiAddress (pkg: InstalledPackageDataEntry): string {
  const interfaces = pkg.manifest.interfaces
  const id = Object.keys(interfaces).find(key => {
    const val = interfaces[key]
    return val.ui && val['lan-config']
  })
  return pkg['interface-info'].addresses[id]['lan-address']
}

export function hasUi (interfaces: { [id: string]: InterfaceDef }): boolean {
  return hasTorUi(interfaces) || hasLanUi(interfaces)
}

export function getManifest (pkg: PackageDataEntry): Manifest {
  if (pkg.state === PackageState.Installed) {
    return pkg.installed.manifest
  }
  return pkg['temp-manifest']
}

function removeProtocol (str: string): string {
  if (str.startsWith('http://')) return str.slice(7)
  if (str.startsWith('https://')) return str.slice(8)
  return str
}

function removePort (str: string): string {
  return str.split(':')[0]
}
