import { Injectable } from '@angular/core'
import { AppStatus } from '../models/app-model'
import { ApiAppInstalledPreview } from './api/api-types'

const { patchDb, maskAs, useMocks, skipStartupAlerts } = require('../../../ui-config.json') as UiConfig

type UiConfig = {
  patchDb: {
    http  : { type: 'mock' } | { type: 'live', url: string }
    source:
        { type: 'poll', cooldown: number  /* in ms */ }
      | { type: 'ws', url: string, version: number }
  }

  useMocks: boolean //@TODO 0.3.0: Deprecated, remove for 0.3.0
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

  api = {
    useMocks: useMocks,
    url: '/api',
    version: '/v0',
    root: '', // empty will default to same origin
  }

  skipStartupAlerts  = skipStartupAlerts
  isConsulate        = window['platform'] === 'ios'

  isTor () : boolean {
    return (this.api.useMocks && maskAs === 'tor') || this.origin.endsWith('.onion')
  }

  hasUI (app: ApiAppInstalledPreview): boolean {
    return app.lanUi || app.torUi
  }

  isLaunchable (app: ApiAppInstalledPreview): boolean {
    return !this.isConsulate &&
      app.status === AppStatus.RUNNING &&
      (
        (app.torAddress && app.torUi && this.isTor()) ||
        (app.lanAddress && app.lanUi && !this.isTor())
      )
  }

}

function removeProtocol (str: string): string {
  if (str.startsWith('http://')) return str.slice(7)
  if (str.startsWith('https://')) return str.slice(8)
  return str
}

function removePort (str: string): string {
  return str.split(':')[0]
}
