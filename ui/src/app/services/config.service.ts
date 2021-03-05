import { Injectable } from '@angular/core'
import { AppStatus } from '../models/app-model'
import { ApiAppInstalledPreview } from './api/api-types'

const { useMocks, mockOver, skipStartupAlerts } = require('../../../use-mocks.json') as UseMocks

type UseMocks = {
  useMocks: boolean
  mockOver: 'tor' | 'lan'
  skipStartupAlerts: boolean
}
@Injectable({
  providedIn: 'root',
})
export class ConfigService {
  origin = removePort(removeProtocol(window.origin))
  version = require('../../../package.json').version

  api = {
    useMocks,
    url: '/api',
    version: '/v0',
    root: '', // empty will default to same origin
  }

  skipStartupAlerts  = skipStartupAlerts
  isConsulate        = window['platform'] === 'ios'

  isTor () : boolean {
    return (this.api.useMocks && mockOver === 'tor') || this.origin.endsWith('.onion')
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
