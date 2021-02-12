import { Injectable } from '@angular/core'

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
  isConsulateIos     = window['platform'] === 'ios'
  isConsulateAndroid = window['platform'] === 'android'

  isTor () : boolean {
    return (this.api.useMocks && mockOver === 'tor') || this.origin.endsWith('.onion')
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
