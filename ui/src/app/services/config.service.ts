import { Injectable } from '@angular/core'

@Injectable({
  providedIn: 'root',
})
export class ConfigService {
  origin = removePort(removeProtocol(window.origin))
  version = require('../../../package.json').version

  api = {
    useMocks: require('../../../use-mocks.json').useMocks,
    url: '/api',
    version: '/v0',
    root: '', // empty will default to same origin
  }

  isConsulateIos     = window['platform'] === 'ios'
  isConsulateAndroid = window['platform'] === 'android'

  isTor () : boolean {
    return this.api.useMocks || this.origin.endsWith('.onion')
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
