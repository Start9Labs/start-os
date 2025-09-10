import { Inject, Injectable, DOCUMENT } from '@angular/core'
import { WorkspaceConfig } from '@start9labs/shared'
import { T, utils } from '@start9labs/start-sdk'

const {
  gitHash,
  useMocks,
  ui: { api, mocks },
} = require('../../../../../config.json') as WorkspaceConfig

@Injectable({
  providedIn: 'root',
})
export class ConfigService {
  constructor(@Inject(DOCUMENT) private readonly document: Document) {}

  hostname = this.document.location.hostname
  // includes port
  host = this.document.location.host
  // includes ":" (e.g. "http:")
  protocol = this.document.location.protocol
  version = require('../../../../../package.json').version as string
  useMocks = useMocks
  mocks = mocks
  gitHash = gitHash
  api = api
  skipStartupAlerts = useMocks && mocks.skipStartupAlerts
  supportsWebSockets = !!window.WebSocket

  isTor(): boolean {
    return useMocks ? mocks.maskAs === 'tor' : this.hostname.endsWith('.onion')
  }

  isLocalhost(): boolean {
    return useMocks
      ? mocks.maskAs === 'localhost'
      : this.hostname === 'localhost' || this.hostname === '127.0.0.1'
  }

  isLanHttp(): boolean {
    return !this.isTor() && !this.isLocalhost() && !this.isHttps()
  }

  private isLocal(): boolean {
    return useMocks
      ? mocks.maskAs === 'local'
      : this.hostname.endsWith('.local')
  }

  private isLanIpv4(): boolean {
    return useMocks
      ? mocks.maskAs === 'ipv4'
      : new RegExp(utils.Patterns.ipv4.regex).test(this.hostname) &&
          (this.hostname.startsWith('192.168.') ||
            this.hostname.startsWith('10.') ||
            (this.hostname.startsWith('172.') &&
              !![this.hostname.split('.').map(Number)[1] || NaN].filter(
                n => n >= 16 && n < 32,
              ).length))
  }

  isIpv6(): boolean {
    return useMocks
      ? mocks.maskAs === 'ipv6'
      : new RegExp(utils.Patterns.ipv6.regex).test(this.hostname)
  }

  isClearnet(): boolean {
    return useMocks
      ? mocks.maskAs === 'clearnet'
      : this.isHttps() &&
          !this.isTor() &&
          !this.isLocal() &&
          !this.isLocalhost() &&
          !this.isLanIpv4() &&
          !this.isIpv6()
  }

  isHttps(): boolean {
    return useMocks ? mocks.maskAsHttps : this.protocol === 'https:'
  }
}
