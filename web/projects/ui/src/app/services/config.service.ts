import { Inject, Injectable, DOCUMENT } from '@angular/core'
import { AccessType, WorkspaceConfig } from '@start9labs/shared'
import { T, utils } from '@start9labs/start-sdk'

const {
  gitHash,
  useMocks,
  ui: { api, mocks },
  defaultRegistry,
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
  defaultRegistry = defaultRegistry

  private getAccessType = utils.once(() => {
    if (useMocks) return mocks.maskAs
    if (this.hostname === 'localhost') return 'localhost'
    if (this.hostname.endsWith('.local')) return 'mdns'
    let ip = null
    try {
      ip = utils.IpAddress.parse(this.hostname.replace(/[\[\]]/g, ''))
    } catch {}
    if (ip) {
      if (utils.IPV4_LOOPBACK.contains(ip) || utils.IPV6_LOOPBACK.contains(ip))
        return 'localhost'
      if (ip.isIpv4()) return ip.isPublic() ? 'wan-ipv4' : 'ipv4'
      return 'ipv6'
    }
    return 'domain'
  })
  get accessType(): AccessType {
    return this.getAccessType()
  }

  isLanHttp(): boolean {
    return !this.isHttps() && this.accessType !== 'localhost'
  }

  isHttps(): boolean {
    return useMocks ? mocks.maskAsHttps : this.protocol === 'https:'
  }
}
