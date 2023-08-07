import { DOCUMENT } from '@angular/common'
import { Inject, Injectable } from '@angular/core'
import { WorkspaceConfig } from '@start9labs/shared'
import {
  InstalledPackageInfo,
  InterfaceInfo,
} from 'src/app/services/patch-db/data-model'

const {
  packageArch,
  osArch,
  gitHash,
  useMocks,
  ui: { api, marketplace, mocks },
} = require('../../../../../config.json') as WorkspaceConfig

@Injectable({
  providedIn: 'root',
})
export class ConfigService {
  constructor(@Inject(DOCUMENT) private readonly document: Document) {}

  hostname = this.document.location.hostname
  version = require('../../../../../package.json').version as string
  useMocks = useMocks
  mocks = mocks
  packageArch = packageArch
  osArch = osArch
  gitHash = gitHash
  api = api
  marketplace = marketplace
  skipStartupAlerts = useMocks && mocks.skipStartupAlerts

  isTor(): boolean {
    return (
      this.hostname.endsWith('.onion') || (useMocks && mocks.maskAs === 'tor')
    )
  }

  isLocal(): boolean {
    return (
      this.hostname.endsWith('.local') || (useMocks && mocks.maskAs === 'local')
    )
  }

  isLocalhost(): boolean {
    return (
      this.hostname === 'localhost' ||
      (useMocks && mocks.maskAs === 'localhost')
    )
  }

  isIpv4(): boolean {
    return isValidIpv4(this.hostname) || (useMocks && mocks.maskAs === 'ipv4')
  }

  isIpv6(): boolean {
    return isValidIpv6(this.hostname) || (useMocks && mocks.maskAs === 'ipv6')
  }

  isClearnet(): boolean {
    return (
      (useMocks && mocks.maskAs === 'clearnet') ||
      (!this.isTor() &&
        !this.isLocal() &&
        !this.isLocalhost() &&
        !this.isIpv4() &&
        !this.isIpv6())
    )
  }

  isSecure(): boolean {
    return window.isSecureContext || this.isTor()
  }

  launchableAddress(info: InterfaceInfo): string {
    return this.isTor()
      ? info.addressInfo.torHostname
      : this.isLocalhost()
      ? `https://${info.addressInfo.lanHostname}`
      : this.isLocal() || this.isIpv4() || this.isIpv6()
      ? `https://${this.hostname}`
      : info.addressInfo.domainInfo?.subdomain
      ? `https://${info.addressInfo.domainInfo.subdomain}${info.addressInfo.domainInfo.domain}`
      : `https://${info.addressInfo.domainInfo?.domain}`
  }
}

export function isValidIpv4(address: string): boolean {
  const regexExp =
    /^(25[0-5]|2[0-4][0-9]|[01]?[0-9][0-9]?)\.(25[0-5]|2[0-4][0-9]|[01]?[0-9][0-9]?)\.(25[0-5]|2[0-4][0-9]|[01]?[0-9][0-9]?)\.(25[0-5]|2[0-4][0-9]|[01]?[0-9][0-9]?)$/
  return regexExp.test(address)
}

export function isValidIpv6(address: string): boolean {
  const regexExp =
    /(([0-9a-fA-F]{1,4}:){7,7}[0-9a-fA-F]{1,4}|([0-9a-fA-F]{1,4}:){1,7}:|([0-9a-fA-F]{1,4}:){1,6}:[0-9a-fA-F]{1,4}|([0-9a-fA-F]{1,4}:){1,5}(:[0-9a-fA-F]{1,4}){1,2}|([0-9a-fA-F]{1,4}:){1,4}(:[0-9a-fA-F]{1,4}){1,3}|([0-9a-fA-F]{1,4}:){1,3}(:[0-9a-fA-F]{1,4}){1,4}|([0-9a-fA-F]{1,4}:){1,2}(:[0-9a-fA-F]{1,4}){1,5}|[0-9a-fA-F]{1,4}:((:[0-9a-fA-F]{1,4}){1,6})|:((:[0-9a-fA-F]{1,4}){1,7}|:)|fe80:(:[0-9a-fA-F]{0,4}){0,4}%[0-9a-zA-Z]{1,}|::(ffff(:0{1,4}){0,1}:){0,1}((25[0-5]|(2[0-4]|1{0,1}[0-9]){0,1}[0-9])\.){3,3}(25[0-5]|(2[0-4]|1{0,1}[0-9]){0,1}[0-9])|([0-9a-fA-F]{1,4}:){1,4}:((25[0-5]|(2[0-4]|1{0,1}[0-9]){0,1}[0-9])\.){3,3}(25[0-5]|(2[0-4]|1{0,1}[0-9]){0,1}[0-9]))/gi
  return regexExp.test(address)
}

export function removeProtocol(str: string): string {
  if (str.startsWith('http://')) return str.slice(7)
  if (str.startsWith('https://')) return str.slice(8)
  return str
}

export function removePort(str: string): string {
  return str.split(':')[0]
}
