import { inject, Injectable } from '@angular/core'
import { ApiService } from 'src/app/services/api/api.service'
import { WanIpv6Form } from '../utils'
import {
  NetworkInterfaceSection,
  UciFile,
  UciSection,
} from 'src/app/services/api/types'

type UciFiles = {
  network: UciFile<UciSection>
}

@Injectable({
  providedIn: 'root',
})
export class WanIpv6UciService {
  private readonly api = inject(ApiService)
  private _uciFiles?: UciFiles
  private _cachedData?: WanIpv6Form

  /**
   * Check if WAN IPv6 is enabled (not disabled mode)
   */
  async isEnabled(): Promise<boolean> {
    const data = await this.getData()
    return data.ip.mode !== 'disabled'
  }

  /**
   * Get cached data or load if not available
   */
  async getData(): Promise<WanIpv6Form> {
    if (this._cachedData) return this._cachedData
    return this.get()
  }

  async get() {
    this._uciFiles = await this.api.getUci<UciFiles>({
      names: ['network'],
    })

    // Find WAN6 interface
    const wan6Interface = this._uciFiles.network.sections.find(
      (s): s is NetworkInterfaceSection =>
        s.type === 'interface' && (s.name === 'wan6' || s.name === 'wan_6'),
    )

    if (!wan6Interface) {
      this._cachedData = {
        ip: { mode: 'disabled' },
      } as WanIpv6Form
      return this._cachedData
    }

    const proto = wan6Interface.options.proto

    // Determine mode based on proto and options
    let mode: WanIpv6Form['ip']['mode'] = 'slaac'

    if (proto === 'none') {
      mode = 'disabled'
    } else if (proto === 'static') {
      mode = 'static'
    } else if (proto === '6rd') {
      mode = '6rd'
    } else if (proto === 'dhcpv6') {
      // Distinguish between SLAAC and DHCPv6
      if (wan6Interface.options.reqaddress === 'force') {
        mode = 'dhcpv6'
      } else {
        mode = 'slaac'
      }
    }

    const ip = { mode } as WanIpv6Form['ip']

    // Always read these for summary display (may be ISP-assigned)
    const ip6addr = wan6Interface.options.ip6addr || ''
    const [addr, prefix] = ip6addr.split('/')
    ip.wan = addr || ''
    ip.prefix = prefix ? `/${prefix}` : ''
    ip.gateway = wan6Interface.options.ip6gw || ''

    // 6RD specific fields
    ip.ip4 = wan6Interface.options.peeraddr || ''
    ip.mask = wan6Interface.options.ip6prefixlen
      ? `/${wan6Interface.options.ip6prefixlen}`
      : ''
    ip.border = wan6Interface.options.ip6prefix || ''

    this._cachedData = {
      ip,
    }

    return this._cachedData
  }

  async set({ ip }: WanIpv6Form) {
    if (!this._uciFiles) {
      throw new Error('Configuration not loaded yet')
    }

    const uciFiles = JSON.parse(JSON.stringify(this._uciFiles)) as UciFiles

    // Find or create WAN6 interface
    let wan6 = uciFiles.network.sections.find(
      (s): s is NetworkInterfaceSection =>
        s.type === 'interface' && (s.name === 'wan6' || s.name === 'wan_6'),
    )

    if (ip.mode === 'disabled') {
      // If disabling IPv6 and interface exists, set to none
      if (wan6) {
        wan6.options.proto = 'none'

        // Clear all IPv6-specific options
        delete wan6.options.ip6addr
        delete wan6.options.ip6gw
        delete wan6.options.ip6prefix
        delete wan6.options.reqaddress
        delete wan6.options.reqprefix
        delete wan6.options.peeraddr
        delete wan6.options.ip6prefixlen
      }
      // If it doesn't exist, nothing to do
    } else {
      // User wants to enable IPv6 - interface must exist or be created
      if (!wan6) {
        wan6 = {
          type: 'interface',
          name: 'wan6',
          options: {
            proto: 'dhcpv6',
            device: '@wan',
          },
          lists: {},
        }
        uciFiles.network.sections.push(wan6)
      }

      // Update protocol
      if (ip.mode === 'slaac') {
        wan6.options.proto = 'dhcpv6'
        wan6.options.reqaddress = 'try'
        wan6.options.reqprefix = 'auto'

        // Remove other mode-specific options
        delete wan6.options.ip6addr
        delete wan6.options.ip6gw
        delete wan6.options.peeraddr
        delete wan6.options.ip6prefix
        delete wan6.options.ip6prefixlen
      } else if (ip.mode === 'dhcpv6') {
        wan6.options.proto = 'dhcpv6'
        wan6.options.reqaddress = 'force'
        wan6.options.reqprefix = 'auto'

        delete wan6.options.ip6addr
        delete wan6.options.ip6gw
        delete wan6.options.peeraddr
        delete wan6.options.ip6prefix
        delete wan6.options.ip6prefixlen
      } else if (ip.mode === 'static') {
        wan6.options.proto = 'static'
        wan6.options.ip6addr = `${ip.wan}${ip.prefix}`
        wan6.options.ip6gw = ip.gateway

        delete wan6.options.reqaddress
        delete wan6.options.reqprefix
        delete wan6.options.peeraddr
        delete wan6.options.ip6prefix
        delete wan6.options.ip6prefixlen
      } else if (ip.mode === '6rd') {
        wan6.options.proto = '6rd'
        wan6.options.peeraddr = ip.ip4
        wan6.options.ip6prefix = ip.border
        // Strip leading slash from mask for UCI
        wan6.options.ip6prefixlen = ip.mask.replace(/^\//, '')

        delete wan6.options.ip6addr
        delete wan6.options.ip6gw
        delete wan6.options.reqaddress
        delete wan6.options.reqprefix
      }
    }

    await this.api.setUci<(keyof typeof uciFiles)[]>(uciFiles)

    // Clear cache
    this._cachedData = undefined

    // Restart network service
    await this.api.exec({
      command: '/etc/init.d/network',
      args: ['restart'],
      timeout: 30000,
    })
  }
}
