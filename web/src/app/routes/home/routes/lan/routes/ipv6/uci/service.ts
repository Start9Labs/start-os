import { inject, Injectable } from '@angular/core'
import { ApiService } from 'src/app/services/api/api.service'
import { LanIpv6Form, numberToPrefix, prefixToNumber } from '../utils'
import {
  DhcpSection,
  NetworkInterfaceSection,
  UciFile,
  UciSection,
} from 'src/app/services/api/types'

type UciFiles = {
  network: UciFile<UciSection>
  dhcp: UciFile<UciSection>
}

export type LanIpv6Data = LanIpv6Form & {
  ip6addr?: string // For summary display
  wanPrefix?: number // WAN prefix for validation (read-only)
}

@Injectable({
  providedIn: 'root',
})
export class LanIpv6UciService {
  private readonly api = inject(ApiService)
  private _uciFiles?: UciFiles
  private _cachedData?: LanIpv6Data

  /**
   * Check if LAN IPv6 is enabled (SLAAC or DHCPv6 active)
   */
  async isEnabled(): Promise<boolean> {
    const data = await this.getData()
    return data.strategy.slaac || data.strategy.dhcpv6
  }

  /**
   * Check if DHCPv6 is enabled (required for IPv6 reservations)
   */
  async isDhcpv6Enabled(): Promise<boolean> {
    const data = await this.getData()
    return data.strategy.dhcpv6
  }

  /**
   * Get cached data or load if not available
   */
  async getData(): Promise<LanIpv6Data> {
    if (this._cachedData) return this._cachedData
    return this.get()
  }

  async get(): Promise<LanIpv6Data> {
    this._uciFiles = await this.api.getUci<UciFiles>({
      names: ['network', 'dhcp'],
    })

    // Find LAN interface for prefix length
    const lanInterface = this._uciFiles.network.sections.find(
      (s): s is NetworkInterfaceSection =>
        s.type === 'interface' && s.name === 'lan',
    )

    // Find WAN6 interface to get WAN prefix
    const wan6Interface = this._uciFiles.network.sections.find(
      (s): s is NetworkInterfaceSection =>
        s.type === 'interface' && (s.name === 'wan6' || s.name === 'wan_6'),
    )

    // Find LAN DHCP section for SLAAC/DHCPv6 settings
    const dhcpLan = this._uciFiles.dhcp.sections.find(
      (s): s is DhcpSection => s.type === 'dhcp' && s.name === 'lan',
    )

    // Determine SLAAC state (ra = 'server' means enabled)
    const slaac = dhcpLan?.options.ra === 'server'

    // Determine DHCPv6 state
    const dhcpv6 = dhcpLan?.options.dhcpv6 === 'server'

    // Get LAN prefix length (default to 64)
    const prefixNum = lanInterface?.options.ip6assign
      ? parseInt(lanInterface.options.ip6assign, 10)
      : 64

    // Get WAN prefix (from reqprefix or default to 48)
    // 'auto' means the ISP assigns dynamically, typically /48 or /56
    let wanPrefix = 48
    if (
      wan6Interface?.options.reqprefix &&
      wan6Interface.options.reqprefix !== 'auto'
    ) {
      const parsed = parseInt(wan6Interface.options.reqprefix, 10)
      if (!isNaN(parsed)) {
        wanPrefix = parsed
      }
    }

    // Get IPv6 address for summary (if assigned)
    const ip6addr = lanInterface?.options.ip6addr || ''

    this._cachedData = {
      strategy: {
        slaac,
        dhcpv6,
      },
      subnet: {
        prefix: numberToPrefix(prefixNum),
      },
      ip6addr,
      wanPrefix,
    }

    return this._cachedData
  }

  async set(form: LanIpv6Form): Promise<void> {
    if (!this._uciFiles) {
      throw new Error('Configuration not loaded yet')
    }

    const uciFiles = JSON.parse(JSON.stringify(this._uciFiles)) as UciFiles

    // Find or create LAN interface
    let lanInterface = uciFiles.network.sections.find(
      (s): s is NetworkInterfaceSection =>
        s.type === 'interface' && s.name === 'lan',
    )

    if (!lanInterface) {
      lanInterface = {
        type: 'interface',
        name: 'lan',
        options: {
          proto: 'static',
        },
        lists: {},
      }
      uciFiles.network.sections.push(lanInterface)
    }

    // Find or create LAN DHCP section
    let dhcpLan = uciFiles.dhcp.sections.find(
      (s): s is DhcpSection => s.type === 'dhcp' && s.name === 'lan',
    )

    if (!dhcpLan) {
      dhcpLan = {
        type: 'dhcp',
        name: 'lan',
        options: {
          interface: 'lan',
          start: '100',
          limit: '150',
          leasetime: '12h',
        },
        lists: {},
      }
      uciFiles.dhcp.sections.push(dhcpLan)
    }

    // Update SLAAC (Router Advertisement)
    if (form.strategy.slaac) {
      dhcpLan.options.ra = 'server'
      lanInterface.options.ip6assign = String(
        prefixToNumber(form.subnet.prefix),
      )
      // Preserve existing DHCPv6 setting (don't modify it)
    } else {
      dhcpLan.options.ra = 'disabled'
      delete lanInterface.options.ip6assign
      // Disable DHCPv6 when SLAAC is disabled
      dhcpLan.options.dhcpv6 = 'disabled'
    }

    await this.api.setUci<(keyof typeof uciFiles)[]>(uciFiles)

    // Clear cache
    this._cachedData = undefined

    // Restart services
    await this.api.exec({
      command: '/etc/init.d/network',
      args: ['restart'],
      timeout: 30000,
    })

    await this.api.exec({
      command: '/etc/init.d/odhcpd',
      args: ['restart'],
      timeout: 10000,
    })
  }
}
