import { inject, Injectable } from '@angular/core'
import {
  prefixFromNetmask,
  netmaskFromPrefix,
  WanIpv4Form,
} from 'src/app/routes/home/routes/wan/routes/ipv4/utils'
import { ApiService } from 'src/app/services/api/api.service'
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
export class WanIpv4UciService {
  private readonly api = inject(ApiService)
  private _uciFiles?: UciFiles
  private _cachedData?: WanIpv4Form

  /**
   * Get the current WAN IPv4 address
   * For DHCP/PPPoE, queries the actual assigned IP via ubus
   */
  async getWanIp(): Promise<string | null> {
    try {
      // Use ubus to get the actual WAN IP (works for DHCP, PPPoE, and static)
      const result = await this.api.exec({
        command: 'ubus',
        args: ['call', 'network.interface.wan', 'status'],
        timeout: 5000,
      })

      if (result.exitCode === 0 && result.stdout) {
        const status = JSON.parse(result.stdout)
        const ipv4Addr = status['ipv4-address']?.[0]?.address
        return ipv4Addr || null
      }
    } catch {
      // Fall back to cached/configured IP if ubus fails
      const data = await this.getData()
      return data.ip.wan || null
    }
    return null
  }

  /**
   * Get cached data or load if not available
   */
  async getData(): Promise<WanIpv4Form> {
    if (this._cachedData) return this._cachedData
    return this.get()
  }

  async get() {
    this._uciFiles = await this.api.getUci<UciFiles>({
      names: ['network'],
    })

    // only support one wan for now
    const wanInterface = this._uciFiles.network.sections.find(
      (s): s is NetworkInterfaceSection =>
        s.type === 'interface' && s.name === 'wan',
    )
    if (!wanInterface) {
      throw new Error('No WAN')
    }

    const mode = wanInterface.options.proto as 'dhcp' | 'static' | 'pppoe' // @TODO bespoke NetworkInterface types

    const ip = { mode } as WanIpv4Form['ip']

    // Always read these for summary display (may be ISP-assigned)
    ip.wan = wanInterface.options.ipaddr || ''
    ip.prefix = prefixFromNetmask(wanInterface.options.netmask)
    ip.gateway = wanInterface.options.gateway || ''

    if (mode === 'pppoe') {
      ip.username = wanInterface.options.username || ''
      ip.password = wanInterface.options.password || ''
      ip.device = wanInterface.options.device || ''
    }

    this._cachedData = {
      ip,
    }

    return this._cachedData
  }

  async set({ ip }: WanIpv4Form) {
    const uciFiles = JSON.parse(JSON.stringify(this._uciFiles)) as UciFiles

    const wan = uciFiles.network.sections.find(
      (s): s is NetworkInterfaceSection =>
        s.type === 'interface' && s.name === 'wan',
    )
    if (!wan) {
      throw new Error('no WAN')
    }

    // Update protocol
    wan.options.proto = ip.mode

    if (ip.mode === 'dhcp') {
      // Remove static/pppoe specific options
      delete wan.options.ipaddr
      delete wan.options.netmask
      delete wan.options.gateway
      delete wan.options.username
      delete wan.options.password
    } else if (ip.mode === 'static') {
      // Set static options
      wan.options.ipaddr = ip.wan
      wan.options.netmask = netmaskFromPrefix(ip.prefix)
      wan.options.gateway = ip.gateway

      // Remove pppoe specific options
      delete wan.options.username
      delete wan.options.password
    } else if (ip.mode === 'pppoe') {
      // Set pppoe options
      wan.options.username = ip.username
      wan.options.password = ip.password
      if (ip.device) {
        wan.options.device = ip.device
      }

      // Remove static specific options
      delete wan.options.ipaddr
      delete wan.options.netmask
      delete wan.options.gateway
    }

    await this.api.setUci<(keyof typeof uciFiles)[]>(uciFiles)

    // Clear cache
    this._cachedData = undefined

    // Restart network
    await this.api.exec({
      command: '/etc/init.d/network',
      args: ['restart'],
      timeout: 30000,
    })
  }
}
