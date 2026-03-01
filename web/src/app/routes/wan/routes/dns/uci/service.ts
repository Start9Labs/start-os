import { inject, Injectable } from '@angular/core'
import { ApiService } from 'src/app/services/api/api.service'
import {
  NetworkInterfaceSection,
  UciFile,
  UciSection,
} from 'src/app/services/api/types'
import { applyDnsToInterface, DnsForm, parseDnsFromInterface } from '../utils'

type UciFiles = {
  network: UciFile<UciSection>
}

@Injectable({
  providedIn: 'root',
})
export class DnsUciService {
  private readonly api = inject(ApiService)
  private _uciFiles?: UciFiles
  private _cachedData?: DnsForm

  /**
   * Get cached data or load if not available
   */
  async getData(): Promise<DnsForm> {
    if (this._cachedData) return this._cachedData
    return this.get()
  }

  async get(): Promise<DnsForm> {
    this._uciFiles = await this.api.getUci<UciFiles>({
      names: ['network'],
    })

    const wanInterface = this._uciFiles.network.sections.find(
      (s): s is NetworkInterfaceSection =>
        s.type === 'interface' && s.name === 'wan',
    )

    if (!wanInterface) {
      throw new Error('No WAN interface found')
    }

    const wan6Interface = this._uciFiles.network.sections.find(
      (s): s is NetworkInterfaceSection =>
        s.type === 'interface' && s.name === 'wan6',
    )

    // Read DNS from wan interface (primary), fallback to wan6 if wan doesn't have custom DNS
    let dns = parseDnsFromInterface(wanInterface)

    // If wan is using ISP DNS but wan6 has custom DNS, prefer wan6 custom DNS
    if (dns.mode === 'isp' && wan6Interface) {
      const wan6Dns = parseDnsFromInterface(wan6Interface)
      if (wan6Dns.mode === 'custom') {
        dns = wan6Dns
      }
    }

    this._cachedData = dns
    return this._cachedData
  }

  async set(dns: DnsForm): Promise<void> {
    if (!this._uciFiles) {
      throw new Error('Configuration not loaded yet')
    }

    const uciFiles = JSON.parse(JSON.stringify(this._uciFiles)) as UciFiles

    const wan = uciFiles.network.sections.find(
      (s): s is NetworkInterfaceSection =>
        s.type === 'interface' && s.name === 'wan',
    )

    if (!wan) {
      throw new Error('No WAN interface found')
    }

    const wan6 = uciFiles.network.sections.find(
      (s): s is NetworkInterfaceSection =>
        s.type === 'interface' && s.name === 'wan6',
    )

    // Apply DNS to wan interface
    applyDnsToInterface(dns, wan)

    // Apply DNS to wan6 interface if it exists and is not disabled
    if (wan6 && wan6.options?.proto !== 'none') {
      applyDnsToInterface(dns, wan6)
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
