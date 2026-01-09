import { inject, Injectable } from '@angular/core'
import {
  prefixFromNetmask,
  netmaskFromPrefix,
  WanIpv4Form,
} from 'src/app/routes/home/routes/wan/routes/ipv4/utils'
import { ApiService } from 'src/app/services/api/api.service'
import { applyDnsToInterface, parseDnsFromInterface } from '../../../dns/utils'
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
export class Ipv4UciService {
  private readonly api = inject(ApiService)
  private _uciFiles?: UciFiles

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

    return {
      ip,
      dns: parseDnsFromInterface(wanInterface),
    }
  }

  async set({ ip, dns }: WanIpv4Form) {
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

    applyDnsToInterface(dns, wan)

    await this.api.setUci<(keyof typeof uciFiles)[]>(uciFiles)

    // Restart network
    await this.api.exec({
      command: '/etc/init.d/network',
      args: ['restart'],
      timeout: 30000,
    })
  }
}
