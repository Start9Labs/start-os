import { inject, Injectable } from '@angular/core'
import { ApiService } from 'src/app/services/api/api.service'
import { WanIpv6Form } from '../utils'
import { applyDnsToInterface, parseDnsFromInterface } from '../../../dns/utils'
import { NetworkInterfaceSection, UciFile } from 'src/app/services/api/types'

type UciFiles = {
  network: UciFile<NetworkInterfaceSection>
}

@Injectable({
  providedIn: 'root',
})
export class Ipv6UciService {
  private readonly api = inject(ApiService)
  private _uciFiles?: UciFiles

  async get() {
    this._uciFiles = await this.api.getUci<UciFiles>({
      names: ['network'],
    })

    // Find WAN6 interface
    const wan6Interface = this._uciFiles.network.sections.find(
      s => s.type === 'interface' && (s.name === 'wan6' || s.name === 'wan_6'),
    )

    if (!wan6Interface) {
      return {
        ip: { mode: 'disabled' },
        dns: { mode: 'isp' },
      } as WanIpv6Form
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

    // Parse IP fields based on mode
    const ip = { mode } as WanIpv6Form['ip']

    if (mode === 'slaac' || mode === 'dhcpv6' || mode === 'static') {
      // Parse ip6addr which is in format "2001:db8::1/64"
      const ip6addr = wan6Interface.options.ip6addr || ''
      const [addr, prefix] = ip6addr.split('/')
      ip.wan = addr
      ip.prefix = prefix ? `/${prefix}` : '/64'
      ip.gateway = wan6Interface.options.ip6gw || ''
    } else if (mode === '6rd') {
      ip.ip4 = wan6Interface.options.peeraddr || ''
      ip.mask = wan6Interface.options.ip6prefixlen || ''
      ip.border = wan6Interface.options.ip6prefix || ''
    }

    return {
      ip,
      dns: parseDnsFromInterface(wan6Interface),
    }
  }

  async set({ ip, dns }: WanIpv6Form) {
    if (!this._uciFiles) {
      throw new Error('Configuration not loaded yet')
    }

    const uciFiles = JSON.parse(JSON.stringify(this._uciFiles)) as UciFiles

    // Find or create WAN6 interface
    let wan6 = uciFiles.network.sections.find(
      ({ type, name }) =>
        type === 'interface' && (name === 'wan6' || name === 'wan_6'),
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
        wan6.options.ip6prefixlen = ip.mask

        delete wan6.options.ip6addr
        delete wan6.options.ip6gw
        delete wan6.options.reqaddress
        delete wan6.options.reqprefix
      }

      // Apply DNS configuration
      applyDnsToInterface(dns, wan6)
    }

    await this.api.setUci<(keyof typeof uciFiles)[]>(uciFiles)

    // Restart network service
    await this.api.exec({
      command: '/etc/init.d/network',
      args: ['restart'],
      timeout: 30000,
    })
  }
}
