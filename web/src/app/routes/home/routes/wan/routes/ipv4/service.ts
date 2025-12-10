import { inject, Injectable } from '@angular/core'
import {
  calculatePrefix,
  getBootstrapDnsForResolver,
  mapFriendlyNameToResolverUrl,
  mapResolverUrlToFriendlyName,
} from 'src/app/routes/home/routes/wan/routes/ipv4/utils'
import { ApiService } from 'src/app/services/api/api.service'
import { FormService } from 'src/app/services/form.service'

import { WanIpv4Form, WanIpv4 } from './types'

const BLANK = {
  ip: { mode: 'dhcp' },
  dns: { mode: 'isp' },
} as WanIpv4Form

@Injectable()
export class Ipv4Service extends FormService<WanIpv4Form> {
  private readonly api = inject(ApiService)
  private _data?: WanIpv4

  async store({ ip, dns }: WanIpv4Form) {
    const data = JSON.parse(JSON.stringify(this._data)) as WanIpv4

    let wanSection = data.network.sections.find(
      ({ type, name }) => type === 'interface' && name === 'wan',
    )

    if (!wanSection) {
      throw new Error('no WAN')
    }

    // Update WAN interface based on selected mode
    if (ip.mode === 'dhcp') {
      wanSection.options = {
        ...wanSection.options,
        proto: 'dhcp',
        device: wanSection.options.device || 'eth0.2',
      }
      // Remove static/pppoe specific options
      delete wanSection.options.ipaddr
      delete wanSection.options.netmask
      delete wanSection.options.gateway
      delete wanSection.options.username
      delete wanSection.options.password
    } else if (ip.mode === 'static') {
      wanSection.options = {
        ...wanSection.options,
        proto: 'static',
        ipaddr: ip.wan,
        netmask: ip.mask,
        gateway: ip.gateway,
        device: wanSection.options.device || 'eth0.2',
      }
      delete wanSection.options.username
      delete wanSection.options.password
    } else if (ip.mode === 'pppoe') {
      wanSection.options = {
        ...wanSection.options,
        proto: 'pppoe',
        username: ip.wan,
        password: ip.password,
        device: ip.vlan || wanSection.options.device || 'eth0.2',
      }
      delete wanSection.options.ipaddr
      delete wanSection.options.netmask
      delete wanSection.options.gateway
    }

    let dnsmasqSection = data.dhcp.sections.find(s => s.type === 'dnsmasq')

    if (!dnsmasqSection) {
      throw new Error('dnsmasq configuration not found')
    }

    // Handle DNS mode
    if (dns.mode === 'isp') {
      // Clear custom DNS servers
      dnsmasqSection.lists.server = []

      // Disable https-dns-proxy
      data['https-dns-proxy'].sections = []
    } else if (dns.mode === 'tls') {
      // Clear custom DNS servers
      dnsmasqSection.lists.server = []

      // Configure https-dns-proxy
      const resolverUrl = mapFriendlyNameToResolverUrl(dns.server)
      const bootstrapDns = getBootstrapDnsForResolver(dns.server)

      data['https-dns-proxy'].sections = [
        {
          type: 'https-dns-proxy',
          name: null,
          options: {
            bootstrap_dns: bootstrapDns,
            resolver_url: resolverUrl,
            listen_addr: '127.0.0.1',
            listen_port: '5053',
          },
          lists: {},
        },
      ]
    } else if (dns.mode === 'custom') {
      // Disable https-dns-proxy
      data['https-dns-proxy'].sections = []

      // Set custom DNS servers
      const servers: string[] = []
      if (dns['1']) {
        servers.push(dns['1'])
      }
      if (dns['2']) {
        servers.push(dns['2'])
      }

      dnsmasqSection.lists.server = servers
    }

    const {
      network,
      dhcp,
      'https-dns-proxy': dnsTls,
    } = await this.api.setUci<(keyof typeof data)[]>(data)

    await this.api.exec({
      command: '/etc/init.d/network',
      args: ['restart'],
      timeout: 30000,
    })

    await this.api.exec({
      command: '/etc/init.d/dnsmasq',
      args: ['restart'],
      timeout: 10000,
    })

    await this.api.exec({
      command: '/etc/init.d/https-dns-proxy',
      args: ['restart'],
      timeout: 10000,
    })

    data.network.modified = network
    data.dhcp.modified = dhcp
    data['https-dns-proxy'].modified = dnsTls

    this._data = data
  }

  async load() {
    this._data = await this.api.getUci<WanIpv4>({
      names: ['network', 'dhcp', 'https-dns-proxy'],
    })

    return this.getValue(this._data)
  }

  private getValue(data?: WanIpv4): WanIpv4Form {
    if (!data) {
      return { ...BLANK }
    }

    const { network, dhcp, 'https-dns-proxy': dnsTls } = data

    // @TODO Aiden can there be multiple wan?
    const wanInterface = network.sections.find(
      s => s.type === 'interface' && s.name === 'wan',
    )

    if (!wanInterface) {
      // @TODO Aiden what should we do in this scenario?
      throw new Error('No WAN')
    }

    const mode = wanInterface.options.proto
    const ip =
      mode === 'dhcp'
        ? {
            wan: wanInterface.options.ipaddr || '',
            prefix: calculatePrefix(wanInterface.options.netmask),
            mask: wanInterface.options.netmask || '',
            gateway: wanInterface.options.gateway || '',
          }
        : {
            wan: wanInterface.options.ipaddr || '',
            prefix: calculatePrefix(wanInterface.options.netmask),
            mask: wanInterface.options.netmask || '',
            gateway: wanInterface.options.gateway || '',
          }

    if (mode === 'pppoe') {
      ip.wan = wanInterface.options.username || ''
    }

    const result = {
      ip: {
        mode,
        ...ip,
        password: wanInterface.options.password || '',
        vlan: wanInterface.options.device || '',
      },
      dns: {},
    } as WanIpv4Form

    // Parse DNS settings
    const dnsmasqSection = dhcp.sections.find(s => s.type === 'dnsmasq')
    const dnsServers = dnsmasqSection?.lists.server || []

    // Check if using https-dns-proxy (DNS over TLS)
    const firstDnsTls = dnsTls.sections[0]

    if (firstDnsTls) {
      result.dns = {
        mode: 'tls',
        server: mapResolverUrlToFriendlyName(
          firstDnsTls.options.resolver_url || '',
        ),
      } as WanIpv4Form['dns']
    } else if (dnsServers.length > 0) {
      // Custom DNS servers configured
      result.dns = {
        mode: 'custom',
        server: dnsServers[0] || '',
        1: dnsServers[0] || '',
        2: dnsServers[1] || '',
        tls1: false, // Would need additional logic to detect TLS
        tls2: false,
      } as WanIpv4Form['dns']
    } else {
      // Using ISP DNS (no custom servers configured)
      result.dns = { mode: 'isp' } as WanIpv4Form['dns']
    }

    return result
  }
}
