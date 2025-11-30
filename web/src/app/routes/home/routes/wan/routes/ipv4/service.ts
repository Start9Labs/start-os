import { inject, Injectable } from '@angular/core'
import { TuiAlertService } from '@taiga-ui/core'
import { TuiNotificationMiddleService } from '@taiga-ui/kit'
import {
  calculatePrefix,
  getBootstrapDnsForResolver,
  mapFriendlyNameToResolverUrl,
  mapResolverUrlToFriendlyName,
} from 'src/app/routes/home/routes/wan/routes/ipv4/utils'
import { ApiService } from 'src/app/services/api/api.service'
import { FormService } from 'src/app/services/form.service'

import { Ipv4, WanIpv4 } from './types'

const BLANK = {
  ip: { mode: 'dhcp' },
  dns: { mode: 'isp' },
} as Ipv4

@Injectable()
export class Ipv4Service extends FormService<Ipv4> {
  private readonly api = inject(ApiService)
  private readonly loading = inject(TuiNotificationMiddleService)
  private readonly alerts = inject(TuiAlertService)

  private data?: WanIpv4

  async load() {
    try {
      this.data = await this.api.getUci<WanIpv4>({
        names: ['network', 'dhcp', 'https-dns-proxy'],
      })
    } catch (e: any) {
      this.alerts.open(e, { appearance: 'negative' }).subscribe()
    }

    return this.getValue(this.data)
  }

  reset() {
    return this.getValue(this.data)
  }

  async save({ ip, dns }: Ipv4) {
    const data = JSON.parse(JSON.stringify(this.data)) as WanIpv4

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
        ipaddr: ip.static.wan,
        netmask: ip.static.mask,
        gateway: ip.static.gateway,
        device: wanSection.options.device || 'eth0.2',
      }
      delete wanSection.options.username
      delete wanSection.options.password
    } else if (ip.mode === 'pppoe') {
      wanSection.options = {
        ...wanSection.options,
        proto: 'pppoe',
        username: ip.pppoe.wan,
        password: ip.pppoe.password,
        device: ip.pppoe.vlan || wanSection.options.device || 'eth0.2',
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
      const resolverUrl = mapFriendlyNameToResolverUrl(dns.tls.server)
      const bootstrapDns = getBootstrapDnsForResolver(dns.tls.server)

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
      if (dns.custom['1']) {
        servers.push(dns.custom['1'])
      }
      if (dns.custom['2']) {
        servers.push(dns.custom['2'])
      }

      dnsmasqSection.lists.server = servers
    }

    const loading = this.loading.open('Saving').subscribe()

    try {
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

      this.data = data
      loading.unsubscribe()

      return true
    } catch (e: any) {
      console.error(e)
      this.alerts.open(e, { appearance: 'negative' }).subscribe()
      loading.unsubscribe()

      return false
    }
  }

  private getValue(data?: WanIpv4): Ipv4 {
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
    const result = {
      ip: {
        mode,
        dhcp:
          mode === 'dhcp'
            ? {
                wan: wanInterface.options.ipaddr || '',
                prefix: calculatePrefix(wanInterface.options.netmask),
                mask: wanInterface.options.netmask || '',
                gateway: wanInterface.options.gateway || '',
              }
            : undefined,
        static:
          mode === 'static'
            ? {
                wan: wanInterface.options.ipaddr || '',
                prefix: calculatePrefix(wanInterface.options.netmask),
                mask: wanInterface.options.netmask || '',
                gateway: wanInterface.options.gateway || '',
              }
            : undefined,
        pppoe:
          mode === 'pppoe'
            ? {
                wan: wanInterface.options.username || '',
                password: wanInterface.options.password || '',
                vlan: wanInterface.options.device || '',
              }
            : undefined,
      },
      dns: {},
    } as Ipv4

    // Parse DNS settings
    const dnsmasqSection = dhcp.sections.find(s => s.type === 'dnsmasq')
    const dnsServers = dnsmasqSection?.lists.server || []

    // Check if using https-dns-proxy (DNS over TLS)
    const firstDnsTls = dnsTls.sections[0]

    if (firstDnsTls) {
      result.dns = {
        mode: 'tls',
        tls: {
          server: mapResolverUrlToFriendlyName(
            firstDnsTls.options.resolver_url || '',
          ),
        },
      } as Ipv4['dns']
    } else if (dnsServers.length > 0) {
      // Custom DNS servers configured
      result.dns = {
        mode: 'custom',
        custom: {
          server: dnsServers[0] || '',
          1: dnsServers[0] || '',
          2: dnsServers[1] || '',
          tls1: false, // Would need additional logic to detect TLS
          tls2: false,
        },
      } as Ipv4['dns']
    } else {
      // Using ISP DNS (no custom servers configured)
      result.dns = { mode: 'isp' } as Ipv4['dns']
    }

    return result
  }
}
