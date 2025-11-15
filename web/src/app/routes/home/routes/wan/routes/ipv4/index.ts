import {
  ChangeDetectionStrategy,
  Component,
  inject,
  signal,
} from '@angular/core'
import { NonNullableFormBuilder } from '@angular/forms'
import { TuiButton } from '@taiga-ui/core'
import { TuiCard } from '@taiga-ui/layout'
import { Help } from 'src/app/directives/help.directive'
import { IPv4Aside } from './aside'
import { Ipv4Dns } from './dns'
import { Ipv4Ip } from './ip'
import { Ipv4Summary } from './summary'
import { ApiService } from 'src/app/services/api/api.service'
import { tuiMarkControlAsTouchedAndValidate } from '@taiga-ui/cdk'
import { TuiButtonLoading } from '@taiga-ui/kit'
import {
  DnsmasqSection,
  HttpsDnsProxySection,
  NetworkInterfaceSection,
  UciFile,
} from 'src/app/services/api/types'

type IpMode = 'dhcp' | 'static' | 'pppoe'
type DnsMode = 'isp' | 'tls' | 'custom'

type WanIpv4 = {
  network: UciFile<NetworkInterfaceSection>
  dhcp: UciFile<DnsmasqSection>
  'https-dns-proxy': UciFile<HttpsDnsProxySection>
}

@Component({
  template: `
    @if (!loading()) {
      @if (error(); as e) {
        <p>{{ e }}</p>
      } @else {
        <ipv4-aside *help />
        <article ipv4Summary tuiCardLarge="compact"></article>
        <ipv4-ip />
        <ipv4-dns />
        <footer class="g-footer">
          <button
            tuiButton
            appearance="flat"
            [disabled]="form.pristine || saving()"
          >
            Cancel
          </button>
          <button
            tuiButton
            [disabled]="form.pristine || form.invalid"
            [loading]="saving()"
            (click)="onSave()"
          >
            Save
          </button>
        </footer>
      }
    }
  `,
  imports: [
    TuiCard,
    TuiButton,
    Ipv4Summary,
    Ipv4Ip,
    Ipv4Dns,
    IPv4Aside,
    Help,
    TuiButtonLoading,
  ],
  changeDetection: ChangeDetectionStrategy.OnPush,
})
export default class Ipv4 {
  private readonly builder = inject(NonNullableFormBuilder)
  private readonly api = inject(ApiService)

  private data = {} as WanIpv4

  readonly loading = signal(true)
  readonly error = signal('')
  readonly saving = signal(false)

  readonly labels: Record<string, string> = {
    // ipv4
    dhcp: 'DHCP',
    static: 'Static',
    pppoe: 'PPPoE',
    wan: 'WAN IP Address',
    prefix: 'Subnet Prefix',
    mask: 'Subnet Mask',
    gateway: 'Gateway IP Address',
    password: 'Password*',
    vlan: 'VLAN ID',
    // dns
    isp: 'Get from ISP',
    tls: 'DNS over TLS',
    custom: 'Custom',
  }

  async ngOnInit() {
    try {
      await this.loadWanSettings()
    } catch (e: any) {
      // @TODO error should be a popover
      this.error.set(e)
      console.error(e)
    } finally {
      this.loading.set(false)
    }
  }

  public readonly form = this.builder.group({
    ip: this.builder.group({
      mode: '' as IpMode,
      dhcp: this.builder.group({
        wan: '',
        prefix: '',
        mask: '',
        gateway: '',
      }),
      static: this.builder.group({
        wan: '',
        prefix: '',
        mask: '',
        gateway: '',
      }),
      pppoe: this.builder.group({
        wan: '',
        password: '',
        vlan: '',
      }),
    }),
    dns: this.builder.group({
      mode: '' as DnsMode,
      isp: this.builder.group({
        server: '',
      }),
      tls: this.builder.group({
        server: '',
      }),
      custom: this.builder.group({
        server: '',
        1: '',
        2: '',
        tls1: false,
        tls2: false,
      }),
      proxy: false,
    }),
  })

  public get ip() {
    return this.form.controls.ip.controls.mode.value
  }

  public get dns() {
    return this.form.controls.dns.controls.mode.value
  }

  async onSave() {
    if (this.form.invalid) {
      tuiMarkControlAsTouchedAndValidate(this.form)
      return
    }

    const { dns, ip } = this.form.getRawValue()

    const data = JSON.parse(JSON.stringify(this.data)) as WanIpv4

    let wanSection = data.network.sections.find(
      s => s.type === 'interface' && s.name === 'wan',
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
      const resolverUrl = this.mapFriendlyNameToResolverUrl(dns.tls.server)
      const bootstrapDns = this.getBootstrapDnsForResolver(dns.tls.server)

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

    this.saving.set(true)

    try {
      await this.api.setUci(data)

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

      this.form.markAsPristine()
      this.data = data
    } catch (e) {
      console.error(e)
    } finally {
      this.saving.set(false)
    }
  }

  private async loadWanSettings() {
    this.data = await this.api.getUci<WanIpv4>({
      names: ['network', 'dhcp', 'https-dns-proxy'],
    })

    const { network, dhcp, 'https-dns-proxy': httpsDnsProxy } = this.data

    // @TODO Aiden can there be multiple wan?
    const wanInterface = network.sections.find(
      s => s.type === 'interface' && s.name === 'wan',
    )

    if (!wanInterface) {
      // @TODO Aiden what should we do in this scenario?
      throw new Error('No WAN')
    }

    const mode = wanInterface.options.proto

    this.form.patchValue({
      ip: {
        mode,
        dhcp:
          mode === 'dhcp'
            ? {
                wan: wanInterface.options.ipaddr || '',
                prefix: this.calculatePrefix(wanInterface.options.netmask),
                mask: wanInterface.options.netmask || '',
                gateway: wanInterface.options.gateway || '',
              }
            : undefined,
        static:
          mode === 'static'
            ? {
                wan: wanInterface.options.ipaddr || '',
                prefix: this.calculatePrefix(wanInterface.options.netmask),
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
    })

    // Parse DNS settings
    const dnsmasqSection = dhcp.sections.find(s => s.type === 'dnsmasq')

    const dnsServers = dnsmasqSection?.lists.server || []

    // Check if using https-dns-proxy (DNS over TLS)
    const hasHttpsProxy = httpsDnsProxy.sections.length > 0
    const httpsProxy = httpsDnsProxy.sections[0]

    if (hasHttpsProxy && httpsProxy) {
      this.form.patchValue({
        dns: {
          mode: 'tls',
          tls: {
            server: this.mapResolverUrlToFriendlyName(
              httpsProxy.options.resolver_url || '',
            ),
          },
        },
      })
    } else if (dnsServers.length > 0) {
      // Custom DNS servers configured
      this.form.patchValue({
        dns: {
          mode: 'custom',
          custom: {
            server: dnsServers[0] || '',
            1: dnsServers[0] || '',
            2: dnsServers[1] || '',
            tls1: false, // Would need additional logic to detect TLS
            tls2: false,
          },
        },
      })
    } else {
      // Using ISP DNS (no custom servers configured)
      this.form.patchValue({
        dns: {
          mode: 'isp',
        },
      })
    }
  }

  private calculatePrefix(netmask: string | undefined): string {
    if (!netmask) return ''

    // Convert netmask to CIDR prefix
    const parts = netmask.split('.').map(Number)
    const binary = parts.map(p => p.toString(2).padStart(8, '0')).join('')
    const prefix = binary.split('1').length - 1

    return `/${prefix}`
  }

  private mapResolverUrlToFriendlyName(url: string): string {
    // Map common resolver URLs to friendly names
    if (url.includes('cloudflare-dns.com') || url.includes('1.1.1.1')) {
      return 'Cloudflare (1.1.1.1)'
    }
    if (url.includes('dns.google')) {
      return 'Google (8.8.8.8)'
    }
    if (url.includes('quad9.net')) {
      return 'Quad9 (9.9.9.9)'
    }
    // Add more mappings as needed
    return url
  }

  private mapFriendlyNameToResolverUrl(friendlyName: string): string {
    if (friendlyName.includes('Cloudflare')) {
      return 'https://cloudflare-dns.com/dns-query'
    }
    if (friendlyName.includes('Google')) {
      return 'https://dns.google/dns-query'
    }
    if (friendlyName.includes('Quad9')) {
      return 'https://dns.quad9.net/dns-query'
    }
    return friendlyName
  }

  private getBootstrapDnsForResolver(friendlyName: string): string {
    if (friendlyName.includes('Cloudflare')) {
      return '1.1.1.1'
    }
    if (friendlyName.includes('Google')) {
      return '8.8.8.8'
    }
    if (friendlyName.includes('Quad9')) {
      return '9.9.9.9'
    }
    return '1.1.1.1'
  }
}
