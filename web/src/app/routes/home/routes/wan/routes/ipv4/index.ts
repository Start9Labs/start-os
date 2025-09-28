import { ChangeDetectionStrategy, Component, inject } from '@angular/core'
import { NonNullableFormBuilder } from '@angular/forms'
import { TuiCard } from '@taiga-ui/layout'

import { Ipv4Dns } from './dns'
import { Ipv4Ip } from './ip'
import { Ipv4Summary } from './summary'

@Component({
  template: `
    <div ipv4Summary tuiCardLarge="compact"></div>
    <ipv4-ip />
    <ipv4-dns />
  `,
  imports: [TuiCard, Ipv4Summary, Ipv4Ip, Ipv4Dns],
  changeDetection: ChangeDetectionStrategy.OnPush,
})
export default class Ipv4 {
  private readonly builder = inject(NonNullableFormBuilder)

  public readonly form = this.builder.group({
    ip: this.builder.group({
      mode: 'DHCP',
      dhcp: this.builder.group({
        wan: '100.65.227.234',
        prefix: '/30',
        mask: '255.255.255.252',
        gateway: '100.65.227.233',
      }),
      static: this.builder.group({
        wan: '',
        prefix: '/30',
        mask: '255.255.255.252',
        gateway: '',
      }),
      pppoe: this.builder.group({
        wan: '',
        password: '',
        vlan: '',
      }),
    }),
    dns: this.builder.group({
      mode: 'ISP',
      isp: this.builder.group({
        server: 'dns.watch',
      }),
      tls: this.builder.group({
        server: 'Cloudflare (1.1.1.1)',
      }),
      custom: this.builder.group({
        server: 'dns.watch',
        1: '',
        2: '',
        tls1: true,
        tls2: true,
      }),
      proxy: false,
    }),
  })

  public get ip() {
    return this.form.controls.ip.controls.mode.value.toLowerCase() as
      | 'dhcp'
      | 'static'
      | 'pppoe'
  }

  public get dns() {
    return this.form.controls.dns.controls.mode.value.toLowerCase() as
      | 'isp'
      | 'tls'
      | 'custom'
  }
}
