import { ChangeDetectionStrategy, Component, inject } from '@angular/core'
import { NonNullableFormBuilder } from '@angular/forms'
import { TuiCard } from '@taiga-ui/layout'

import { Ipv6Dns } from './dns'
import { Ipv6Ip } from './ip'
import { Ipv6Summary } from './summary'

@Component({
  template: `
    <article ipv6Summary tuiCardLarge="compact"></article>
    <ipv6-ip />
    <ipv6-dns />
  `,
  imports: [TuiCard, Ipv6Summary, Ipv6Ip, Ipv6Dns],
  changeDetection: ChangeDetectionStrategy.OnPush,
})
export default class Ipv6 {
  public readonly builder = inject(NonNullableFormBuilder)
  public readonly form = this.builder.group({
    ip: this.builder.group({
      mode: 'SLAAC',
      slaac: this.builder.group({
        wan: '2001:db8:abcd:1234::2',
        length: '/64',
        gateway: '2001:db8:abcd:1234::1',
      }),
      dhcpv6: this.builder.group({
        wan: '2001:db8:abcd:1234::2',
        length: '/64',
        gateway: '2001:db8:abcd:1234::1',
      }),
      static: this.builder.group({
        wan: '',
        length: '/64',
        gateway: '',
      }),
      '6rd': this.builder.group({
        wan: '2001:db8:abcd:1234::2',
        length: '/64',
        prefix: '',
        ip4: '',
        mask: '0',
        border: '',
      }),
    }),
    dns: this.builder.group({
      mode: 'ISP',
      isp: this.builder.group({ server: 'dns.watch' }),
      tls: this.builder.group({ server: 'Cloudflare (1.1.1.1)' }),
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
      | 'slaac'
      | 'dhcpv6'
      | 'static'
      | '6rd'
      | 'disabled'
  }

  public get dns() {
    return this.form.controls.dns.controls.mode.value.toLowerCase() as
      | 'isp'
      | 'tls'
      | 'custom'
  }
}
