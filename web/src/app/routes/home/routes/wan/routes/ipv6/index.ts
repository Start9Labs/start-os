import { ChangeDetectionStrategy, Component, inject } from '@angular/core'
import { NonNullableFormBuilder, ReactiveFormsModule } from '@angular/forms'
import { TuiTitle } from '@taiga-ui/core'
import { TuiHeader } from '@taiga-ui/layout'
import { Footer } from 'src/app/components/footer'
import { Form } from 'src/app/directives/form'
import { Help } from 'src/app/directives/help'

import { IPv6Aside } from './aside'
import { Ipv6Dns } from './dns'
import { Ipv6Ip } from './ip'
import { Ipv6Summary } from './summary'

@Component({
  template: `
    <ipv6-aside *help />
    <header tuiHeader="h6"><h2 tuiTitle>Summary</h2></header>
    <article ipv6Summary [formLoading]="false"></article>
    <header tuiHeader="h6"><h2 tuiTitle>Settings</h2></header>
    <form [formGroup]="form" [formLoading]="false">
      <ipv6-ip formGroupName="ip" />
      <ipv6-dns formGroupName="dns" />
      <footer appFooter></footer>
    </form>
  `,
  host: { class: 'g-page' },
  imports: [
    ReactiveFormsModule,
    TuiHeader,
    TuiTitle,
    Footer,
    Form,
    Help,
    Ipv6Summary,
    Ipv6Ip,
    Ipv6Dns,
    IPv6Aside,
  ],
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
