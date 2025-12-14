import {
  ChangeDetectionStrategy,
  Component,
  effect,
  inject,
} from '@angular/core'
import { NonNullableFormBuilder, ReactiveFormsModule } from '@angular/forms'
import { tuiMarkControlAsTouchedAndValidate } from '@taiga-ui/cdk'
import { TuiTitle } from '@taiga-ui/core'
import { TuiHeader } from '@taiga-ui/layout'
import { Footer } from 'src/app/components/footer'
import { Form } from 'src/app/directives/form'
import { Help } from 'src/app/directives/help'
import {
  injectFormService,
  provideFormService,
} from 'src/app/services/form.service'

import { IPv4Aside } from './aside'
import { Ipv4Dns } from './dns'
import { Ipv4Ip } from './ip'
import { Ipv4Service } from './service'
import { Ipv4Summary } from './summary'
import { WanIpv4Form } from './types'

@Component({
  template: `
    <ipv4-aside *help />
    <header tuiHeader="h6"><h2 tuiTitle>Summary</h2></header>
    <article ipv4Summary [formLoading]="!service.data()"></article>
    <header tuiHeader="h6"><h2 tuiTitle>Settings</h2></header>
    <form
      [formGroup]="form"
      [formLoading]="!service.data()"
      (reset.prevent)="form.reset(service.data())"
      (ngSubmit)="onSave()"
    >
      <ipv4-ip formGroupName="ip" />
      <ipv4-dns formGroupName="dns" />
      @if (service.data()) {
        <footer appFooter></footer>
      }
    </form>
  `,
  imports: [
    ReactiveFormsModule,
    TuiHeader,
    TuiTitle,
    Footer,
    Form,
    Help,
    Ipv4Summary,
    Ipv4Ip,
    Ipv4Dns,
    IPv4Aside,
  ],
  host: { class: 'g-page' },
  providers: [provideFormService(Ipv4Service)],
  changeDetection: ChangeDetectionStrategy.OnPush,
})
export default class Ipv4 {
  protected readonly builder = inject(NonNullableFormBuilder)
  protected readonly service = injectFormService<WanIpv4Form>()

  readonly form = this.builder.group({
    ip: this.builder.group({
      mode: 'dhcp' as WanIpv4Form['ip']['mode'],
      wan: '',
      prefix: '',
      mask: '',
      gateway: '',
      password: '',
      vlan: '',
    }),
    dns: this.builder.group({
      mode: 'isp' as WanIpv4Form['dns']['mode'],
      server: '',
      1: '',
      2: '',
      tls1: false,
      tls2: false,
      proxy: false,
    }),
  })

  constructor() {
    effect(() => {
      if (this.service.data() && this.form.pristine) {
        this.form.reset(this.service.data())
      }
    })
  }

  public get ip() {
    return this.form.controls.ip.controls.mode.value
  }

  public get dns() {
    return this.form.controls.dns.controls.mode.value
  }

  async onSave() {
    if (this.form.invalid) {
      tuiMarkControlAsTouchedAndValidate(this.form)
    } else if (await this.service.save(this.form.getRawValue())) {
      this.form.markAsPristine()
    }
  }
}
