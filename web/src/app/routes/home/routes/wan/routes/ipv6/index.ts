import {
  ChangeDetectionStrategy,
  Component,
  effect,
  inject,
} from '@angular/core'
import { NonNullableFormBuilder, ReactiveFormsModule } from '@angular/forms'
import { TuiTitle } from '@taiga-ui/core'
import { TuiHeader } from '@taiga-ui/layout'
import { Footer } from 'src/app/components/footer'
import { Form } from 'src/app/directives/form'
import { Help } from 'src/app/directives/help'
import {
  injectFormService,
  provideFormService,
} from 'src/app/services/form.service'
import { IPv6Aside } from './aside'
import { Ipv6Ip } from './ip'
import { Ipv6Summary } from './summary'
import { Ipv6Service } from './service'
import { getWanIpv6Form, WanIpv6Form } from './utils'
import { tuiMarkControlAsTouchedAndValidate } from '@taiga-ui/cdk'
import { Dns } from '../../dns/dns'

@Component({
  template: `
    <ipv6-aside *help />
    <header tuiHeader="h6"><h2 tuiTitle>Summary</h2></header>
    <article ipv6Summary [formLoading]="!service.data()"></article>
    <header tuiHeader="h6"><h2 tuiTitle>Settings</h2></header>
    <form
      [formGroup]="form"
      [formLoading]="!service.data()"
      (reset.prevent)="form.reset(service.data())"
      (ngSubmit)="onSave()"
    >
      <ipv6-ip [mode]="ipMode" formGroupName="ip" />
      <wan-dns [mode]="dnsMode" formGroupName="dns" />
      @if (service.data()) {
        <footer appFooter [disabled]="form.pristine"></footer>
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
    Ipv6Summary,
    Ipv6Ip,
    Dns,
    IPv6Aside,
  ],
  host: { class: 'g-page' },
  providers: [provideFormService(Ipv6Service)],
  changeDetection: ChangeDetectionStrategy.OnPush,
})
export default class Ipv6 {
  protected readonly builder = inject(NonNullableFormBuilder)
  protected readonly service = injectFormService<WanIpv6Form>()

  public readonly form = getWanIpv6Form(this.builder)

  constructor() {
    effect(() => {
      if (this.service.data() && this.form.pristine) {
        this.form.reset(this.service.data())
      }
    })
  }

  public get ipMode() {
    return this.form.controls.ip.controls.mode.value
  }

  public get dnsMode() {
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
