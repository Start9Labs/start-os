import {
  ChangeDetectionStrategy,
  Component,
  effect,
  inject,
} from '@angular/core'
import { toSignal } from '@angular/core/rxjs-interop'
import { NonNullableFormBuilder, ReactiveFormsModule } from '@angular/forms'
import { tuiMarkControlAsTouchedAndValidate } from '@taiga-ui/cdk'
import { TuiTitle } from '@taiga-ui/core'
import { TuiHeader } from '@taiga-ui/layout'
import { startWith } from 'rxjs'
import { Footer } from 'src/app/components/footer'
import { Form } from 'src/app/directives/form'
import { Help } from 'src/app/directives/help'
import {
  injectFormService,
  provideFormService,
} from 'src/app/services/form.service'
import { CustomValidators } from 'src/app/utils/validators'
import { IPv6Aside } from './aside'
import { Dns } from '../../dns/dns'
import { updateDnsValidators } from '../../dns/utils'
import { Ipv6Ip } from './ip'
import { Ipv6Summary } from './summary'
import { Ipv6Service } from './service'
import { getWanIpv6Form, updateIpv6Validators, WanIpv6Form } from './utils'

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
      <ipv6-ip formGroupName="ip" />
      @if (ipMode() !== 'disabled') {
        <wan-dns [mode]="dnsMode()" formGroupName="dns" />
      }
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

  readonly form = getWanIpv6Form(this.builder)

  readonly ipMode = toSignal(
    this.form.controls.ip.controls.mode.valueChanges.pipe(
      startWith(this.form.controls.ip.controls.mode.value),
    ),
    { requireSync: true },
  )

  readonly dnsMode = toSignal(
    this.form.controls.dns.controls.mode.valueChanges.pipe(
      startWith(this.form.controls.dns.controls.mode.value),
    ),
    { requireSync: true },
  )

  constructor() {
    // Reset form when data loads
    effect(() => {
      const data = this.service.data()
      if (data && this.form.pristine) {
        this.form.reset(data)
        updateIpv6Validators(this.form, data.ip.mode)
        updateDnsValidators(this.form.controls.dns, data.dns.mode, [
          CustomValidators.ipv4OrIpv6(),
        ])
      }
    })

    // Update validators when IP mode changes
    effect(() => {
      const mode = this.ipMode()
      if (mode) {
        updateIpv6Validators(this.form, mode)
      }
    })

    // Update validators when DNS mode changes
    effect(() => {
      const mode = this.dnsMode()
      if (mode) {
        updateDnsValidators(this.form.controls.dns, mode, [
          CustomValidators.ipv4OrIpv6(),
        ])
      }
    })
  }

  async onSave() {
    if (this.form.invalid) {
      tuiMarkControlAsTouchedAndValidate(this.form)
    } else if (await this.service.save(this.form.getRawValue())) {
      this.form.markAsPristine()
    }
  }
}
