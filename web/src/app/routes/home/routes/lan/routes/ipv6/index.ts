import {
  ChangeDetectionStrategy,
  Component,
  computed,
  effect,
  inject,
} from '@angular/core'
import { toSignal } from '@angular/core/rxjs-interop'
import { NonNullableFormBuilder, ReactiveFormsModule } from '@angular/forms'
import { tuiMarkControlAsTouchedAndValidate } from '@taiga-ui/cdk'
import { TuiExpand, tuiNumberFormatProvider, TuiTitle } from '@taiga-ui/core'
import { TuiHeader } from '@taiga-ui/layout'
import { startWith } from 'rxjs'
import { Footer } from 'src/app/components/footer'
import { Form } from 'src/app/directives/form'
import { Help } from 'src/app/directives/help'
import {
  injectFormService,
  provideFormService,
} from 'src/app/services/form.service'
import { IPv6Aside } from './aside'
import { LanIpv6Service } from './service'
import { LanIpv6Strategy } from './strategy'
import { LanIpv6Subnet } from './subnet'
import { LanIpv6Summary } from './summary'
import { getLanIpv6Form, updateLanIpv6Validators } from './utils'
import { LanIpv6Data } from './uci/service'

@Component({
  template: `
    <ipv6-aside *help />
    <header tuiHeader="h6"><h2 tuiTitle>Summary</h2></header>
    <article lanIpv6Summary [formLoading]="!service.data()"></article>
    <header tuiHeader="h6"><h2 tuiTitle>Settings</h2></header>
    <form
      [formGroup]="form"
      [formLoading]="!service.data()"
      (reset.prevent)="form.reset(service.data())"
      (ngSubmit)="onSave()"
    >
      <lan-ipv6-strategy formGroupName="strategy" />
      <tui-expand [expanded]="slaacEnabled()">
        <lan-ipv6-subnet formGroupName="subnet" />
      </tui-expand>
      @if (service.data()) {
        <footer appFooter [disabled]="form.pristine"></footer>
      }
    </form>
  `,
  host: { class: 'g-page' },
  imports: [
    ReactiveFormsModule,
    TuiHeader,
    TuiTitle,
    TuiExpand,
    Footer,
    Form,
    Help,
    LanIpv6Summary,
    LanIpv6Strategy,
    LanIpv6Subnet,
    IPv6Aside,
  ],
  providers: [
    provideFormService(LanIpv6Service),
    tuiNumberFormatProvider({ precision: 0 }),
  ],
  changeDetection: ChangeDetectionStrategy.OnPush,
})
export default class LanIpv6 {
  protected readonly builder = inject(NonNullableFormBuilder)
  protected readonly service = injectFormService<LanIpv6Data>()

  readonly form = getLanIpv6Form(this.builder)

  readonly slaacEnabled = toSignal(
    this.form.controls.strategy.controls.slaac.valueChanges.pipe(
      startWith(this.form.controls.strategy.controls.slaac.value),
    ),
    { requireSync: true },
  )

  readonly wanPrefix = computed(() => this.service.data()?.wanPrefix ?? 48)

  constructor() {
    // Reset form when data loads
    effect(() => {
      const data = this.service.data()
      if (data && this.form.pristine) {
        this.form.reset(data)
        updateLanIpv6Validators(
          this.form,
          data.strategy.slaac,
          data.wanPrefix ?? 48,
        )
      }
    })

    // Update validators when SLAAC mode changes
    effect(() => {
      const slaac = this.slaacEnabled()
      const wanPrefix = this.wanPrefix()
      updateLanIpv6Validators(this.form, slaac, wanPrefix)
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
