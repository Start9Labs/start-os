import {
  ChangeDetectionStrategy,
  Component,
  computed,
  effect,
  inject,
  signal,
} from '@angular/core'
import { toSignal } from '@angular/core/rxjs-interop'
import { NonNullableFormBuilder, ReactiveFormsModule } from '@angular/forms'
import { tuiMarkControlAsTouchedAndValidate } from '@taiga-ui/cdk'
import { tuiNumberFormatProvider, TuiTitle } from '@taiga-ui/core'
import { TuiHeader } from '@taiga-ui/layout'
import { startWith } from 'rxjs'
import { Footer } from 'src/app/components/footer'
import { Form } from 'src/app/directives/form'
import { Help } from 'src/app/directives/help'
import {
  injectFormService,
  provideFormService,
} from 'src/app/services/form.service'
import { PublishedPortsUciService } from 'src/app/routes/home/routes/published-ports/uci/service'
import { IPv6Aside } from './aside'
import { LanIpv6Form } from './form'
import { LanIpv6Service } from './service'
import { LanIpv6Summary } from './summary'
import { LanIpv6Data } from './uci/service'
import { getLanIpv6Form, updateLanIpv6Validators } from './utils'

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
      <lan-ipv6-form
        [enabled]="slaacEnabled()"
        [locked]="slaacLocked()"
        [lockedReason]="slaacLockedReason()"
      />
      @if (service.data()) {
        <footer appFooter></footer>
      }
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
    LanIpv6Summary,
    LanIpv6Form,
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
  private readonly publishedPortsUci = inject(PublishedPortsUciService)

  readonly form = getLanIpv6Form(this.builder)

  readonly slaacEnabled = toSignal(
    this.form.controls.strategy.controls.slaac.valueChanges.pipe(
      startWith(this.form.controls.strategy.controls.slaac.value),
    ),
    { requireSync: true },
  )

  readonly wanPrefix = computed(() => this.service.data()?.wanPrefix ?? 48)

  // Track if published ports use IPv6
  readonly hasIpv6Ports = signal(false)

  // SLAAC is locked if there are published ports using IPv6
  readonly slaacLocked = computed(() => this.hasIpv6Ports())

  readonly slaacLockedReason = computed(() => {
    if (!this.slaacLocked()) return null
    return 'Cannot disable: published ports using IPv6 exist'
  })

  constructor() {
    // Load IPv6 dependencies
    this.loadIpv6Dependencies()

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

  private async loadIpv6Dependencies() {
    const hasPorts = await this.publishedPortsUci.hasIpv6Ports()
    this.hasIpv6Ports.set(hasPorts)
  }

  async onSave() {
    if (this.form.invalid) {
      tuiMarkControlAsTouchedAndValidate(this.form)
    } else if (await this.service.save(this.form.getRawValue())) {
      this.form.markAsPristine()
    }
  }
}
