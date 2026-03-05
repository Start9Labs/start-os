import {
  ChangeDetectionStrategy,
  Component,
  effect,
  inject,
} from '@angular/core'
import { toSignal } from '@angular/core/rxjs-interop'
import { NonNullableFormBuilder, ReactiveFormsModule } from '@angular/forms'
import { MaskitoDirective } from '@maskito/angular'
import { TuiAnimated, tuiMarkControlAsTouchedAndValidate } from '@taiga-ui/cdk'
import {
  TuiError,
  TuiHint,
  TuiInput,
  TuiLabel,
  tuiNumberFormatProvider,
  TuiTitle,
} from '@taiga-ui/core'
import { TuiSwitch } from '@taiga-ui/kit'
import { TuiElasticContainer, TuiHeader } from '@taiga-ui/layout'
import { startWith } from 'rxjs'
import { Footer } from 'src/app/components/footer'
import { Form } from 'src/app/components/form'
import { PublishedPortsUciService } from 'src/app/routes/published-ports/uci/service'
import {
  injectFormService,
  provideFormService,
} from 'src/app/services/form.service'
import { PREFIX } from 'src/app/utils/masks'

import { LanIpv6Service } from './service'
import { LanIpv6Summary } from './summary'
import { LanIpv6Data } from './service'
import { getLanIpv6Form, updateLanIpv6Validators } from './utils'

@Component({
  template: `
    <header tuiHeader="h6"><h2 tuiTitle>Summary</h2></header>
    <article lanIpv6Summary [formLoading]="!service.data()"></article>
    <header tuiHeader="h6"><h2 tuiTitle>Settings</h2></header>
    <form
      [formGroup]="form"
      [formLoading]="!service.data()"
      (reset.prevent)="form.reset(service.data())"
      (ngSubmit)="onSave()"
    >
      <ng-container formGroupName="strategy">
        <label tuiLabel>
          <input type="checkbox" tuiSwitch formControlName="slaac" />
          Enable
          <i tuiHint="Cannot disable: published ports using IPv6 exist"></i>
        </label>
      </ng-container>
      <tui-elastic-container formGroupName="subnet">
        @if (slaacEnabled()) {
          <tui-textfield tuiAnimated>
            <label tuiLabel>Prefix Length</label>
            <input tuiInput formControlName="prefix" [maskito]="mask" />
          </tui-textfield>
          <tui-error formControlName="prefix" />
        }
      </tui-elastic-container>
      @if (service.data()) {
        <footer appFooter></footer>
      }
    </form>
  `,
  styles: `
    tui-textfield {
      max-width: 10rem;
    }
  `,
  host: { class: 'g-page' },
  imports: [
    ReactiveFormsModule,
    TuiHeader,
    TuiTitle,
    Footer,
    Form,
    LanIpv6Summary,
    TuiLabel,
    TuiSwitch,
    TuiHint,
    TuiError,
    TuiInput,
    TuiElasticContainer,
    MaskitoDirective,
    TuiAnimated,
  ],
  providers: [
    provideFormService(LanIpv6Service),
    tuiNumberFormatProvider({ precision: 0 }),
  ],
  changeDetection: ChangeDetectionStrategy.OnPush,
})
export default class LanIpv6 {
  private readonly builder = inject(NonNullableFormBuilder)
  private readonly publishedPortsUci = inject(PublishedPortsUciService)

  protected readonly service = injectFormService<LanIpv6Data>()
  protected readonly form = getLanIpv6Form(this.builder)
  protected readonly mask = PREFIX
  protected readonly slaacEnabled = toSignal(
    this.form.controls.strategy.controls.slaac.valueChanges.pipe(
      startWith(this.form.controls.strategy.controls.slaac.value),
    ),
    { requireSync: true },
  )

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
      updateLanIpv6Validators(
        this.form,
        this.slaacEnabled(),
        this.service.data()?.wanPrefix ?? 48,
      )
    })
  }

  private async loadIpv6Dependencies() {
    if ((await this.publishedPortsUci.hasIpv6Ports()) && this.slaacEnabled()) {
      this.form.controls.strategy.controls.slaac.disable()
    }
  }

  async onSave() {
    if (this.form.invalid) {
      tuiMarkControlAsTouchedAndValidate(this.form)
    } else if (await this.service.save(this.form.getRawValue())) {
      this.form.markAsPristine()
    }
  }
}
