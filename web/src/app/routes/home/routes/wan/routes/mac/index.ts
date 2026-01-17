import {
  ChangeDetectionStrategy,
  Component,
  effect,
  inject,
} from '@angular/core'
import { toSignal } from '@angular/core/rxjs-interop'
import { NonNullableFormBuilder, ReactiveFormsModule } from '@angular/forms'
import { tuiMarkControlAsTouchedAndValidate } from '@taiga-ui/cdk'
import { TuiLabel, TuiTitle } from '@taiga-ui/core'
import { TuiRadio } from '@taiga-ui/kit'
import { TuiHeader } from '@taiga-ui/layout'
import { startWith } from 'rxjs'
import { Footer } from 'src/app/components/footer'
import { Form } from 'src/app/directives/form'
import { Help } from 'src/app/directives/help'
import {
  injectFormService,
  provideFormService,
} from 'src/app/services/form.service'
import { MacAddress } from './form/address'
import { MacAside } from './aside'
import { MacService } from './service'
import { MacSummary } from './summary'
import {
  getMacForm,
  MacForm,
  MAC_LABELS,
  MAC_STRATEGIES,
  updateMacValidators,
} from './utils'

@Component({
  template: `
    <mac-aside *help />
    <header tuiHeader="h6"><h2 tuiTitle>Summary</h2></header>
    <article macSummary [formLoading]="!service.data()"></article>
    <header tuiHeader="h6"><h2 tuiTitle>Settings</h2></header>
    <form
      [formGroup]="form"
      [formLoading]="!service.data()"
      (reset.prevent)="form.reset(service.data())"
      (ngSubmit)="onSave()"
    >
      <section>
        @for (strategy of strategies; track strategy) {
          <label tuiLabel>
            <input
              type="radio"
              tuiRadio
              formControlName="strategy"
              [value]="strategy"
            />
            {{ labels[strategy] }}{{ $first ? ' (Default)' : '' }}
          </label>
        }
      </section>
      @if (strategy() === 'custom') {
        <hr />
        <mac-address formGroupName="address" />
      }
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
    TuiRadio,
    TuiLabel,
    Form,
    Footer,
    Help,
    MacSummary,
    MacAddress,
    MacAside,
  ],
  providers: [provideFormService(MacService)],
  changeDetection: ChangeDetectionStrategy.OnPush,
})
export default class Mac {
  protected readonly builder = inject(NonNullableFormBuilder)
  protected readonly service = injectFormService<MacForm>()

  readonly form = getMacForm(this.builder)

  protected readonly strategies = MAC_STRATEGIES
  protected readonly labels = MAC_LABELS

  readonly strategy = toSignal(
    this.form.controls.strategy.valueChanges.pipe(
      startWith(this.form.controls.strategy.value),
    ),
    { requireSync: true },
  )

  constructor() {
    // Reset form when data loads
    effect(() => {
      const data = this.service.data()
      if (data && this.form.pristine) {
        this.form.reset(data)
        updateMacValidators(this.form, data.strategy)
      }
    })

    // Update validators when strategy changes
    effect(() => {
      const strategy = this.strategy()
      if (strategy) {
        updateMacValidators(this.form, strategy)
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
