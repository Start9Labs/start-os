import {
  ChangeDetectionStrategy,
  Component,
  effect,
  inject,
} from '@angular/core'
import { toSignal } from '@angular/core/rxjs-interop'
import { NonNullableFormBuilder, ReactiveFormsModule } from '@angular/forms'
import { tuiMarkControlAsTouchedAndValidate } from '@taiga-ui/cdk'
import {
  TuiError,
  TuiInput,
  TuiLabel,
  TuiRadio,
  TuiTextfield,
  TuiTitle,
  tuiValidationErrorsProvider,
} from '@taiga-ui/core'
import { TuiSwitch } from '@taiga-ui/kit'
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
import { DnsAside } from './aside'
import { DnsService } from './service'
import {
  DnsForm,
  DNS_LABELS,
  DNS_MODES,
  DNS_VALIDATION_ERRORS,
  getDnsForm,
  updateDnsValidators,
} from './utils'

@Component({
  template: `
    <wan-dns-aside *help />
    <header tuiHeader="h6"><h2 tuiTitle>Settings</h2></header>
    <form
      [formGroup]="form"
      [formLoading]="!service.data()"
      (reset.prevent)="form.reset(service.data())"
      (ngSubmit)="onSave()"
    >
      <section>
        @for (mode of dnsModes; track $index) {
          <label tuiLabel>
            <input
              type="radio"
              tuiRadio
              formControlName="mode"
              [value]="mode"
            />
            {{ labels[mode] }}{{ $first ? ' (Default)' : '' }}
          </label>
        }
      </section>
      @if (mode() === 'custom') {
        <section>
          <tui-textfield>
            <label tuiLabel>Primary*</label>
            <input tuiInput formControlName="custom1" />
          </tui-textfield>
          <label tuiLabel>
            <input type="checkbox" tuiSwitch formControlName="custom1Tls" />
            TLS
          </label>
        </section>
        <tui-error formControlName="custom1" />
        <section>
          <tui-textfield>
            <label tuiLabel>Secondary</label>
            <input tuiInput formControlName="custom2" />
          </tui-textfield>
          <label tuiLabel>
            <input type="checkbox" tuiSwitch formControlName="custom2Tls" />
            TLS
          </label>
        </section>
        <tui-error formControlName="custom2" />
        <section>
          <tui-textfield>
            <label tuiLabel>Tertiary</label>
            <input tuiInput formControlName="custom3" />
          </tui-textfield>
          <label tuiLabel>
            <input type="checkbox" tuiSwitch formControlName="custom3Tls" />
            TLS
          </label>
        </section>
        <tui-error formControlName="custom3" />
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
    TuiTextfield,
    TuiError,
    TuiInput,
    TuiLabel,
    TuiRadio,
    TuiSwitch,
    Form,
    Footer,
    Help,
    DnsAside,
  ],
  providers: [
    provideFormService(DnsService),
    tuiValidationErrorsProvider(DNS_VALIDATION_ERRORS),
  ],
  changeDetection: ChangeDetectionStrategy.OnPush,
})
export default class Dns {
  protected readonly builder = inject(NonNullableFormBuilder)
  protected readonly service = injectFormService<DnsForm>()

  protected readonly labels = DNS_LABELS
  protected readonly dnsModes = DNS_MODES

  readonly form = getDnsForm(this.builder, [CustomValidators.ipv4()])

  readonly mode = toSignal(
    this.form.controls.mode.valueChanges.pipe(
      startWith(this.form.controls.mode.value),
    ),
    { requireSync: true },
  )

  constructor() {
    // Reset form when data loads
    effect(() => {
      const data = this.service.data()
      if (data && this.form.pristine) {
        this.form.reset(data)
        updateDnsValidators(this.form, data.mode, [CustomValidators.ipv4()])
      }
    })

    // Update validators when mode changes
    effect(() => {
      const mode = this.mode()
      if (mode) {
        updateDnsValidators(this.form, mode, [CustomValidators.ipv4()])
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
