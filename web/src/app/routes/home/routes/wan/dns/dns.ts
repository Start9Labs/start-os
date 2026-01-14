import { ChangeDetectionStrategy, Component, input } from '@angular/core'
import { ReactiveFormsModule } from '@angular/forms'
import {
  TuiError,
  TuiInput,
  TuiTextfield,
  TuiTitle,
  tuiValidationErrorsProvider,
} from '@taiga-ui/core'
import { TuiRadio, TuiSwitch } from '@taiga-ui/kit'
import { TuiCardLarge, TuiForm, TuiHeader } from '@taiga-ui/layout'
import { FORM } from 'src/app/directives/form'
import { LABELS, DNS_MODES, DNS_VALIDATION_ERRORS, DnsForm } from './utils'

@Component({
  selector: 'wan-dns',
  template: `
    <header tuiHeader="body-l"><h2 tuiTitle>DNS</h2></header>
    <section>
      @for (mode of dnsModes; track $index) {
        <label tuiLabel>
          <input type="radio" tuiRadio formControlName="mode" [value]="mode" />
          {{ labels[mode] }}{{ $index ? '' : ' (Default)' }}
        </label>
      }
    </section>
    @if (mode() === 'custom') {
      <section>
        <div>
          <tui-textfield>
            <label tuiLabel>Primary*</label>
            <input tuiInput formControlName="custom1" />
          </tui-textfield>
          <tui-error formControlName="custom1" />
        </div>
        <label tuiLabel>
          <input type="checkbox" tuiSwitch formControlName="custom1Tls" />
          TLS
        </label>
      </section>
      <section>
        <div>
          <tui-textfield>
            <label tuiLabel>Secondary</label>
            <input tuiInput formControlName="custom2" />
          </tui-textfield>
          <tui-error formControlName="custom2" />
        </div>
        <label tuiLabel>
          <input type="checkbox" tuiSwitch formControlName="custom2Tls" />
          TLS
        </label>
      </section>
    }
  `,
  viewProviders: [FORM],
  hostDirectives: [TuiForm, TuiCardLarge],
  providers: [tuiValidationErrorsProvider(DNS_VALIDATION_ERRORS)],
  changeDetection: ChangeDetectionStrategy.OnPush,
  imports: [
    ReactiveFormsModule,
    TuiHeader,
    TuiTitle,
    TuiTextfield,
    TuiError,
    TuiInput,
    TuiRadio,
    TuiSwitch,
  ],
})
export class Dns {
  protected readonly labels = LABELS
  protected readonly dnsModes = DNS_MODES

  readonly mode = input.required<DnsForm['mode']>()
}
