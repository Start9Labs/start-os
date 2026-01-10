import { AsyncPipe } from '@angular/common'
import { ChangeDetectionStrategy, Component, input } from '@angular/core'
import { ReactiveFormsModule } from '@angular/forms'
import { TuiError, TuiTextfield, TuiTitle } from '@taiga-ui/core'
import {
  TUI_VALIDATION_ERRORS,
  TuiDataListWrapper,
  TuiFieldErrorPipe,
  TuiRadio,
  TuiSelect,
  TuiSwitch,
} from '@taiga-ui/kit'
import { TuiHeader } from '@taiga-ui/layout'
import { FORM, FormSection } from 'src/app/directives/form'
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
            <input tuiTextfield formControlName="custom1" />
          </tui-textfield>
          <tui-error
            formControlName="custom1"
            [error]="[] | tuiFieldError | async"
          />
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
            <input tuiTextfield formControlName="custom2" />
          </tui-textfield>
          <tui-error
            formControlName="custom2"
            [error]="[] | tuiFieldError | async"
          />
        </div>
        <label tuiLabel>
          <input type="checkbox" tuiSwitch formControlName="custom2Tls" />
          TLS
        </label>
      </section>
    }
  `,
  viewProviders: [FORM],
  hostDirectives: [FormSection],
  providers: [
    {
      provide: TUI_VALIDATION_ERRORS,
      useValue: DNS_VALIDATION_ERRORS,
    },
  ],
  changeDetection: ChangeDetectionStrategy.OnPush,
  imports: [
    AsyncPipe,
    ReactiveFormsModule,
    TuiHeader,
    TuiTitle,
    TuiTextfield,
    TuiError,
    TuiFieldErrorPipe,
    TuiRadio,
    TuiDataListWrapper,
    TuiSelect,
    TuiSwitch,
  ],
})
export class Dns {
  protected readonly labels = LABELS
  protected readonly dnsModes = DNS_MODES

  readonly mode = input.required<DnsForm['mode']>()
}
