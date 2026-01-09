import { ChangeDetectionStrategy, Component, input } from '@angular/core'
import { ReactiveFormsModule } from '@angular/forms'
import { TuiTextfield, TuiTitle } from '@taiga-ui/core'
import {
  TuiDataListWrapper,
  TuiRadio,
  TuiSelect,
  TuiSwitch,
} from '@taiga-ui/kit'
import { TuiHeader } from '@taiga-ui/layout'
import { FORM, FormSection } from 'src/app/directives/form'
import { LABELS, DNS_MODES, DnsForm } from './utils'

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
        <tui-textfield>
          <label tuiLabel>Priority 1*</label>
          <input tuiTextfield formControlName="custom1" />
        </tui-textfield>
        <label tuiLabel>
          <input type="checkbox" tuiSwitch formControlName="custom1Tls" />
          TLS
        </label>
      </section>
      <section>
        <tui-textfield>
          <label tuiLabel>Priority 2</label>
          <input tuiTextfield formControlName="custom2" />
        </tui-textfield>
        <label tuiLabel>
          <input type="checkbox" tuiSwitch formControlName="custom2Tls" />
          TLS
        </label>
      </section>
    }
  `,
  viewProviders: [FORM],
  hostDirectives: [FormSection],
  changeDetection: ChangeDetectionStrategy.OnPush,
  imports: [
    ReactiveFormsModule,
    TuiHeader,
    TuiTitle,
    TuiTextfield,
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
