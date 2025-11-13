import { ChangeDetectionStrategy, Component, inject } from '@angular/core'
import { ReactiveFormsModule } from '@angular/forms'
import {
  TuiAppearance,
  TuiGroup,
  TuiNumberFormat,
  TuiTextfield,
  TuiTitle,
} from '@taiga-ui/core'
import { TuiInputNumber, TuiRadio } from '@taiga-ui/kit'
import { TuiCard, TuiForm, TuiHeader } from '@taiga-ui/layout'

import Ipv4 from '.'

@Component({
  selector: 'ipv4-dhcp',
  template: `
    <form
      tuiForm
      tuiCardLarge="compact"
      tuiAppearance="neutral"
      class="g-form"
      [formGroup]="parent.form.controls.dhcp"
    >
      <header tuiHeader><h2 tuiTitle>DHCP</h2></header>
      <section>
        @for (mode of ['server', 'relay', 'disabled']; track $index) {
          <label tuiLabel>
            <input
              type="radio"
              tuiRadio
              formControlName="mode"
              [value]="mode"
            />
            {{ $last ? '' : 'DHCP ' }}{{ mode }}{{ $index ? '' : ' (Default)' }}
          </label>
        }
      </section>
      <fieldset [style.grid-template-columns]="'repeat(auto-fit, 15rem)'">
        <label tuiLabel>
          Starting IP Address
          <div tuiGroup>
            <tui-textfield>
              <input tuiTextfield value="192" disabled />
            </tui-textfield>
            <tui-textfield>
              <input tuiTextfield value="168" disabled />
            </tui-textfield>
            <tui-textfield>
              <input tuiTextfield value="x" disabled />
            </tui-textfield>
            <tui-textfield>
              <input tuiInputNumber formControlName="start" />
            </tui-textfield>
          </div>
        </label>
        <label tuiLabel>
          Ending IP Address
          <div tuiGroup>
            <tui-textfield>
              <input tuiTextfield value="192" disabled />
            </tui-textfield>
            <tui-textfield>
              <input tuiTextfield value="168" disabled />
            </tui-textfield>
            <tui-textfield>
              <input tuiTextfield value="x" disabled />
            </tui-textfield>
            <tui-textfield>
              <input tuiInputNumber formControlName="end" />
            </tui-textfield>
          </div>
        </label>
      </fieldset>
    </form>
  `,
  styles: `
    label {
      text-transform: capitalize;
    }
  `,
  changeDetection: ChangeDetectionStrategy.OnPush,
  imports: [
    ReactiveFormsModule,
    TuiForm,
    TuiAppearance,
    TuiHeader,
    TuiTitle,
    TuiTextfield,
    TuiRadio,
    TuiCard,
    TuiGroup,
    TuiInputNumber,
  ],
})
export class Ipv4Dhcp {
  protected readonly parent = inject(Ipv4)
}
