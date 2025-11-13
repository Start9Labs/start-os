import { ChangeDetectionStrategy, Component, inject } from '@angular/core'
import { ReactiveFormsModule } from '@angular/forms'
import { TuiAppearance, TuiGroup, TuiTextfield, TuiTitle } from '@taiga-ui/core'
import { TuiInputNumber } from '@taiga-ui/kit'
import { TuiCard, TuiForm, TuiHeader } from '@taiga-ui/layout'

import Ipv4 from '.'

@Component({
  selector: 'ipv4-ip',
  template: `
    <form
      tuiForm
      tuiCardLarge="compact"
      tuiAppearance="neutral"
      class="g-form"
      [formGroup]="parent.form.controls.ip"
    >
      <header tuiHeader><h2 tuiTitle>IP Addresses</h2></header>
      <fieldset [style.grid-template-columns]="'repeat(auto-fit, 15rem)'">
        <label tuiLabel>
          Range
          <div tuiGroup>
            <tui-textfield>
              <input tuiInputNumber formControlName="range" />
            </tui-textfield>
            <tui-textfield>
              <input tuiTextfield value="168" disabled />
            </tui-textfield>
            <tui-textfield>
              <input tuiTextfield value="x" disabled />
            </tui-textfield>
            <tui-textfield>
              <input tuiTextfield value="x" disabled />
            </tui-textfield>
          </div>
        </label>
        <label tuiLabel>
          Router's IP
          <div tuiGroup>
            <tui-textfield>
              <input tuiTextfield value="192" disabled />
            </tui-textfield>
            <tui-textfield>
              <input tuiTextfield value="168" disabled />
            </tui-textfield>
            <tui-textfield>
              <input tuiTextfield value="0" disabled />
            </tui-textfield>
            <tui-textfield>
              <input tuiInputNumber formControlName="router" />
            </tui-textfield>
          </div>
        </label>
      </fieldset>
    </form>
  `,
  changeDetection: ChangeDetectionStrategy.OnPush,
  imports: [
    ReactiveFormsModule,
    TuiForm,
    TuiAppearance,
    TuiHeader,
    TuiTitle,
    TuiTextfield,
    TuiCard,
    TuiGroup,
    TuiInputNumber,
  ],
})
export class Ipv4Ip {
  protected readonly parent = inject(Ipv4)
}
