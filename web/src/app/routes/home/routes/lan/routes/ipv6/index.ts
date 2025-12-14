import { ChangeDetectionStrategy, Component, inject } from '@angular/core'
import { NonNullableFormBuilder, ReactiveFormsModule } from '@angular/forms'
import {
  TuiAppearance,
  TuiButton,
  TuiError,
  TuiNumberFormat,
  TuiTextfield,
  TuiTitle,
} from '@taiga-ui/core'
import { TuiInputNumber, TuiSwitch } from '@taiga-ui/kit'
import { TuiCard, TuiForm, TuiHeader } from '@taiga-ui/layout'
import { Form } from 'src/app/directives/form'
import { Help } from 'src/app/directives/help'

import { IPv6Aside } from './aside'
import { Ipv6Summary } from './summary'

@Component({
  template: `
    <ipv6-aside *help />
    <article ipv6Summary [formLoading]="false"></article>
    <form
      tuiForm
      tuiCardLarge="compact"
      tuiAppearance="neutral"
      class="g-form"
      [formGroup]="form"
    >
      <header tuiHeader><h2 tuiTitle>Strategy</h2></header>
      <label tuiLabel>
        <input type="checkbox" tuiSwitch size="m" formControlName="slaac" />
        Enable SLAAC
      </label>
      <label tuiLabel>
        <input type="checkbox" tuiSwitch size="m" formControlName="dhcpv6" />
        Enable DHCPv6
      </label>
    </form>
    <form
      tuiForm
      tuiCardLarge="compact"
      tuiAppearance="neutral"
      class="g-form"
      [formGroup]="form"
    >
      <header tuiHeader><h2 tuiTitle>Subnet</h2></header>
      <fieldset>
        <div>
          <tui-textfield>
            <label tuiLabel>Router's IPv6 Address</label>
            <input tuiTextfield formControlName="ip" [readOnly]="true" />
          </tui-textfield>
        </div>
        <label tuiLabel>
          <tui-textfield>
            <label tuiLabel>IPv6 Prefix Length</label>
            <input
              tuiInputNumber
              formControlName="prefix"
              prefix="/"
              [min]="0"
              [max]="64"
              [tuiNumberFormat]="{ precision: 0 }"
            />
          </tui-textfield>
          <span class="g-secondary">Can only be smaller than WAN setting</span>
        </label>
      </fieldset>
    </form>
    <footer class="g-footer">
      <button tuiButton appearance="flat">Cancel</button>
      <button tuiButton>Save</button>
    </footer>
  `,
  host: { class: 'g-page' },
  imports: [
    TuiCard,
    TuiButton,
    TuiForm,
    TuiAppearance,
    TuiHeader,
    TuiTitle,
    TuiTextfield,
    TuiInputNumber,
    TuiNumberFormat,
    TuiSwitch,
    ReactiveFormsModule,
    Form,
    Help,
    Ipv6Summary,
    IPv6Aside,
  ],
  changeDetection: ChangeDetectionStrategy.OnPush,
})
export default class Ipv6 {
  public readonly form = inject(NonNullableFormBuilder).group({
    slaac: true,
    dhcpv6: true,
    ip: '3211:0:0:1234:5678:ABCD:EF12:1234',
    prefix: 64,
  })
}
