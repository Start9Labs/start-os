import { ChangeDetectionStrategy, Component, inject } from '@angular/core'
import { ReactiveFormsModule } from '@angular/forms'
import {
  TuiError,
  TuiInput,
  TuiTextfield,
  TuiTitle,
  tuiValidationErrorsProvider,
} from '@taiga-ui/core'
import { TuiSwitch } from '@taiga-ui/kit'
import { TuiCardLarge, TuiForm, TuiHeader } from '@taiga-ui/layout'
import { FORM } from 'src/app/directives/form'
import { DEVICE_VALIDATION_ERRORS } from 'src/app/routes/home/routes/devices/utils'

import DeviceDetail from '.'

@Component({
  selector: 'device-ip',
  template: `
    <header tuiHeader="body-l"><h2 tuiTitle>IP Addresses</h2></header>
    <section>
      <div>
        <tui-textfield>
          <label tuiLabel>IPv4{{ parent.ipv4Static() ? '*' : '' }}</label>
          <input
            tuiInput
            formControlName="ipv4"
            [readOnly]="!parent.ipv4Static()"
          />
        </tui-textfield>
        <tui-error formControlName="ipv4" />
      </div>
      <label tuiLabel>
        <input tuiSwitch type="checkbox" formControlName="ipv4Static" />
        Use static
      </label>
    </section>
    <section>
      <div>
        <tui-textfield>
          <label tuiLabel>IPv6{{ parent.ipv6Static() ? '*' : '' }}</label>
          <input
            tuiInput
            formControlName="ipv6"
            [readOnly]="!parent.ipv6Static()"
          />
        </tui-textfield>
        <tui-error formControlName="ipv6" />
      </div>
      <label tuiLabel>
        <input tuiSwitch type="checkbox" formControlName="ipv6Static" />
        Use static
      </label>
    </section>
  `,
  viewProviders: [FORM],
  hostDirectives: [TuiForm, TuiCardLarge],
  providers: [tuiValidationErrorsProvider(DEVICE_VALIDATION_ERRORS)],
  changeDetection: ChangeDetectionStrategy.OnPush,
  imports: [
    ReactiveFormsModule,
    TuiHeader,
    TuiTitle,
    TuiTextfield,
    TuiInput,
    TuiError,
    TuiSwitch,
  ],
})
export class DeviceIp {
  protected readonly parent = inject(DeviceDetail)
}
