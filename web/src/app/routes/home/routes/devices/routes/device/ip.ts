import { AsyncPipe } from '@angular/common'
import { ChangeDetectionStrategy, Component, inject } from '@angular/core'
import { ReactiveFormsModule } from '@angular/forms'
import { TuiError, TuiTextfield, TuiTitle } from '@taiga-ui/core'
import {
  TUI_VALIDATION_ERRORS,
  TuiFieldErrorPipe,
  TuiSwitch,
} from '@taiga-ui/kit'
import { TuiHeader } from '@taiga-ui/layout'
import { FORM, FormSection } from 'src/app/directives/form'
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
            tuiTextfield
            formControlName="ipv4"
            [readOnly]="!parent.ipv4Static()"
          />
        </tui-textfield>
        <tui-error
          formControlName="ipv4"
          [error]="[] | tuiFieldError | async"
        />
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
            tuiTextfield
            formControlName="ipv6"
            [readOnly]="!parent.ipv6Static()"
          />
        </tui-textfield>
        <tui-error
          formControlName="ipv6"
          [error]="[] | tuiFieldError | async"
        />
      </div>
      <label tuiLabel>
        <input tuiSwitch type="checkbox" formControlName="ipv6Static" />
        Use static
      </label>
    </section>
  `,
  viewProviders: [FORM],
  hostDirectives: [FormSection],
  providers: [
    {
      provide: TUI_VALIDATION_ERRORS,
      useValue: DEVICE_VALIDATION_ERRORS,
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
    TuiSwitch,
  ],
})
export class DeviceIp {
  protected readonly parent = inject(DeviceDetail)
}
