import {
  ChangeDetectionStrategy,
  Component,
  inject,
  input,
} from '@angular/core'
import { ReactiveFormsModule } from '@angular/forms'
import {
  TuiError,
  TuiHint,
  TuiInput,
  TuiTextfield,
  TuiTitle,
  tuiValidationErrorsProvider,
} from '@taiga-ui/core'
import { TuiSwitch } from '@taiga-ui/kit'
import { TuiCardLarge, TuiForm, TuiHeader } from '@taiga-ui/layout'
import { FORM } from 'src/app/components/form'
import { DEVICE_VALIDATION_ERRORS } from 'src/app/routes/devices/utils'

import DeviceDetail from '.'

@Component({
  selector: 'device-ip',
  template: `
    <header tuiHeader="body-l"><h2 tuiTitle>IP Address</h2></header>
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
      <label
        tuiLabel
        [class.locked]="ipv4Locked() && parent.ipv4Static()"
        [tuiHint]="
          ipv4Locked() && parent.ipv4Static()
            ? 'Required by a published port rule'
            : null
        "
        tuiHintAppearance="error"
      >
        <input
          tuiSwitch
          type="checkbox"
          formControlName="ipv4Static"
          (click)="onToggleClick($event, ipv4Locked() && parent.ipv4Static())"
        />
        Reserve
      </label>
    </section>
  `,
  styles: `
    label.locked {
      opacity: 0.6;
      cursor: not-allowed;

      input {
        pointer-events: none;
      }
    }
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
    TuiHint,
  ],
})
export class DeviceIp {
  protected readonly parent = inject(DeviceDetail)

  readonly ipv4Locked = input(false)

  onToggleClick(event: Event, locked: boolean) {
    if (locked) {
      event.preventDefault()
      event.stopPropagation()
    }
  }
}
