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
    <section [class.disabled-section]="!ipv6Enabled()">
      <div>
        <tui-textfield>
          <label tuiLabel>
            IPv6{{ ipv6Enabled() && parent.ipv6Static() ? '*' : '' }}
          </label>
          @if (ipv6Enabled()) {
            <input
              tuiInput
              formControlName="ipv6"
              [readOnly]="!parent.ipv6Static()"
            />
          } @else {
            <input tuiInput value="disabled" disabled />
          }
        </tui-textfield>
        <tui-error formControlName="ipv6" />
      </div>
      <label
        tuiLabel
        [class.locked]="!ipv6Enabled() || (ipv6Locked() && parent.ipv6Static())"
        [tuiHint]="getIpv6Hint()"
        tuiHintAppearance="error"
      >
        <input
          tuiSwitch
          type="checkbox"
          [formControlName]="ipv6Enabled() ? 'ipv6Static' : ''"
          [checked]="ipv6Enabled() ? undefined : false"
          [disabled]="!ipv6Enabled()"
          (click)="onIpv6ToggleClick($event)"
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

    .disabled-section {
      opacity: 0.6;
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
  readonly ipv6Locked = input(false)
  readonly ipv6Enabled = input(true)

  onToggleClick(event: Event, locked: boolean) {
    if (locked) {
      event.preventDefault()
      event.stopPropagation()
    }
  }

  onIpv6ToggleClick(event: Event) {
    if (
      !this.ipv6Enabled() ||
      (this.ipv6Locked() && this.parent.ipv6Static())
    ) {
      event.preventDefault()
      event.stopPropagation()
    }
  }

  getIpv6Hint(): string | null {
    if (!this.ipv6Enabled()) {
      return 'Enable LAN IPv6 to reserve addresses'
    }
    if (this.ipv6Locked() && this.parent.ipv6Static()) {
      return 'Required by a published port rule'
    }
    return null
  }
}
