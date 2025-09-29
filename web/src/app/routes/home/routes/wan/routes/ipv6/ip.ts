import { KeyValuePipe } from '@angular/common'
import { ChangeDetectionStrategy, Component, inject } from '@angular/core'
import { ReactiveFormsModule } from '@angular/forms'
import { TuiAppearance, TuiError, TuiTextfield, TuiTitle } from '@taiga-ui/core'
import { TuiRadio } from '@taiga-ui/kit'
import { TuiCard, TuiForm, TuiHeader } from '@taiga-ui/layout'

import Ipv6 from '.'

@Component({
  selector: 'ipv6-ip',
  template: `
    <form
      tuiForm
      tuiCardLarge="compact"
      tuiAppearance="neutral"
      class="g-form"
      [formGroup]="parent.form.controls.ip"
    >
      <header tuiHeader><h2 tuiTitle>IP Address</h2></header>
      <section>
        @for (mode of modes; track $index) {
          <label tuiLabel>
            <input
              type="radio"
              tuiRadio
              formControlName="mode"
              [value]="mode"
            />
            {{ mode }}{{ $index ? '' : ' (Default)' }}
          </label>
        }
      </section>
      @if (parent.ip !== 'disabled') {
        <section [formGroupName]="parent.ip">
          @for (
            control of parent.form.controls.ip.controls[parent.ip].controls
              | keyvalue: asIs;
            track control
          ) {
            <div>
              <tui-textfield>
                <label tuiLabel>{{ labels[control.key] }}</label>
                <input
                  tuiTextfield
                  [formControlName]="control.key"
                  [readOnly]="isReadOnly(control.key)"
                />
              </tui-textfield>
              @if (control.key === 'gateway' && parent.ip === 'static') {
                <tui-error
                  class="g-secondary"
                  error="Only needed if behind NAT"
                />
              }
            </div>
          }
        </section>
      }
    </form>
  `,
  changeDetection: ChangeDetectionStrategy.OnPush,
  imports: [
    ReactiveFormsModule,
    KeyValuePipe,
    TuiForm,
    TuiAppearance,
    TuiHeader,
    TuiTitle,
    TuiTextfield,
    TuiRadio,
    TuiCard,
    TuiError,
  ],
})
export class Ipv6Ip {
  protected readonly parent = inject(Ipv6)
  protected readonly modes = ['SLAAC', 'DHCPv6', 'Static', '6RD', 'Disabled']
  protected readonly asIs = () => 0
  protected readonly labels: Record<string, string> = {
    wan: 'WAN IPv6 Address',
    length: 'IPv6 Prefix Length*',
    prefix: 'IPv6 Prefix*',
    gateway: 'Gateway IPv6 Address',
    ip4: 'IPv4 Gateway Address*',
    mask: 'IPv4 Mask Length*',
    border: 'IPv4 Border Router Address*',
  }

  protected isReadOnly(key: string): boolean {
    return (
      this.parent.ip !== 'static' &&
      (key === 'wan' || key === 'gateway' || this.parent.ip === 'dhcpv6')
    )
  }
}
