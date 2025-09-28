import { KeyValuePipe } from '@angular/common'
import { ChangeDetectionStrategy, Component, inject } from '@angular/core'
import { ReactiveFormsModule } from '@angular/forms'
import { TuiAppearance, TuiError, TuiTextfield, TuiTitle } from '@taiga-ui/core'
import { TuiRadio } from '@taiga-ui/kit'
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
      <header tuiHeader><h2 tuiTitle>IP Address</h2></header>
      <section>
        @for (mode of ['DHCP', 'Static', 'PPPoE']; track $index) {
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
      @if (parent.ip === 'pppoe') {
        <section [formGroupName]="parent.ip">
          @for (
            control of parent.form.controls.ip.controls[parent.ip].controls
              | keyvalue: asIs;
            track $index
          ) {
            <tui-textfield>
              <label tuiLabel>{{ labels[control.key] }}</label>
              <input tuiTextfield [formControlName]="control.key" />
            </tui-textfield>
          }
        </section>
      } @else {
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
                  [readOnly]="control.key === 'mask' || parent.ip === 'dhcp'"
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
export class Ipv4Ip {
  protected readonly parent = inject(Ipv4)
  protected readonly asIs = () => 0
  protected readonly labels: Record<string, string> = {
    dhcp: 'DHCP (Default)',
    static: 'Static',
    pppoe: 'PPPoE',
    wan: 'WAN IP Address',
    prefix: 'Subnet Prefix',
    mask: 'Subnet Mask',
    gateway: 'Gateway IP Address',
    password: 'Password*',
    vlan: 'VLAN ID',
  }
}
