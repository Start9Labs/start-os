import { KeyValuePipe } from '@angular/common'
import { ChangeDetectionStrategy, Component, inject } from '@angular/core'
import { ReactiveFormsModule } from '@angular/forms'
import { TuiError, TuiTextfield, TuiTitle } from '@taiga-ui/core'
import { TuiRadio } from '@taiga-ui/kit'
import { TuiHeader } from '@taiga-ui/layout'
import { Form } from 'src/app/directives/form.directive'

import Ipv4 from '.'
import { LABELS } from './utils'

@Component({
  selector: 'form[ipv4Ip]',
  template: `
    <ng-container [formGroup]="parent.form.controls.ip">
      <header tuiHeader><h2 tuiTitle>IP Address</h2></header>
      <section>
        @for (mode of ['dhcp', 'static', 'pppoe']; track $index) {
          <label tuiLabel>
            <input
              type="radio"
              tuiRadio
              formControlName="mode"
              [value]="mode"
            />
            {{ labels[mode] }}{{ $index ? '' : ' (Default)' }}
          </label>
        }
      </section>
      @if (parent.ip === 'static') {
        <section [formGroupName]="parent.ip">
          @for (
            control of parent.form.controls.ip.controls[parent.ip]?.controls
              | keyvalue: asIs;
            track control
          ) {
            <div>
              <tui-textfield>
                <label tuiLabel>{{ labels[control.key] }}</label>
                <input
                  tuiTextfield
                  [formControlName]="control.key"
                  [readOnly]="control.key === 'mask'"
                />
              </tui-textfield>
              @if (control.key === 'gateway') {
                <tui-error
                  class="g-secondary"
                  error="Only needed if behind NAT"
                />
              }
            </div>
          }
        </section>
      }
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
      } @else {}
    </ng-container>
  `,
  hostDirectives: [Form],
  changeDetection: ChangeDetectionStrategy.OnPush,
  imports: [
    ReactiveFormsModule,
    KeyValuePipe,
    TuiHeader,
    TuiTitle,
    TuiTextfield,
    TuiRadio,
    TuiError,
  ],
})
export class Ipv4Ip {
  protected readonly parent = inject(Ipv4)
  protected readonly labels = LABELS
  protected readonly asIs = () => 0
}
