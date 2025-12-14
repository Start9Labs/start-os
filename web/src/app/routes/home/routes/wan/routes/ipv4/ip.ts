import { ChangeDetectionStrategy, Component, inject } from '@angular/core'
import { ReactiveFormsModule } from '@angular/forms'
import { TuiError, TuiTextfield, TuiTitle } from '@taiga-ui/core'
import { TuiRadio } from '@taiga-ui/kit'
import { TuiHeader } from '@taiga-ui/layout'
import { FORM, FormSection } from 'src/app/directives/form'

import Ipv4 from '.'
import { LABELS } from './utils'

@Component({
  selector: 'ipv4-ip',
  template: `
    <header tuiHeader="body-l"><h2 tuiTitle>IP Address</h2></header>
    <section>
      @for (mode of ['dhcp', 'static', 'pppoe']; track $index) {
        <label tuiLabel>
          <input type="radio" tuiRadio formControlName="mode" [value]="mode" />
          {{ labels[mode] }}{{ $index ? '' : ' (Default)' }}
        </label>
      }
    </section>
    @if (parent.ip === 'static') {
      <section>
        @for (control of static; track control) {
          <div>
            <tui-textfield>
              <label tuiLabel>{{ labels[control] }}</label>
              <input
                tuiTextfield
                [formControlName]="control"
                [readOnly]="control === 'mask'"
              />
            </tui-textfield>
            @if (control === 'gateway') {
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
      <section>
        @for (control of pppoe; track $index) {
          <tui-textfield>
            <label tuiLabel>{{ labels[control] }}</label>
            <input tuiTextfield [formControlName]="control" />
          </tui-textfield>
        }
      </section>
    }
  `,
  viewProviders: [FORM],
  hostDirectives: [FormSection],
  changeDetection: ChangeDetectionStrategy.OnPush,
  imports: [
    ReactiveFormsModule,
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
  protected readonly static = ['wan', 'prefix', 'mask', 'gateway']
  protected readonly pppoe = ['wan', 'password', 'vlan']
}
