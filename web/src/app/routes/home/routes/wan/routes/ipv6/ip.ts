import { ChangeDetectionStrategy, Component, input } from '@angular/core'
import { ReactiveFormsModule } from '@angular/forms'
import { TuiError, TuiTextfield, TuiTitle } from '@taiga-ui/core'
import { TuiRadio } from '@taiga-ui/kit'
import { TuiHeader } from '@taiga-ui/layout'
import { FORM, FormSection } from 'src/app/directives/form'
import {
  IPV6_MODES,
  Ipv6Mode,
  IPV6_LABELS,
  IPV6_SIXRD_CONTROLS,
  IPV6_SLAAC_CONTROLS,
  IPV6_STATIC_CONTROLS,
} from './utils'

@Component({
  selector: 'ipv6-ip',
  template: `
    <header tuiHeader="body-l"><h2 tuiTitle>IP Address</h2></header>
    <section>
      @for (mode of modes; track $index) {
        <label tuiLabel>
          <input type="radio" tuiRadio formControlName="mode" [value]="mode" />
          {{ mode }}{{ $index ? '' : ' (Default)' }}
        </label>
      }
    </section>

    @if (mode() === 'slaac') {
      <section>
        @for (control of slaacControls; track control) {
          <div>
            <tui-textfield>
              <label tuiLabel>{{ labels[control] }}</label>
              <input tuiTextfield [formControlName]="control" />
            </tui-textfield>
          </div>
        }
      </section>
    }

    @if (mode() === 'static') {
      <section>
        @for (control of staticControls; track control) {
          <div>
            <tui-textfield>
              <label tuiLabel>{{ labels[control] }}</label>
              <input tuiTextfield [formControlName]="control" />
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

    @if (mode() === '6rd') {
      <section>
        @for (control of sixrdControls; track control) {
          <div>
            <tui-textfield>
              <label tuiLabel>{{ labels[control] }}</label>
              <input tuiTextfield [formControlName]="control" />
            </tui-textfield>
          </div>
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
export class Ipv6Ip {
  protected readonly modes = IPV6_MODES
  protected readonly labels = IPV6_LABELS
  protected readonly slaacControls = IPV6_SLAAC_CONTROLS
  protected readonly staticControls = IPV6_STATIC_CONTROLS
  protected readonly sixrdControls = IPV6_SIXRD_CONTROLS

  readonly mode = input.required<Ipv6Mode>()
}
