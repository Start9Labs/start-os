import {
  ChangeDetectionStrategy,
  Component,
  computed,
  inject,
  input,
} from '@angular/core'
import { ReactiveFormsModule } from '@angular/forms'
import { TuiError, TuiTextfield, TuiTitle } from '@taiga-ui/core'
import { TuiRadio } from '@taiga-ui/kit'
import { TuiHeader } from '@taiga-ui/layout'
import { FORM, FormSection } from 'src/app/directives/form'
import {
  IPV4_LABELS,
  IPV4_MODES,
  IPV4_PPPOE_CONTROLS,
  IPV4_STATIC_CONTROLS,
  netmaskFromPrefix,
} from './utils'
import Ipv4 from '.'
import { toSignal } from '@angular/core/rxjs-interop'

@Component({
  selector: 'ipv4-ip',
  template: `
    <header tuiHeader="body-l"><h2 tuiTitle>IP Address</h2></header>
    <section>
      @for (mode of modes; track $index) {
        <label tuiLabel>
          <input type="radio" tuiRadio formControlName="mode" [value]="mode" />
          {{ labels[mode] }}{{ $index ? '' : ' (Default)' }}
        </label>
      }
    </section>

    @if (parent.ipMode === 'static') {
      <section>
        @for (control of staticControls; track control) {
          <div>
            <tui-textfield>
              <label tuiLabel>{{ labels[control] }}</label>
              <input tuiTextfield [formControlName]="control" />
            </tui-textfield>
            @if (control === 'prefix') {
              <tui-error class="g-secondary" [error]="netmask()" />
            }
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
    @if (parent.ipMode === 'pppoe') {
      <section>
        @for (control of pppoeControls; track control) {
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

  protected readonly modes = IPV4_MODES
  protected readonly staticControls = IPV4_STATIC_CONTROLS
  protected readonly pppoeControls = IPV4_PPPOE_CONTROLS
  protected readonly labels = IPV4_LABELS

  readonly prefix = toSignal(
    this.parent.form.controls.ip.controls.prefix.valueChanges,
  )

  readonly netmask = computed(
    () => `Subnet: ${netmaskFromPrefix(this.prefix() || '')}`,
  )
}
