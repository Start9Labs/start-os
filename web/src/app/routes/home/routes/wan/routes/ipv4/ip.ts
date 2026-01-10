import { AsyncPipe } from '@angular/common'
import {
  ChangeDetectionStrategy,
  Component,
  computed,
  inject,
} from '@angular/core'
import { toSignal } from '@angular/core/rxjs-interop'
import { ReactiveFormsModule } from '@angular/forms'
import { MaskitoDirective } from '@maskito/angular'
import { MaskitoOptions } from '@maskito/core'
import { TuiError, TuiTextfield, TuiTitle } from '@taiga-ui/core'
import {
  TUI_VALIDATION_ERRORS,
  TuiFieldErrorPipe,
  TuiRadio,
} from '@taiga-ui/kit'
import { TuiHeader } from '@taiga-ui/layout'
import { FORM, FormSection } from 'src/app/directives/form'
import {
  IPV4_LABELS,
  IPV4_MODES,
  IPV4_PPPOE_CONTROLS,
  IPV4_STATIC_CONTROLS,
  IPV4_VALIDATION_ERRORS,
  netmaskFromPrefix,
} from './utils'
import Ipv4 from '.'

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

    @if (parent.ipMode() === 'static') {
      <section>
        @for (control of staticControls; track control) {
          <div>
            <tui-textfield>
              <label tuiLabel>{{ labels[control] }}*</label>
              <input
                tuiTextfield
                [formControlName]="control"
                [maskito]="control === 'prefix' ? prefixMask : null"
              />
            </tui-textfield>
            <tui-error
              [formControlName]="control"
              [error]="[] | tuiFieldError | async"
            />
            @if (control === 'prefix' && netmask()) {
              <tui-error class="g-secondary" [error]="'Subnet: ' + netmask()" />
            }
          </div>
        }
      </section>
    }
    @if (parent.ipMode() === 'pppoe') {
      <section>
        @for (control of pppoeControls; track control) {
          <div>
            <tui-textfield>
              <label tuiLabel>
                {{ labels[control] }}{{ control !== 'device' ? '*' : '' }}
              </label>
              <input
                tuiTextfield
                [formControlName]="control"
                [type]="control === 'password' ? 'password' : 'text'"
              />
            </tui-textfield>
            <tui-error
              [formControlName]="control"
              [error]="[] | tuiFieldError | async"
            />
          </div>
        }
      </section>
    }
  `,
  viewProviders: [FORM],
  hostDirectives: [FormSection],
  providers: [
    {
      provide: TUI_VALIDATION_ERRORS,
      useValue: IPV4_VALIDATION_ERRORS,
    },
  ],
  changeDetection: ChangeDetectionStrategy.OnPush,
  imports: [
    AsyncPipe,
    ReactiveFormsModule,
    TuiHeader,
    TuiTitle,
    TuiTextfield,
    TuiRadio,
    TuiError,
    TuiFieldErrorPipe,
    MaskitoDirective,
  ],
})
export class Ipv4Ip {
  protected readonly parent = inject(Ipv4)

  protected readonly modes = IPV4_MODES
  protected readonly staticControls = IPV4_STATIC_CONTROLS
  protected readonly pppoeControls = IPV4_PPPOE_CONTROLS
  protected readonly labels = IPV4_LABELS

  protected readonly prefixMask: MaskitoOptions = {
    mask: ['/', /\d/, /\d/],
  }

  readonly prefix = toSignal(
    this.parent.form.controls.ip.controls.prefix.valueChanges,
  )

  readonly netmask = computed(() => netmaskFromPrefix(this.prefix() || ''))
}
