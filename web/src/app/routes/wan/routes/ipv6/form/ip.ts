import {
  ChangeDetectionStrategy,
  Component,
  inject,
  input,
} from '@angular/core'
import { ReactiveFormsModule } from '@angular/forms'
import { MaskitoDirective } from '@maskito/angular'
import { MaskitoOptions } from '@maskito/core'
import {
  TuiError,
  TuiHint,
  TuiInput,
  TuiRadio,
  TuiTextfield,
  TuiTitle,
  tuiValidationErrorsProvider,
} from '@taiga-ui/core'
import { TuiCardLarge, TuiForm, TuiHeader } from '@taiga-ui/layout'
import { FORM } from 'src/app/components/form'
import {
  IPV6_MODES,
  IPV6_LABELS,
  IPV6_DHCPV6_CONTROLS,
  IPV6_SIXRD_CONTROLS,
  IPV6_SLAAC_CONTROLS,
  IPV6_STATIC_CONTROLS,
  IPV6_VALIDATION_ERRORS,
} from '../utils'
import WanIpv6 from '../'

@Component({
  selector: 'wan-ipv6-ip',
  template: `
    <header tuiHeader="body-l"><h2 tuiTitle>IP Address</h2></header>
    <section>
      @for (mode of modes; track $index) {
        <label
          tuiLabel
          [class.locked]="
            mode === 'disabled' &&
            disabledLocked() &&
            parent.ipMode() !== 'disabled'
          "
          [tuiHint]="
            mode === 'disabled' &&
            disabledLocked() &&
            parent.ipMode() !== 'disabled'
              ? 'Published ports are using IPv6'
              : null
          "
          tuiHintAppearance="error"
        >
          <input
            type="radio"
            tuiRadio
            formControlName="mode"
            [value]="mode"
            (click)="onModeClick($event, mode)"
          />
          {{ labels[mode] }}{{ $index ? '' : ' (Default)' }}
        </label>
      }
    </section>

    @if (parent.ipMode() === 'slaac') {
      <section>
        @for (control of slaacControls; track control) {
          <div>
            <tui-textfield>
              <label tuiLabel>{{ labels[control] }}</label>
              <input
                tuiInput
                [formControlName]="control"
                [maskito]="control === 'prefix' ? prefixMask : null"
                [placeholder]="control === 'prefix' ? 'Auto' : ''"
              />
            </tui-textfield>
            <tui-error [formControlName]="control" />
          </div>
        }
      </section>
    }

    @if (parent.ipMode() === 'dhcpv6') {
      <section>
        @for (control of dhcpv6Controls; track control) {
          <div>
            <tui-textfield>
              <label tuiLabel>{{ labels[control] }}</label>
              <input
                tuiInput
                [formControlName]="control"
                [maskito]="control === 'prefix' ? prefixMask : null"
                [placeholder]="control === 'prefix' ? 'Auto' : ''"
              />
            </tui-textfield>
            <tui-error [formControlName]="control" />
          </div>
        }
      </section>
    }

    @if (parent.ipMode() === 'static') {
      <section>
        @for (control of staticControls; track control) {
          <div>
            <tui-textfield>
              <label tuiLabel>{{ labels[control] }}*</label>
              <input
                tuiInput
                [formControlName]="control"
                [maskito]="control === 'prefix' ? prefixMask : null"
              />
            </tui-textfield>
            <tui-error [formControlName]="control" />
          </div>
        }
      </section>
    }

    @if (parent.ipMode() === '6rd') {
      <section>
        @for (control of sixrdControls; track control) {
          <div>
            <tui-textfield>
              <label tuiLabel>{{ labels[control] }}*</label>
              <input
                tuiInput
                [formControlName]="control"
                [maskito]="
                  control === 'prefix' || control === 'mask' ? prefixMask : null
                "
              />
            </tui-textfield>
            <tui-error [formControlName]="control" />
          </div>
        }
      </section>
    }
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
  providers: [tuiValidationErrorsProvider(IPV6_VALIDATION_ERRORS)],
  changeDetection: ChangeDetectionStrategy.OnPush,
  imports: [
    ReactiveFormsModule,
    TuiHeader,
    TuiTitle,
    TuiTextfield,
    TuiRadio,
    TuiError,
    TuiInput,
    TuiHint,
    MaskitoDirective,
  ],
})
export class WanIpv6Ip {
  protected readonly parent = inject(WanIpv6)

  readonly disabledLocked = input(false)

  protected readonly modes = IPV6_MODES
  protected readonly labels = IPV6_LABELS
  protected readonly slaacControls = IPV6_SLAAC_CONTROLS
  protected readonly dhcpv6Controls = IPV6_DHCPV6_CONTROLS
  protected readonly staticControls = IPV6_STATIC_CONTROLS
  protected readonly sixrdControls = IPV6_SIXRD_CONTROLS

  protected readonly prefixMask: MaskitoOptions = {
    mask: ['/', /\d/, /\d/, /\d/],
  }

  onModeClick(event: Event, mode: string) {
    // Prevent selecting 'disabled' if locked and not already disabled
    if (
      mode === 'disabled' &&
      this.disabledLocked() &&
      this.parent.ipMode() !== 'disabled'
    ) {
      event.preventDefault()
      event.stopPropagation()
    }
  }
}
