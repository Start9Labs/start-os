import {
  ChangeDetectionStrategy,
  Component,
  inject,
  input,
} from '@angular/core'
import { ControlContainer, ReactiveFormsModule } from '@angular/forms'
import { MaskitoDirective } from '@maskito/angular'
import { MaskitoOptions } from '@maskito/core'
import {
  TuiError,
  TuiHint,
  TuiInput,
  TuiLabel,
  TuiTextfield,
  TuiTitle,
  tuiValidationErrorsProvider,
} from '@taiga-ui/core'
import { TuiSwitch } from '@taiga-ui/kit'
import { TuiCardLarge, TuiForm, TuiHeader } from '@taiga-ui/layout'

import { PREFIX_VALIDATION_ERRORS } from './utils'

@Component({
  selector: 'lan-ipv6-form',
  template: `
    <section formGroupName="strategy">
      <label
        tuiLabel
        [class.locked]="locked() && enabled()"
        [tuiHint]="locked() && enabled() ? lockedReason() : null"
        tuiHintAppearance="error"
      >
        <input
          type="checkbox"
          tuiSwitch
          formControlName="slaac"
          (click)="onToggleClick($event)"
        />
        Enable
      </label>
    </section>
    @if (enabled()) {
      <section formGroupName="subnet">
        <tui-textfield>
          <label tuiLabel>Prefix Length*</label>
          <input tuiInput formControlName="prefix" [maskito]="prefixMask" />
        </tui-textfield>
        <tui-error formControlName="prefix" />
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

    section + section {
      margin-top: 1rem;
    }

    tui-textfield {
      max-width: 10rem;
    }
  `,
  viewProviders: [
    {
      provide: ControlContainer,
      useFactory: () => inject(ControlContainer, { skipSelf: true }),
    },
  ],
  hostDirectives: [TuiForm, TuiCardLarge],
  providers: [tuiValidationErrorsProvider(PREFIX_VALIDATION_ERRORS)],
  changeDetection: ChangeDetectionStrategy.OnPush,
  imports: [
    ReactiveFormsModule,
    MaskitoDirective,
    TuiLabel,
    TuiSwitch,
    TuiHint,
    TuiInput,
    TuiTextfield,
    TuiError,
  ],
})
export class LanIpv6Form {
  readonly enabled = input(false)
  readonly locked = input(false)
  readonly lockedReason = input<string | null>(null)

  protected readonly prefixMask: MaskitoOptions = {
    mask: ['/', /\d/, /\d/, /\d/],
  }

  onToggleClick(event: Event) {
    if (this.locked() && this.enabled()) {
      event.preventDefault()
      event.stopPropagation()
    }
  }
}
