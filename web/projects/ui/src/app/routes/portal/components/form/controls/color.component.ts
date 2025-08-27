import { ChangeDetectionStrategy, Component } from '@angular/core'
import { FormsModule } from '@angular/forms'
import { IST } from '@start9labs/start-sdk'
import { TuiIcon, TuiTextfield } from '@taiga-ui/core'
import { TuiInputColor, TuiTooltip } from '@taiga-ui/kit'

import { Control } from './control'
import { HintPipe } from '../pipes/hint.pipe'

@Component({
  selector: 'form-color',
  template: `
    <tui-textfield iconStart=" " [tuiTextfieldCleaner]="false">
      @if (spec.name) {
        <label tuiLabel>
          {{ spec.name }}
          @if (spec.required) {
            <span>*</span>
          }
        </label>
      }
      <input
        placeholder="#000000"
        tuiInputColor
        [invalid]="control.invalid()"
        [readOnly]="readOnly"
        [disabled]="!!spec.disabled"
        [(ngModel)]="value"
        (blur)="control.onTouched()"
      />
      @if (spec | hint; as hint) {
        <tui-icon [tuiTooltip]="hint" />
      }
    </tui-textfield>
  `,
  imports: [
    FormsModule,
    TuiTextfield,
    TuiInputColor,
    TuiIcon,
    TuiTooltip,
    HintPipe,
  ],
  changeDetection: ChangeDetectionStrategy.OnPush,
})
export class FormColorComponent extends Control<IST.ValueSpecColor, string> {}
