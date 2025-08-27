import { Component } from '@angular/core'
import { FormsModule } from '@angular/forms'
import { IST } from '@start9labs/start-sdk'
import { TuiIcon, TuiNumberFormat, TuiTextfield } from '@taiga-ui/core'
import { TuiInputNumber, TuiTooltip } from '@taiga-ui/kit'

import { Control } from './control'
import { HintPipe } from '../pipes/hint.pipe'

@Component({
  selector: 'form-number',
  template: `
    <tui-textfield [tuiNumberFormat]="{ precision, decimalMode: 'not-zero' }">
      @if (spec.name) {
        <label tuiLabel>
          {{ spec.name }}
          @if (spec.required) {
            <span>*</span>
          }
        </label>
      }
      <input
        tuiInputNumber
        [postfix]="spec.units ? ' ' + spec.units : ''"
        [min]="spec.min"
        [max]="spec.max"
        [step]="spec.step || 0"
        [invalid]="control.invalid()"
        [disabled]="!!spec.disabled"
        [readOnly]="readOnly"
        [placeholder]="spec.placeholder || ''"
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
    TuiInputNumber,
    TuiNumberFormat,
    TuiIcon,
    TuiTooltip,
    HintPipe,
  ],
})
export class FormNumberComponent extends Control<IST.ValueSpecNumber, number> {
  get precision(): number {
    return this.spec.integer ? 0 : Infinity
  }
}
