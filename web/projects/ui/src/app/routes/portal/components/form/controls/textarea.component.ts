import { Component } from '@angular/core'
import { FormsModule } from '@angular/forms'
import { IST } from '@start9labs/start-sdk'
import { TuiIcon, TuiTextfield } from '@taiga-ui/core'
import { TuiTextarea, TuiTooltip } from '@taiga-ui/kit'

import { Control } from './control'
import { HintPipe } from '../pipes/hint.pipe'

@Component({
  selector: 'form-textarea',
  template: `
    <tui-textfield>
      @if (spec.name) {
        <label tuiLabel>
          {{ spec.name }}
          @if (spec.required) {
            <span>*</span>
          }
        </label>
      }
      <textarea
        placeholder="Placeholder"
        tuiTextarea
        [max]="6"
        [min]="3"
        [attr.maxLength]="spec.maxLength"
        [disabled]="!!spec.disabled"
        [readOnly]="readOnly"
        [placeholder]="spec.placeholder || ''"
        [invalid]="control.invalid()"
        [(ngModel)]="value"
        (blur)="control.onTouched()"
      ></textarea>
      @if (spec | hint; as hint) {
        <tui-icon [tuiTooltip]="hint" />
      }
    </tui-textfield>
  `,
  imports: [
    FormsModule,
    TuiTextfield,
    TuiTextarea,
    TuiIcon,
    TuiTooltip,
    HintPipe,
  ],
})
export class FormTextareaComponent extends Control<
  IST.ValueSpecTextarea,
  string
> {}
