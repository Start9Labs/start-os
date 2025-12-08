import { Component, inject } from '@angular/core'
import { FormsModule } from '@angular/forms'
import { invert } from '@start9labs/shared'
import { IST } from '@start9labs/start-sdk'
import { TUI_IS_MOBILE } from '@taiga-ui/cdk'
import { TuiDataList, TuiIcon, TuiTextfield } from '@taiga-ui/core'
import {
  TuiDataListWrapper,
  TuiFluidTypography,
  tuiFluidTypographyOptionsProvider,
  TuiSelect,
  TuiTooltip,
} from '@taiga-ui/kit'

import { Control } from './control'
import { HintPipe } from '../pipes/hint.pipe'

@Component({
  selector: 'form-select',
  template: `
    <tui-textfield
      [tuiTextfieldCleaner]="false"
      [disabledItemHandler]="disabledItemHandler"
      (tuiActiveZoneChange)="!$event && control.onTouched()"
    >
      @if (spec.name) {
        <label tuiLabel>{{ spec.name }} *</label>
      }
      @if (mobile) {
        <select
          tuiSelect
          [disabled]="disabled"
          [readOnly]="readOnly"
          [invalid]="control.invalid()"
          [placeholder]="spec.name"
          [items]="items"
          [(ngModel)]="selected"
        ></select>
      } @else {
        <input
          tuiSelect
          [disabled]="disabled"
          [readOnly]="readOnly"
          [invalid]="control.invalid()"
          [placeholder]="spec.name"
          [(ngModel)]="selected"
        />
      }
      @if (!mobile) {
        <tui-data-list *tuiTextfieldDropdown>
          @for (item of items; track $index) {
            <button
              tuiOption
              new
              tuiFluidTypography
              [style.white-space]="'nowrap'"
              [value]="item"
            >
              {{ item }}
            </button>
          }
        </tui-data-list>
      }
      @if (spec | hint; as hint) {
        <tui-icon [tuiTooltip]="hint" />
      }
    </tui-textfield>
  `,
  providers: [tuiFluidTypographyOptionsProvider({ max: 1 })],
  imports: [
    FormsModule,
    TuiTextfield,
    TuiSelect,
    TuiDataList,
    TuiFluidTypography,
    TuiIcon,
    TuiTooltip,
    HintPipe,
  ],
})
export class FormSelectComponent extends Control<IST.ValueSpecSelect, string> {
  private readonly inverted = invert(this.spec.values)

  protected readonly mobile = inject(TUI_IS_MOBILE)
  protected readonly items = Object.values(this.spec.values)
  protected readonly disabledItemHandler = (item: string) =>
    Array.isArray(this.spec.disabled) &&
    !!this.inverted[item] &&
    this.spec.disabled.includes(this.inverted[item]!)

  get disabled(): boolean {
    return typeof this.spec.disabled === 'string'
  }

  get selected(): string | null {
    return (this.value && this.spec.values[this.value]) || null
  }

  set selected(value: string | null) {
    this.value = (value && this.inverted[value]) || null
  }
}
