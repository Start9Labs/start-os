import { Component, computed } from '@angular/core'
import { FormsModule } from '@angular/forms'
import { invert } from '@start9labs/shared'
import { IST } from '@start9labs/start-sdk'
import { TuiIcon, TuiTextfield } from '@taiga-ui/core'
import { TuiChevron, TuiMultiSelect, TuiTooltip } from '@taiga-ui/kit'
import { HintPipe } from '../pipes/hint.pipe'
import { Control } from './control'

@Component({
  selector: 'form-multiselect',
  template: `
    <tui-textfield multi tuiChevron [disabledItemHandler]="disabledItemHandler">
      @if (spec.name) {
        <label tuiLabel>{{ spec.name }}</label>
      }
      <select
        tuiMultiSelect
        [invalid]="control.invalid()"
        [disabled]="disabled"
        [readOnly]="readOnly"
        [items]="items"
        [ngModel]="selected()"
        (ngModelChange)="onSelected($event)"
        (blur)="control.onTouched()"
      ></select>
      @if (spec | hint; as hint) {
        <tui-icon [tuiTooltip]="hint" />
      }
    </tui-textfield>
  `,
  styles: `
    // TODO: Remove after Taiga UI update
    :host ::ng-deep .t-input {
      pointer-events: none;
    }
  `,
  imports: [
    FormsModule,
    TuiTextfield,
    TuiMultiSelect,
    TuiIcon,
    TuiTooltip,
    HintPipe,
    TuiChevron,
  ],
})
export class FormMultiselectComponent extends Control<
  IST.ValueSpecMultiselect,
  readonly string[]
> {
  private readonly inverted = invert(this.spec.values)

  private readonly isDisabled = (item: string) =>
    Array.isArray(this.spec.disabled) &&
    !!this.inverted[item] &&
    this.spec.disabled.includes(this.inverted[item]!)

  private readonly isExceedingLimit = (item: string) =>
    !!this.spec.maxLength &&
    this.selected().length >= this.spec.maxLength &&
    !this.selected().includes(item)

  readonly disabledItemHandler = (item: string): boolean =>
    this.isDisabled(item) || this.isExceedingLimit(item)

  readonly items = Object.values(this.spec.values)
  readonly selected = computed(
    () =>
      this.control.value().map((key: string) => this.spec.values[key] || '') ||
      [],
  )

  get disabled(): boolean {
    return typeof this.spec.disabled === 'string'
  }

  onSelected(value: string[]) {
    this.value = Object.entries(this.spec.values)
      .filter(([_, v]) => value.includes(v))
      .map(([k]) => k)
  }
}
