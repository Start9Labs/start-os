import { Component } from '@angular/core'
import { ValueSpecMultiselect } from '@start9labs/start-sdk/lib/config/configTypes'
import { Control } from '../control'
import { tuiPure } from '@taiga-ui/cdk'
import { invert } from '@start9labs/shared'

@Component({
  selector: 'form-multiselect',
  templateUrl: './form-multiselect.component.html',
})
export class FormMultiselectComponent extends Control<
  ValueSpecMultiselect,
  readonly string[]
> {
  private readonly inverted = invert(this.spec.values)

  private readonly isDisabled = (item: string) =>
    Array.isArray(this.spec.disabled) &&
    this.spec.disabled.includes(this.inverted[item])

  private readonly isExceedingLimit = (item: string) =>
    !!this.spec.maxLength &&
    this.selected.length >= this.spec.maxLength &&
    !this.selected.includes(item)

  readonly disabledItemHandler = (item: string): boolean =>
    this.isDisabled(item) || this.isExceedingLimit(item)

  readonly items = Object.values(this.spec.values)

  get disabled(): boolean {
    return typeof this.spec.disabled === 'string'
  }

  get selected(): string[] {
    return this.memoize(this.value)
  }

  set selected(value: string[]) {
    this.value = Object.entries(this.spec.values)
      .filter(([_, v]) => value.includes(v))
      .map(([k]) => k)
  }

  @tuiPure
  private memoize(value: null | readonly string[]): string[] {
    return value?.map(key => this.spec.values[key]) || []
  }
}
