import { Component } from '@angular/core'
import { ValueSpecMultiselect } from '@start9labs/start-sdk/lib/config/configTypes'
import { Control } from '../control'
import { tuiPure } from '@taiga-ui/cdk'

@Component({
  selector: 'form-multiselect',
  templateUrl: './form-multiselect.component.html',
})
export class FormMultiselectComponent extends Control<
  ValueSpecMultiselect,
  readonly string[]
> {
  readonly items = Object.values(this.spec.values)

  readonly disabledItemHandler = (item: string): boolean =>
    !!this.spec.maxLength &&
    this.selected.length >= this.spec.maxLength &&
    !this.selected.includes(item)

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
