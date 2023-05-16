import { Component } from '@angular/core'
import { ValueSpecSelect } from '@start9labs/start-sdk/lib/config/configTypes'
import { Control } from '../control'

@Component({
  selector: 'form-select',
  templateUrl: './form-select.component.html',
})
export class FormSelectComponent extends Control<ValueSpecSelect, string> {
  readonly items = Object.values(this.spec.values)

  get selected(): string | null {
    return this.value && this.spec.values[this.value]
  }

  set selected(value: string | null) {
    this.value =
      Object.entries(this.spec.values).find(([_, v]) => value === v)?.[0] ??
      null
  }
}
