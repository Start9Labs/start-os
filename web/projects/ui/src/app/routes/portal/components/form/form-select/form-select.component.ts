import { Component } from '@angular/core'
import { IST } from '@start9labs/start-sdk'
import { invert } from '@start9labs/shared'
import { Control } from '../control'

@Component({
  selector: 'form-select',
  templateUrl: './form-select.component.html',
  standalone: false,
})
export class FormSelectComponent extends Control<IST.ValueSpecSelect, string> {
  private readonly inverted = invert(this.spec.values)

  readonly items = Object.values(this.spec.values)

  readonly disabledItemHandler = (item: string) =>
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
