import { Component } from '@angular/core'
import { ValueSpecNumber } from 'start-sdk/lib/config/configTypes'
import { Control } from '../control'

@Component({
  selector: 'form-number',
  templateUrl: './form-number.component.html',
})
export class FormNumberComponent extends Control<ValueSpecNumber, number> {
  protected readonly Infinity = Infinity

  get min(): number {
    if (typeof this.spec.min !== 'number') return -Infinity
    return this.spec.min
  }

  get max(): number {
    if (typeof this.spec.max !== 'number') return Infinity
    return this.spec.max
  }
}
