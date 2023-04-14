import { Component } from '@angular/core'
import { ValueSpecNumber } from 'start-sdk/lib/config/configTypes'
import { Range } from 'src/app/util/config-utilities'
import { Control } from '../control'

@Component({
  selector: 'form-number',
  templateUrl: './form-number.component.html',
})
export class FormNumberComponent extends Control<ValueSpecNumber, number> {
  protected readonly Infinity = Infinity
  private range = Range.from(this.spec.range)

  get min(): number {
    const min = this.range.min || -Infinity

    return this.range.minInclusive || !this.spec.integral ? min : min + 1
  }

  get max(): number {
    const max = this.range.max || Infinity

    return this.range.maxInclusive || !this.spec.integral ? max : max - 1
  }
}
