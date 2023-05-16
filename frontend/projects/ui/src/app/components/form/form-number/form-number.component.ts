import { Component } from '@angular/core'
import { ValueSpecNumber } from '@start9labs/start-sdk/lib/config/configTypes'
import { Control } from '../control'

@Component({
  selector: 'form-number',
  templateUrl: './form-number.component.html',
})
export class FormNumberComponent extends Control<ValueSpecNumber, number> {
  protected readonly Infinity = Infinity
}
