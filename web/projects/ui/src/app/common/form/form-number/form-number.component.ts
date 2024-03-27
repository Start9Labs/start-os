import { Component } from '@angular/core'
import { CT } from '@start9labs/start-sdk'
import { Control } from '../control'

@Component({
  selector: 'form-number',
  templateUrl: './form-number.component.html',
})
export class FormNumberComponent extends Control<CT.ValueSpecNumber, number> {
  protected readonly Infinity = Infinity
}
