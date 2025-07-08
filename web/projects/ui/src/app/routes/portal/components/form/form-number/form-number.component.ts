import { Component } from '@angular/core'
import { IST } from '@start9labs/start-sdk'
import { Control } from '../control'

@Component({
  selector: 'form-number',
  templateUrl: './form-number.component.html',
  standalone: false,
})
export class FormNumberComponent extends Control<IST.ValueSpecNumber, number> {
  protected readonly Infinity = Infinity
}
