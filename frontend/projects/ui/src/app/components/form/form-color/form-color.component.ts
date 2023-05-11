import { Component } from '@angular/core'
import { ValueSpecColor } from '@start9labs/start-sdk/lib/config/configTypes'
import { Control } from '../control'
import { MaskitoOptions } from '@maskito/core'

@Component({
  selector: 'form-color',
  templateUrl: './form-color.component.html',
  styleUrls: ['./form-color.component.scss'],
})
export class FormColorComponent extends Control<ValueSpecColor, string> {
  readonly mask: MaskitoOptions = {
    mask: ['#', ...Array(6).fill(/[0-9a-f]/i)],
  }
}
