import { Component } from '@angular/core'
import { IST } from '@start9labs/start-sdk'
import { Control } from '../control'
import { MaskitoOptions } from '@maskito/core'

@Component({
  selector: 'form-color',
  templateUrl: './form-color.component.html',
  styleUrls: ['./form-color.component.scss'],
})
export class FormColorComponent extends Control<IST.ValueSpecColor, string> {
  readonly mask: MaskitoOptions = {
    mask: ['#', ...Array(6).fill(/[0-9a-f]/i)],
  }
}
