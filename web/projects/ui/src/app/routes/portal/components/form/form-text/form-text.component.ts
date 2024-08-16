import { Component } from '@angular/core'
import { CT, utils } from '@start9labs/start-sdk'
import { Control } from '../control'

@Component({
  selector: 'form-text',
  templateUrl: './form-text.component.html',
  styleUrls: ['./form-text.component.scss'],
})
export class FormTextComponent extends Control<CT.ValueSpecText, string> {
  masked = true

  generate() {
    this.value = utils.getDefaultString(this.spec.generate || '')
  }
}
