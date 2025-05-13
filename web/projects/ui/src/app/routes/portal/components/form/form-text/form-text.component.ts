import { Component } from '@angular/core'
import { IST, utils } from '@start9labs/start-sdk'
import { Control } from '../control'

@Component({
  selector: 'form-text',
  templateUrl: './form-text.component.html',
  styleUrls: ['./form-text.component.scss'],
  standalone: false,
})
export class FormTextComponent extends Control<IST.ValueSpecText, string> {
  masked = true

  generate() {
    this.value = utils.getDefaultString(this.spec.generate || '')
  }
}
