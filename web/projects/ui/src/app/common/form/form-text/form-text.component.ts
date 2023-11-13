import { Component } from '@angular/core'
import { ValueSpecText } from '@start9labs/start-sdk/lib/config/configTypes'
import { Control } from '../control'
import { getDefaultString } from 'src/app/util/config-utilities'

@Component({
  selector: 'form-text',
  templateUrl: './form-text.component.html',
  styleUrls: ['./form-text.component.scss'],
})
export class FormTextComponent extends Control<ValueSpecText, string> {
  masked = true

  generate() {
    this.value = getDefaultString(this.spec.generate || '')
  }
}
