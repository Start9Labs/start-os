import { Component } from '@angular/core'
import { ValueSpecTextarea } from '@start9labs/start-sdk/lib/config/configTypes'
import { Control } from '../control'
import { getDefaultString } from '../../../util/config-utilities'

@Component({
  selector: 'form-textarea',
  templateUrl: './form-textarea.component.html',
})
export class FormTextareaComponent extends Control<ValueSpecTextarea, string> {
  generate() {
    this.value = getDefaultString(this.spec.generate || '')
  }
}
