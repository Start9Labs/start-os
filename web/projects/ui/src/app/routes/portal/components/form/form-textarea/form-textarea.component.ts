import { Component } from '@angular/core'
import { CT } from '@start9labs/start-sdk'
import { Control } from '../control'

@Component({
  selector: 'form-textarea',
  templateUrl: './form-textarea.component.html',
})
export class FormTextareaComponent extends Control<
  CT.ValueSpecTextarea,
  string
> {}
