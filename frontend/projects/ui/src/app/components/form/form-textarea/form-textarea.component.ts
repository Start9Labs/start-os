import { Component } from '@angular/core'
import { ValueSpecTextarea } from 'start-sdk/lib/config/configTypes'
import { Control } from '../control'

@Component({
  selector: 'form-textarea',
  templateUrl: './form-textarea.component.html',
})
export class FormTextareaComponent extends Control<ValueSpecTextarea, string> {}
