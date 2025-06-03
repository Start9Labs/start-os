import { Component } from '@angular/core'
import { IST } from '@start9labs/start-sdk'
import { Control } from '../control'

@Component({
  selector: 'form-textarea',
  templateUrl: './form-textarea.component.html',
  standalone: false,
})
export class FormTextareaComponent extends Control<
  IST.ValueSpecTextarea,
  string
> {}
