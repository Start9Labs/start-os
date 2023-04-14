import { Component } from '@angular/core'
import { ValueSpecTextarea } from 'start-sdk/lib/config/configTypes'
import { Control } from '../control'

@Component({
  selector: 'form-text',
  templateUrl: './form-text.component.html',
})
export class FormTextComponent extends Control<ValueSpecTextarea, string> {}
