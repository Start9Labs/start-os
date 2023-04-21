import { Component } from '@angular/core'
import { ValueSpecText } from 'start-sdk/lib/config/configTypes'
import { Control } from '../control'

@Component({
  selector: 'form-text',
  templateUrl: './form-text.component.html',
  styleUrls: ['./form-text.component.scss'],
})
export class FormTextComponent extends Control<ValueSpecText, string> {
  masked = true
}
