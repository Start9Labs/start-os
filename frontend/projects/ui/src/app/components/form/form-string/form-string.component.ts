import { Component } from '@angular/core'
import { ValueSpecString } from 'start-sdk/lib/config/configTypes'
import { Control } from '../control'

@Component({
  selector: 'form-string',
  templateUrl: './form-string.component.html',
  styleUrls: ['./form-string.component.scss'],
})
export class FormStringComponent extends Control<ValueSpecString, string> {
  masked = true
}
