import { Component } from '@angular/core'
import { ValueSpecBoolean } from 'start-sdk/lib/config/configTypes'
import { Control } from '../control'

@Component({
  selector: 'form-boolean',
  templateUrl: './form-boolean.component.html',
  styleUrls: ['./form-boolean.component.scss'],
})
export class FormBooleanComponent extends Control<ValueSpecBoolean, boolean> {}
