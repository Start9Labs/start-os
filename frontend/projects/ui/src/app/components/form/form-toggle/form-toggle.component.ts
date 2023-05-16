import { Component } from '@angular/core'
import { ValueSpecToggle } from '@start9labs/start-sdk/lib/config/configTypes'
import { Control } from '../control'

@Component({
  selector: 'form-toggle',
  templateUrl: './form-toggle.component.html',
  styleUrls: ['./form-toggle.component.scss'],
})
export class FormToggleComponent extends Control<ValueSpecToggle, boolean> {}
