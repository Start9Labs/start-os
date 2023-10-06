import { Component } from '@angular/core'
import { ValueSpecToggle } from '@start9labs/start-sdk/lib/config/configTypes'
import { Control } from '../control'

@Component({
  selector: 'form-toggle',
  templateUrl: './form-toggle.component.html',
  host: { class: 'g-toggle' },
})
export class FormToggleComponent extends Control<ValueSpecToggle, boolean> {}
