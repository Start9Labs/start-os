import { Component } from '@angular/core'
import { CT } from '@start9labs/start-sdk'
import { Control } from '../control'

@Component({
  selector: 'form-toggle',
  templateUrl: './form-toggle.component.html',
  host: { class: 'g-toggle' },
})
export class FormToggleComponent extends Control<CT.ValueSpecToggle, boolean> {}
