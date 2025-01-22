import { Component } from '@angular/core'
import { IST } from '@start9labs/start-sdk'
import { Control } from '../control'

@Component({
  selector: 'form-toggle',
  templateUrl: './form-toggle.component.html',
  host: { style: 'display: flex' },
})
export class FormToggleComponent extends Control<
  IST.ValueSpecToggle,
  boolean
> {}
