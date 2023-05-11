import { Component, Input, Output, EventEmitter } from '@angular/core'
import { AbstractControl } from '@angular/forms'
import { ValueSpecOf } from '@start9labs/start-sdk/lib/config/configTypes'

@Component({
  selector: 'form-subform',
  templateUrl: './form-subform.component.html',
  styleUrls: ['./form-subform.component.scss'],
})
export class FormSubformComponent {
  @Input() spec!: ValueSpecOf<'object'>
  @Input() control!: AbstractControl
  @Input() hasNewOptions = false

  expanded = false
}
