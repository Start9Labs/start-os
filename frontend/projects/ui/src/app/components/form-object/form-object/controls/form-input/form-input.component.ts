import { Component, Input, inject, Output, EventEmitter } from '@angular/core'
import { FormControl } from '@angular/forms'
import { ValueSpecOf } from 'start-sdk/types/config-types'
import { THEME } from '@start9labs/shared'
import { FormObjectComponent } from '../../form-object.component'

@Component({
  selector: 'form-input',
  templateUrl: './form-input.component.html',
  styleUrls: ['./form-input.component.scss'],
})
export class FormInputComponent {
  @Input() name!: string
  @Input() spec!: ValueSpecOf<'string' | 'number'>
  @Input() control!: FormControl

  @Output() onInputChange = new EventEmitter<void>()

  unmasked = false

  readonly theme$ = inject(THEME)

  constructor(readonly form: FormObjectComponent) {}
}