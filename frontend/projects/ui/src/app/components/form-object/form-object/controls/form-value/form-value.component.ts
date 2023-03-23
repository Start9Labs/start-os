import { Component, Input } from '@angular/core'
import { FormControl } from '@angular/forms'
import { ValueSpecOf } from 'start-sdk/types/config-types'

@Component({
  selector: 'form-value',
  templateUrl: './form-value.component.html',
})
export class FormValueComponent {
  @Input() spec!: ValueSpecOf<'boolean' | 'enum'>
  @Input() control!: FormControl
  @Input() name!: string

  cancel = () => this.control.setValue(!this.control.value)
}
