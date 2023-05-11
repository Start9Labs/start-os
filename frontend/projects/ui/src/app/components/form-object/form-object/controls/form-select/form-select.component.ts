import { Component, Input } from '@angular/core'
import { FormControl } from '@angular/forms'
import { ValueSpecOf } from '@start9labs/start-sdk/lib/config/configTypes'

@Component({
  selector: 'form-select',
  templateUrl: './form-select.component.html',
  styleUrls: ['./form-select.component.scss'],
})
export class FormSelectComponent {
  @Input() spec!: ValueSpecOf<'toggle' | 'select' | 'multiselect'>
  @Input() control!: FormControl
  @Input() name!: string

  cancelBool = () => this.control.setValue(!this.control.value)
}
