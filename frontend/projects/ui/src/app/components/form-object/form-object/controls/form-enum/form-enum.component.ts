import { Component, Input, Output, EventEmitter } from '@angular/core'
import { AbstractControl } from '@angular/forms'
import { ValueSpecOf } from 'start-sdk/types/config-types'

@Component({
  selector: 'form-enum',
  templateUrl: './form-enum.component.html',
  styleUrls: ['./form-enum.component.scss'],
})
export class FormEnumComponent {
  @Input() spec!: ValueSpecOf<'list'>
  @Input() control!: AbstractControl

  @Output() edit = new EventEmitter<void>()
}
