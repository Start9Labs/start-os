import { Component, Input } from '@angular/core'
import { AbstractControl } from '@angular/forms'
import { ValueSpecOf } from '@start9labs/start-sdk/lib/config/configTypes'

@Component({
  selector: 'form-file',
  templateUrl: './form-file.component.html',
  styleUrls: ['./form-file.component.scss'],
})
export class FormFileComponent {
  @Input() spec!: ValueSpecOf<'file'>
  @Input() control!: AbstractControl

  handleFileInput(e: any) {
    this.control.patchValue(e.target.files[0])
  }

  clearFile() {
    this.control.patchValue(null)
  }
}
