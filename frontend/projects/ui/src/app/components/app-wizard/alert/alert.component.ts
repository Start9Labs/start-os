import { Component, Input } from '@angular/core'
import { BaseSlide } from '../wizard-types'

@Component({
  selector: 'alert',
  templateUrl: './alert.component.html',
  styleUrls: ['../app-wizard.component.scss'],
})
export class AlertComponent implements BaseSlide {
  @Input() params: {
    message: string
  }

  async load() {}

  loading = false
}
