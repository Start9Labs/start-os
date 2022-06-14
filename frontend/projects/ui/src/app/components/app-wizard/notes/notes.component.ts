import { Component, Input } from '@angular/core'
import { BaseSlide } from '../wizard-types'

@Component({
  selector: 'notes',
  templateUrl: './notes.component.html',
  styleUrls: ['../app-wizard.component.scss'],
})
export class NotesComponent implements BaseSlide {
  @Input() params: {
    versions: { version: string; notes: string }[]
    headline: string
  }

  loading = false

  async load() {}

  asIsOrder() {
    return 0
  }
}
