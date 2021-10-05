import { Component, Input } from '@angular/core'

@Component({
  selector: 'notes',
  templateUrl: './notes.component.html',
  styleUrls: ['../install-wizard.component.scss'],
})
export class NotesComponent {
  @Input() params: {
    notes: { [version: string]: string }
    title: string
    titleColor: string
    headline: string
  }

  load () { }

  asIsOrder () {
    return 0
  }
}
