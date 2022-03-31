import { Component, Input } from '@angular/core'
import { BehaviorSubject, Subject } from 'rxjs'

@Component({
  selector: 'notes',
  templateUrl: './notes.component.html',
  styleUrls: ['../install-wizard.component.scss'],
})
export class NotesComponent {
  @Input() params: {
    versions: { version: string; notes: string }[]
    title: string
    titleColor: string
    headline: string
  }

  load() {}
  loading$ = new BehaviorSubject(false)
  cancel$ = new Subject<void>()

  asIsOrder() {
    return 0
  }
}
