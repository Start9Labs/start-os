import { Component, Input, OnInit } from '@angular/core'
import { BehaviorSubject, Subject } from 'rxjs'
import { Loadable } from '../loadable'

@Component({
  selector: 'notes',
  templateUrl: './notes.component.html',
  styleUrls: ['../install-wizard.component.scss'],
})
export class NotesComponent implements OnInit, Loadable {
  @Input() params: {
    notes: { [version: string]: string }
    title: string
    titleColor: string
    headline: string
  }

  loading$ = new BehaviorSubject(false)
  cancel$ = new Subject<void>()

  load () { }

  constructor () { }
  ngOnInit () { }

  asIsOrder () {
    return 0
  }
}
