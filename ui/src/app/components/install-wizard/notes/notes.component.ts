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
    notes: string
    title: string
    titleColor: string
  }

  loading$ = new BehaviorSubject(false)
  cancel$ = new Subject<void>()

  load () { }

  constructor () { }
  ngOnInit () { }
}
