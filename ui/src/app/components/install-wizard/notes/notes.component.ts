import { Component, Input, OnInit } from '@angular/core'
import { BehaviorSubject, Subject } from 'rxjs'
import { Colorable, Loadable } from '../loadable'
import { WizardAction } from '../wizard-types'

@Component({
  selector: 'notes',
  templateUrl: './notes.component.html',
  styleUrls: ['../install-wizard.component.scss'],
})
export class NotesComponent implements OnInit, Loadable, Colorable {
  @Input() params: {
    action: WizardAction
    notes: string
    title: string
    titleColor: string
  }

  $loading$ = new BehaviorSubject(false)
  $color$ = new BehaviorSubject('light')
  $cancel$ = new Subject<void>()

  load () { }

  constructor () { }
  ngOnInit () { this.$color$.next(this.params.titleColor) }
}
