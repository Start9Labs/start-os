import { Component, Input, OnInit } from '@angular/core'
import { BehaviorSubject, Subject } from 'rxjs'
import { Colorable, Loadable } from '../loadable'
import { WizardAction } from '../wizard-types'

@Component({
  selector: 'developer-notes',
  templateUrl: './developer-notes.component.html',
  styleUrls: ['../install-wizard.component.scss'],
})
export class DeveloperNotesComponent implements OnInit, Loadable, Colorable {
  @Input() params: {
    action: WizardAction
    developerNotes: string
  }

  $loading$ = new BehaviorSubject(false)
  $color$ = new BehaviorSubject('warning')
  $cancel$ = new Subject<void>()

  load () { }

  constructor () { }
  ngOnInit () {
    console.log('Developer Notes', this.params)
  }
}
