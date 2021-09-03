import { Component, Input, OnInit } from '@angular/core'
import { BehaviorSubject, Subject } from 'rxjs'
import { Loadable } from '../loadable'

@Component({
  selector: 'alert',
  templateUrl: './alert.component.html',
  styleUrls: ['../install-wizard.component.scss'],
})
export class AlertComponent implements OnInit, Loadable {
  @Input() params: {
    title: string
    message: string
    titleColor: string
  }

  loading$ = new BehaviorSubject(false)
  cancel$ = new Subject<void>()

  load () { }

  constructor () { }
  ngOnInit () { }
}
