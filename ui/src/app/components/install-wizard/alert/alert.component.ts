import { Component, Input } from '@angular/core'
import { BehaviorSubject, Subject } from 'rxjs'

@Component({
  selector: 'alert',
  templateUrl: './alert.component.html',
  styleUrls: ['../install-wizard.component.scss'],
})
export class AlertComponent {
  @Input() params: {
    title: string
    message: string
    titleColor: string
  }

  loading$ = new BehaviorSubject(false)
  cancel$ = new Subject<void>()

  load () { }
}
