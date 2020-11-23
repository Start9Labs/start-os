import { Component, Input, OnInit } from '@angular/core'
import { BehaviorSubject } from 'rxjs'

@Component({
  selector: 'error-message',
  templateUrl: './error-message.component.html',
  styleUrls: ['./error-message.component.scss'],
})
export class ErrorMessageComponent implements OnInit {
  @Input() $error$: BehaviorSubject<string | undefined> = new BehaviorSubject(undefined)
  @Input() dismissable = true

  constructor () { }
  ngOnInit () { }

  clear () {
    this.$error$.next(undefined)
  }
}
