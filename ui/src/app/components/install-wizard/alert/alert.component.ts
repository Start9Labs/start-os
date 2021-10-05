import { Component, Input } from '@angular/core'

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

  load () { }
}
