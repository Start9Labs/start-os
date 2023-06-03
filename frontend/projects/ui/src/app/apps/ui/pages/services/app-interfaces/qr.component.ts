import { Component, Input } from '@angular/core'

@Component({
  selector: 'qr',
  template: '<qr-code [value]="text" size="400"></qr-code>',
})
export class QRComponent {
  @Input() text!: string
}
