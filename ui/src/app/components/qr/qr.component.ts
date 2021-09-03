import { Component, Input } from '@angular/core'

@Component({
  selector: 'qr',
  templateUrl: './qr.component.html',
  styleUrls: ['./qr.component.scss'],
})
export class QRComponent {
  @Input() text: string
}
