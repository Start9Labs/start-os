import { Component, Input } from '@angular/core'
import { isPlatform } from '@ionic/angular'

@Component({
  selector: 'qr',
  templateUrl: './qr.component.html',
  styleUrls: ['./qr.component.scss'],
})
export class QRComponent {
  @Input() text: string
  width: number

  ngOnInit () {
    this.width = isPlatform('ios') || isPlatform('android') ? 320 : 420
  }
}
