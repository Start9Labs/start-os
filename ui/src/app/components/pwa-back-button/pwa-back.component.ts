import { Component } from '@angular/core'
import { NavController } from '@ionic/angular'

@Component({
  selector: 'pwa-back-button',
  templateUrl: './pwa-back.component.html',
  styleUrls: ['./pwa-back.component.scss'],
})
export class PwaBackComponent {
  constructor (
    private readonly nav: NavController,
  ) {  }

  navigateBack () {
    return this.nav.back()
  }
}
