import { Component } from '@angular/core'
import { NavController } from '@ionic/angular'

@Component({
  selector: 'app-product-key',
  templateUrl: 'product-key.page.html',
  styleUrls: ['product-key.page.scss'],
})
export class ProductKeyPage {
  constructor(
    private readonly navCtrl: NavController,
  ) {}

  async recoverNav () {
    await this.navCtrl.navigateForward(`/recover`, { animationDirection: 'forward' })
  }

  async embassyNav () {
    await this.navCtrl.navigateForward(`/embassy`, { animationDirection: 'forward' })
  }
}

