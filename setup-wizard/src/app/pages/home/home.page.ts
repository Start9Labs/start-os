import { Component } from '@angular/core'
import { NavController } from '@ionic/angular'

@Component({
  selector: 'home',
  templateUrl: 'home.page.html',
  styleUrls: ['home.page.scss'],
})
export class HomePage {
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

