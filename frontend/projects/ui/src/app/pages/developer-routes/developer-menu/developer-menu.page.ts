import { Component } from '@angular/core'
import { ActivatedRoute } from '@angular/router'
import { NavController } from '@ionic/angular'

@Component({
  selector: 'developer-menu',
  templateUrl: 'developer-menu.page.html',
  styleUrls: ['developer-menu.page.scss'],
})
export class DeveloperMenuPage {
  constructor(
    private readonly navCtrl: NavController,
    private readonly route: ActivatedRoute,
  ) {}

  navToConfig() {
    this.navCtrl.navigateForward(['config'], { relativeTo: this.route })
  }

  navToInstructions() {
    this.navCtrl.navigateForward(['instructions'], { relativeTo: this.route })
  }
}
