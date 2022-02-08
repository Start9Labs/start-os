import { Component } from '@angular/core'
import { ActivatedRoute } from '@angular/router'
import { NavController } from '@ionic/angular'

@Component({
  selector: 'developer-list',
  templateUrl: 'developer-list.page.html',
  styleUrls: ['developer-list.page.scss'],
})
export class DeveloperPage {
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
