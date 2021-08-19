import { Component, OnInit } from '@angular/core'
import { NavController } from '@ionic/angular'

@Component({
  selector: 'app-root',
  templateUrl: 'app.component.html',
  styleUrls: ['app.component.scss'],
})
export class AppComponent implements OnInit {

  constructor(
    private readonly navCtrl: NavController,
  ) {}

  async ngOnInit() {
    await this.navCtrl.navigateForward(`/product-key`)
  }
}
