import { Component, OnInit } from '@angular/core'
import { NavController } from '@ionic/angular'
import { StateService } from './services/state.service'

@Component({
  selector: 'app-root',
  templateUrl: 'app.component.html',
  styleUrls: ['app.component.scss'],
})
export class AppComponent implements OnInit {

  constructor(
    private readonly navCtrl: NavController,
    private stateService: StateService

  ) {}

  async ngOnInit() {
    this.stateService.reset()
    await this.navCtrl.navigateForward(`/wizard`)
  }
}
