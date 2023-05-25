import { Component } from '@angular/core'
import { NavController } from '@ionic/angular'
import { StateService } from 'src/app/services/state.service'

@Component({
  templateUrl: 'loading.page.html',
})
export class LoadingPage {
  constructor(
    readonly stateService: StateService,
    readonly navCtrl: NavController,
  ) {}
}
