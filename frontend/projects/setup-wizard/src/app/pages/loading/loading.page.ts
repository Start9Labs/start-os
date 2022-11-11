import { Component } from '@angular/core'
import { NavController } from '@ionic/angular'
import { StateService } from 'src/app/services/state.service'

@Component({
  selector: 'app-loading',
  templateUrl: 'loading.page.html',
  styleUrls: ['loading.page.scss'],
})
export class LoadingPage {
  constructor(
    public stateService: StateService,
    private navCtrl: NavController,
  ) {}

  ngOnInit() {
    this.stateService.pollDataTransferProgress()
    const progSub = this.stateService.dataCompletionSubject.subscribe(
      async complete => {
        if (complete) {
          progSub.unsubscribe()
          await this.navCtrl.navigateForward(`/success`)
        }
      },
    )
  }
}
