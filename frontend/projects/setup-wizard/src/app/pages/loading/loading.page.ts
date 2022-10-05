import { Component } from '@angular/core'
import { ActivatedRoute } from '@angular/router'
import { NavController } from '@ionic/angular'
import { StateService } from 'src/app/services/state.service'

@Component({
  selector: 'app-loading',
  templateUrl: 'loading.page.html',
  styleUrls: ['loading.page.scss'],
})
export class LoadingPage {
  incomingAction!: string

  constructor(
    public stateService: StateService,
    private navCtrl: NavController,
    private route: ActivatedRoute,
  ) {}

  ngOnInit() {
    this.incomingAction = this.route.snapshot.paramMap.get('action')!
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
