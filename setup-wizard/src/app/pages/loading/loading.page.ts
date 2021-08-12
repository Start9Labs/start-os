import { Component } from '@angular/core'
import { AlertController } from '@ionic/angular'
import { StateService } from 'src/app/services/state.service'

@Component({
  selector: 'app-loading',
  templateUrl: 'loading.page.html',
  styleUrls: ['loading.page.scss'],
})
export class LoadingPage {
  constructor(
    private stateService: StateService,
    private alertCtrl: AlertController
  ) {}

  ngOnInit () {
    this.stateService.pollDataTransferProgress()
    const progSub = this.stateService.dataProgSubject.subscribe(async progress => {
      if(progress === 1) {
        await this.successAlert()
        progSub.unsubscribe()
      }
    })
  }

  async successAlert () {
    const alert = await this.alertCtrl.create({
      cssClass: 'success-alert',
      header: 'Success!',
      subHeader: `Your Embassy is set up and ready to go.`,
      backdropDismiss: false,
      buttons: [
        {
          text: 'Go To Embassy',
          handler: () => {
            window.location.reload()
          }
        }
      ]
    })
    await alert.present()
  }
}

