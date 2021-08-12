import { Component } from '@angular/core'
import { AlertController, ModalController, NavController } from '@ionic/angular'
import { ApiService, EmbassyDrive } from 'src/app/services/api/api.service'
import { StateService } from 'src/app/services/state.service'
import { PasswordPage } from '../password/password.page'

@Component({
  selector: 'app-embassy',
  templateUrl: 'embassy.page.html',
  styleUrls: ['embassy.page.scss'],
})
export class EmbassyPage {
  embassyDrives = []
  selectedDrive: EmbassyDrive = null
  loading = true

  constructor(
    private readonly apiService: ApiService,
    private readonly navCtrl: NavController,
    private modalController: ModalController,
    private stateService: StateService,
    private readonly alertCtrl: AlertController,
  ) {}

  async ngOnInit() {
    this.embassyDrives = await this.apiService.getEmbassyDrives()
    this.loading = false
  }

  async chooseDrive(drive: EmbassyDrive) {    
    const modal = await this.modalController.create({
      component: PasswordPage,
      componentProps: {
        embassyDrive: drive
      }
    })
    modal.onDidDismiss().then(async ret => {
      if (!ret.data && !ret.data.success) return

      if(!!this.stateService.recoveryDrive) {
        await this.navCtrl.navigateForward(`/loading`, { animationDirection: 'forward' })
      } else {
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
    })
    await modal.present();
  }
}
