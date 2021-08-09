import { Component } from '@angular/core'
import { ModalController, NavController } from '@ionic/angular'
import { ApiService, EmbassyDrive } from 'src/app/services/api/api.service'
import { StateService } from 'src/app/services/state.service'
import { PasswordPage } from '../password/password.page'

@Component({
  selector: 'embassy',
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
    private stateService: StateService
  ) {}

  async ngOnInit() {
    this.embassyDrives = await this.apiService.getEmbassyDrives()
    this.loading = false
  }

  async chooseDrive(drive: EmbassyDrive) {    
    const modal = await this.modalController.create({
      component: PasswordPage,
      componentProps: {
        embassyDrive: drive,
        verify: false
      }
    })
    modal.onDidDismiss().then(async ret => {
      if (!ret.data) return
    })
    await modal.present();
  }
}
