import { Component } from '@angular/core'
import { iosTransitionAnimation, LoadingController, ModalController, NavController } from '@ionic/angular'
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
  window = window

  constructor(
    private readonly apiService: ApiService,
    private readonly navCtrl: NavController,
    private modalController: ModalController,
    private stateService: StateService,
    private loadingCtrl: LoadingController
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
      if (!ret.data || !ret.data.password) return

      const loader = await this.loadingCtrl.create({
        message: 'Setting up your Embassy!'
      })
      
      await loader.present()
  
      this.stateService.embassyDrive = drive
      this.stateService.embassyPassword = ret.data.password
  
      try {
        this.stateService.torAddress = (await this.stateService.setupEmbassy()).torAddress
      } catch (e) {
        console.log(e.message)
      } finally {
        loader.dismiss()
        if(!!this.stateService.recoveryDrive) {
          await this.navCtrl.navigateForward(`/loading`, { animationDirection: 'forward', animation: iosTransitionAnimation })
        } else {
          await this.navCtrl.navigateForward(`/success`, { animationDirection: 'forward', animation: iosTransitionAnimation })
        }
      }
    })
    await modal.present();
  }
}
