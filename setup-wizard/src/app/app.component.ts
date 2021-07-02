import { Component, OnInit } from '@angular/core'
import { LoadingController, NavController, ToastController } from '@ionic/angular'
import { ApiService } from './services/api/api.service'
import { StateService } from './services/state.service'

@Component({
  selector: 'app-root',
  templateUrl: 'app.component.html',
  styleUrls: ['app.component.scss'],
})
export class AppComponent implements OnInit {

  constructor(
    private readonly apiService: ApiService,
    private readonly stateService: StateService,
    private readonly navCtrl: NavController,
    private readonly toastController: ToastController,
    private readonly loadingCtrl: LoadingController,

  ) {
    this.apiService.watchError$.subscribe(error => {
      if(error) {
        this.presentToast(error)
      }
    })
  }

  async ngOnInit() {
    const loader = await this.loadingCtrl.create({
      message: 'Connecting to your Embassy'
    })
    await loader.present()
    try {
      await this.stateService.getState()
      if (this.stateService.recoveryDrive) {
        await this.navCtrl.navigateForward(`/recover`)
      } else {
        await this.navCtrl.navigateForward(`/wizard`)
      }
    } catch (e) {} finally {
      loader.dismiss()
    }
  }

  async presentToast(error: string) {
    const toast = await this.toastController.create({
      header: 'Error',
      message: error,
      position: 'bottom',
      buttons: [
       {
          text: 'X',
          role: 'cancel',
        }
      ]
    })
    await toast.present()

    await toast.onDidDismiss()
  }
}
