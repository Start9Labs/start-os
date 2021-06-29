import { Component } from '@angular/core';
import { NavController, ToastController } from '@ionic/angular';
import { ApiService } from './services/api/api.service';
import { StateService } from './services/state.sevice';

@Component({
  selector: 'app-root',
  templateUrl: 'app.component.html',
  styleUrls: ['app.component.scss'],
})
export class AppComponent {

  constructor(
    private readonly apiService: ApiService,
    private readonly stateService: StateService,
    private readonly navCtrl: NavController,
    public toastController: ToastController
  ) {
    this.apiService.watchError$.subscribe(error => {
      if(error) {
        this.presentToast(error)
      }
    })
  }

  async ngOnInit() {
    await this.stateService.getState()
    if(this.stateService.hasPassword) {
      //redirect to embassyOS
    } else if (this.stateService.recoveryDrive) {
      await this.navCtrl.navigateForward(`/recover`)
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
    });
    await toast.present();

    await toast.onDidDismiss();
  }
}
