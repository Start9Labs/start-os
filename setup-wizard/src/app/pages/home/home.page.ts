import { Component } from '@angular/core'
import { AlertController, ModalController } from '@ionic/angular'
import { ApiService } from 'src/app/services/api/api.service'
import { StateService } from 'src/app/services/state.service'
import { PasswordPage } from '../password/password.page'

@Component({
  selector: 'home',
  templateUrl: 'home.page.html',
  styleUrls: ['home.page.scss'],
})
export class HomePage {
  dataDrives = []
  selectedDrive = null

  constructor(
    private readonly apiService: ApiService,
    private readonly stateService: StateService,
    private readonly alertController: AlertController,
    private modalController: ModalController,
  ) {}

  async ngOnInit() {
    if(!this.stateService.selectedDataDrive) {
      this.dataDrives = await this.apiService.getStorageDisks()
    }
  }

  selectDrive(name: string) {
    if (name === this.selectedDrive) {
      this.selectedDrive = null
    } else {
      this.selectedDrive = name
    }
  }

  async warn() {
    const alert = await this.alertController.create({
      cssClass: 'my-custom-class',
      header: 'Warning!',
      message: 'This disk will be entirely wiped of all memory.',
      buttons: [
        {
          text: 'Cancel',
          role: 'cancel',
          cssClass: 'secondary',
          handler: () => {
            this.selectedDrive = null
          }
        }, {
          text: 'Okay',
          handler: async () => {
            await this.chooseDisk()
          }
        }
      ]
    });

    await alert.present();
  }

  async chooseDisk() {
    await this.apiService.selectStorageDisk({logicalName: this.selectedDrive})
    this.stateService.selectedDataDrive = this.selectedDrive
  }

  async presentPasswordModal() {
    const modal = await this.modalController.create({
      component: PasswordPage,
      backdropDismiss: false,
      componentProps: {
        'version': null,
      }
    })
    modal.onDidDismiss().then(ret => {
      if(ret.data.pwValid) {
        this.submitPassword(ret.data.password)
      }
    })
    await modal.present();
  }

  async submitPassword (pw: string) {
    await this.apiService.submitPassword(pw)
    // @TODO navigate to embassyOS
  }

}
