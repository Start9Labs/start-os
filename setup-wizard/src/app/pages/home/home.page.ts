import { Component } from '@angular/core'
import { AlertController, ModalController, LoadingController } from '@ionic/angular'
import { ApiService, DataDrive } from 'src/app/services/api/api.service'
import { StateService } from 'src/app/services/state.service'
import { PasswordPage } from '../password/password.page'

@Component({
  selector: 'home',
  templateUrl: 'home.page.html',
  styleUrls: ['home.page.scss'],
})
export class HomePage {
  dataDrives = null
  selectedDrive: DataDrive = null

  constructor(
    private readonly apiService: ApiService,
    private readonly stateService: StateService,
    private readonly alertController: AlertController,
    private readonly modalController: ModalController,
    private readonly loadingCtrl: LoadingController,
  ) {}

  async ngOnInit() {
    const loader = await this.loadingCtrl.create({
      message: 'Selecting data drive'
    })
    if(!this.stateService.dataDrive) {
      const loader = await this.loadingCtrl.create({
        message: 'Fetching data drives'
      })
      await loader.present()
      this.dataDrives = await this.apiService.getDataDrives()
      loader.dismiss()
    }
  }

  selectDrive(drive: DataDrive) {
    if (drive.logicalname === this.selectedDrive?.logicalname) {
      this.selectedDrive = null
    } else {
      this.selectedDrive = drive
    }
  }

  async warn() {
    const alert = await this.alertController.create({
      cssClass: 'my-custom-class',
      header: 'Warning!',
      message: 'This drive will be entirely wiped of all memory.',
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
            await this.chooseDrive()
          }
        }
      ]
    });

    await alert.present();
  }

  async chooseDrive() {
    const loader = await this.loadingCtrl.create({
      message: 'Selecting data drive'
    })
    await loader.present()
    try {
      await this.apiService.selectDataDrive(this.selectedDrive.logicalname)
      this.stateService.dataDrive = this.selectedDrive
    } catch (e) {
    } finally {
      loader.dismiss()
    }
  }

  async presentPasswordModal() {
    const modal = await this.modalController.create({
      component: PasswordPage,
      backdropDismiss: false
    })
    modal.onDidDismiss().then(ret => {
      const pass = ret.data.password
      if (pass) {
        this.submitPassword(pass)
      }
    })
    await modal.present();
  }

  async submitPassword (pw: string) {
    const loader = await this.loadingCtrl.create({
      message: 'Setting up your Embassy'
    })
    await loader.present()

    try {
      await this.apiService.submitPassword(pw)
      location.reload()
    } catch (e) {
    } finally {
      loader.dismiss()
    }
  }

}
