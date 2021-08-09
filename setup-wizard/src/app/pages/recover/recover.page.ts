import { Component } from '@angular/core'
import { AlertController, ModalController, LoadingController } from '@ionic/angular'
import { ApiService, RecoveryDrive } from 'src/app/services/api/api.service'
import { StateService } from 'src/app/services/state.service'
import { PasswordPage } from '../password/password.page'

@Component({
  selector: 'recover',
  templateUrl: 'recover.page.html',
  styleUrls: ['recover.page.scss'],
})
export class RecoverPage {
  recoveryDrives = []
  selectedDrive: RecoveryDrive = null
  loading = true

  constructor(
    private readonly apiService: ApiService,
    private readonly stateService: StateService,
    public alertController: AlertController,
    private modalController: ModalController,
    private readonly loadingCtrl: LoadingController,
  ) {}

  async ngOnInit() {
    if(!this.stateService.recoveryDrive) {
      const loader = await this.loadingCtrl.create({
        message: 'Fetching recovery drives'
      })
      await loader.present()
      this.recoveryDrives = await this.apiService.getRecoveryDrives()

      loader.dismiss()  
      this.loading = false
    } else {
      this.loading = false
      this.stateService.pollDataTransferProgress()
    }
  }

  selectDrive(drive: RecoveryDrive) {
    if (drive.logicalname === this.selectedDrive?.logicalname) {
      this.selectedDrive = null
    } else {
      this.selectedDrive = drive
    }
  }

  async chooseDrive() {
    this.presentPasswordModal()
  }

  async presentPasswordModal() {
    const modal = await this.modalController.create({
      component: PasswordPage,
      backdropDismiss: false,
      cssClass: 'pw-modal',
      componentProps: {
        recoveryDrive: this.selectedDrive,
      }
    })
    modal.onDidDismiss().then(ret => {
      if(ret.data) {
        const pass = ret.data.password
        if(pass) {
          this.submitPWAndDrive(pass)
        }
      }
    })
    await modal.present();
  }

  async submitPWAndDrive(pw: string) {
    const loader = await this.loadingCtrl.create({
      message: 'Validating password'
    })
    await loader.present()

    try {
      this.stateService.recoveryDrive = this.selectedDrive
      await this.apiService.selectRecoveryDrive(this.selectedDrive.logicalname, pw)
      this.stateService.pollDataTransferProgress()
    } catch (e) {
    } finally {
      loader.dismiss()
    }


  }

  async navToEmbassy() {
    location.reload()
  }

}
