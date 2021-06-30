import { Component } from '@angular/core'
import { AlertController, ModalController } from '@ionic/angular'
import { ApiService, EmbassyDrive } from 'src/app/services/api/api.service'
import { StateService } from 'src/app/services/state.service'
import { PasswordPage } from '../password/password.page'

@Component({
  selector: 'recover',
  templateUrl: 'recover.page.html',
  styleUrls: ['recover.page.scss'],
})
export class RecoverPage {
  dataDrives = []
  selectedDrive = null
  selectedVersion = null

  constructor(
    private readonly apiService: ApiService,
    private readonly stateService: StateService,
    public alertController: AlertController,
    private modalController: ModalController,
  ) {}

  async ngOnInit() {
    if(!this.stateService.recoveryDrive) {
      this.dataDrives = await this.apiService.getEmbassyDrives()
    } else {
      this.stateService.pollDataTransferProgress()
    }
  }

  selectDrive(disk: EmbassyDrive) {
    const name = disk['logical-name']
    const version = disk.version
    if (name === this.selectedDrive) {
      this.selectedDrive = null
      this.selectedVersion = null
    } else {
      this.selectedDrive = name
      this.selectedVersion = version
    }
  }

  async warn() {
    const alert = await this.alertController.create({
      cssClass: 'my-custom-class',
      header: 'Warning!',
      message: 'Once you select this the recovery process will begin.',
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
    this.presentPasswordModal()
  }

  async presentPasswordModal() {
    const modal = await this.modalController.create({
      component: PasswordPage,
      backdropDismiss: false,
      componentProps: {
        'version': this.selectedVersion,
      }
    })
    modal.onDidDismiss().then(ret => {
      if(ret.data.pwValid) {
        this.submitPWAndDisk(ret.data.password)
      }
    })
    await modal.present();
  }

  async submitPWAndDisk(pw: string) {
    await this.apiService.selectEmbassyDrive({logicalName: this.selectedDrive}, pw)
    this.stateService.recoveryDrive = this.selectedDrive
    this.stateService.pollDataTransferProgress()
  }

  async navToEmbassy() {
    // @TODO nav to embassy
  }

}
