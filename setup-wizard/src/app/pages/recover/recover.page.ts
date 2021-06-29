import { Component } from '@angular/core'
import { AlertController, NavController } from '@ionic/angular'
import { ApiService } from 'src/app/services/api/api.service'
import { StateService } from 'src/app/services/state.sevice'

@Component({
  selector: 'recover-page',
  templateUrl: 'recover.page.html',
  styleUrls: ['recover.page.scss'],
})
export class RecoverPage {
  dataDrives = []
  selectedDrive = null

  constructor(
    private readonly apiService: ApiService,
    private readonly stateService: StateService,
    public alertController: AlertController,
  ) {}

  async ngOnInit() {
    if(!this.stateService.recoveryDrive) {
      this.dataDrives = await this.apiService.getEmbassyDrives()
    } else {
      this.stateService.pollDataTransferProgress()
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
    await this.apiService.selectEmbassyDrive({logicalName: this.selectedDrive})
    this.stateService.recoveryDrive = this.selectedDrive
    this.stateService.pollDataTransferProgress()
  }

}
