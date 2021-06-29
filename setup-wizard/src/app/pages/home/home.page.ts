import { Component } from '@angular/core'
import { AlertController, NavController } from '@ionic/angular'
import { ApiService } from 'src/app/services/api/api.service'
import { StateService } from 'src/app/services/state.sevice'

@Component({
  selector: 'app-home',
  templateUrl: 'home.page.html',
  styleUrls: ['home.page.scss'],
})
export class HomePage {
  dataDrives = []
  selectedDrive = null

  constructor(
    private readonly apiService: ApiService,
    private readonly stateService: StateService,
    public alertController: AlertController,
    private readonly navCtrl: NavController,
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

}
