import { Component } from '@angular/core'
import { AlertController, NavController } from '@ionic/angular'
import { ApiService } from 'src/app/services/api/api.service'
import { DiskInfo, ErrorToastService } from '@start9labs/shared'
import { StateService } from 'src/app/services/state.service'

@Component({
  selector: 'app-transfer',
  templateUrl: 'transfer.page.html',
  styleUrls: ['transfer.page.scss'],
})
export class TransferPage {
  loading = true
  drives: DiskInfo[] = []

  constructor(
    private readonly apiService: ApiService,
    private readonly navCtrl: NavController,
    private readonly alertCtrl: AlertController,
    private readonly errToastService: ErrorToastService,
    private readonly stateService: StateService,
  ) {}

  async ngOnInit() {
    await this.getDrives()
  }

  async refresh() {
    this.loading = true
    await this.getDrives()
  }

  async getDrives() {
    try {
      this.drives = await this.apiService.getDrives()
    } catch (e: any) {
      this.errToastService.present(e)
    } finally {
      this.loading = false
    }
  }

  async select(guid: string) {
    const alert = await this.alertCtrl.create({
      header: 'Warning',
      message:
        'After transferring data from this drive, <b>do not</b> use it again as an Embassy. This may result in services malfunctioning, data corruption, or loss of funds.',
      buttons: [
        {
          role: 'cancel',
          text: 'Cancel',
        },
        {
          text: 'Continue',
          handler: () => {
            this.stateService.recoverySource = {
              type: 'migrate',
              guid,
            }
            this.navCtrl.navigateForward(`/embassy`)
          },
        },
      ],
    })
    await alert.present()
  }
}
