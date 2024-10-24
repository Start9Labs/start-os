import { Component } from '@angular/core'
import { AlertController, NavController } from '@ionic/angular'
import { DiskInfo, ErrorService } from '@start9labs/shared'
import { ApiService } from 'src/app/services/api/api.service'
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
    private readonly errorService: ErrorService,
    private readonly stateService: StateService,
  ) {}

  async ngOnInit() {
    this.stateService.setupType = 'transfer'
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
      this.errorService.handleError(e)
    } finally {
      this.loading = false
    }
  }

  async select(guid: string) {
    const alert = await this.alertCtrl.create({
      header: 'Warning',
      message:
        'After transferring data from this drive, <b>do not</b> attempt to boot into it again as a Start9 Server. This may result in services malfunctioning, data corruption, or loss of funds.',
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
            this.navCtrl.navigateForward(`/storage`)
          },
        },
      ],
    })
    await alert.present()
  }
}
