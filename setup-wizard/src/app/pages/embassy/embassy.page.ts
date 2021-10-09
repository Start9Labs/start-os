import { Component } from '@angular/core'
import { AlertController, LoadingController, ModalController, NavController } from '@ionic/angular'
import { ApiService, DiskInfo } from 'src/app/services/api/api.service'
import { ErrorToastService } from 'src/app/services/error-toast.service'
import { StateService } from 'src/app/services/state.service'
import { PasswordPage } from '../password/password.page'

@Component({
  selector: 'app-embassy',
  templateUrl: 'embassy.page.html',
  styleUrls: ['embassy.page.scss'],
})
export class EmbassyPage {
  storageDrives = []
  selectedDrive: DiskInfo = null
  loading = true

  constructor (
    private readonly apiService: ApiService,
    private readonly navCtrl: NavController,
    private readonly modalController: ModalController,
    private readonly alertCtrl: AlertController,
    private readonly stateService: StateService,
    private readonly loadingCtrl: LoadingController,
    private readonly errorToastService: ErrorToastService,
  ) { }

  async ngOnInit () {
    await this.getDrives()
  }

  async refresh () {
    this.storageDrives = []
    this.selectedDrive = null
    this.loading = true
    await this.getDrives()
  }

  async getDrives () {
    try {
      this.storageDrives = (await this.apiService.getDrives()).filter(d => d.logicalname !== this.stateService.recoveryDrive?.logicalname)
    } catch (e) {
      this.errorToastService.present(e.message)
    } finally {
      this.loading = false
    }
  }

  async chooseDrive (drive: DiskInfo) {
    if (!!drive.partitions.find(p => p.used)) {
      const alert = await this.alertCtrl.create({
        header: 'Warning',
        subHeader: 'Drive contains data!',
        message: 'All data stored on this drive will be permanently deleted.',
        buttons: [
          {
            role: 'cancel',
            text: 'Cancel',
          },
          {
            text: 'Continue',
            handler: () => {
              this.presentModalPassword(drive)
            },
          },
        ],
      })
      await alert.present()
    } else {
      this.presentModalPassword(drive)
    }
  }

  private async presentModalPassword (drive: DiskInfo): Promise<void> {
    const modal = await this.modalController.create({
      component: PasswordPage,
      componentProps: {
        storageDrive: drive,
      },
    })
    modal.onDidDismiss().then(async ret => {
      if (!ret.data || !ret.data.password) return

      const loader = await this.loadingCtrl.create({
        message: 'Setting up your Embassy!',
      })

      await loader.present()

      this.stateService.storageDrive = drive
      this.stateService.embassyPassword = ret.data.password

      try {
        this.stateService.torAddress = (await this.stateService.setupEmbassy()).torAddress
      } catch (e) {
        this.errorToastService.present(`${e.message}: ${e.details}`)
        console.error(e.message)
        console.error(e.details)
      } finally {
        loader.dismiss()
        if (!!this.stateService.recoveryDrive) {
          await this.navCtrl.navigateForward(`/loading`, { animationDirection: 'forward' })
        } else {
          await this.navCtrl.navigateForward(`/success`, { animationDirection: 'forward' })
        }
      }
    })
    await modal.present()
  }
}
