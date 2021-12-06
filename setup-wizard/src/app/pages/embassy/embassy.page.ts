import { Component } from '@angular/core'
import { AlertController, LoadingController, ModalController, NavController } from '@ionic/angular'
import { ApiService, DiskInfo, DiskRecoverySource } from 'src/app/services/api/api.service'
import { ErrorToastService } from 'src/app/services/error-toast.service'
import { StateService } from 'src/app/services/state.service'
import { PasswordPage } from '../../modals/password/password.page'

@Component({
  selector: 'app-embassy',
  templateUrl: 'embassy.page.html',
  styleUrls: ['embassy.page.scss'],
})
export class EmbassyPage {
  storageDrives: DiskInfo[] = []
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
    this.loading = true
    await this.getDrives()
  }

  async getDrives () {
    try {
      const drives = await this.apiService.getDrives()
      this.storageDrives = drives.filter(d => !d.partitions.map(p => p.logicalname).includes((this.stateService.recoverySource as DiskRecoverySource)?.logicalname))
    } catch (e) {
      this.errorToastService.present(e.message)
    } finally {
      this.loading = false
    }
  }

  async chooseDrive (drive: DiskInfo) {
    if (!!drive.partitions.find(p => p.used) || !!drive.guid) {
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
              if (this.stateService.recoveryPassword) {
                this.setupEmbassy(drive, this.stateService.recoveryPassword)
              } else {
                this.presentModalPassword(drive)
              }
            },
          },
        ],
      })
      await alert.present()
    } else {
      if (this.stateService.recoveryPassword) {
        this.setupEmbassy(drive, this.stateService.recoveryPassword)
      } else {
        this.presentModalPassword(drive)
      }
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
      this.setupEmbassy(drive, ret.data.password)
    })
    await modal.present()
  }

  private async setupEmbassy (drive: DiskInfo, password: string): Promise<void> {
    const loader = await this.loadingCtrl.create({
      message: 'Transferring encrypted data. This could take a while...',
    })

    await loader.present()

    try {
      await this.stateService.setupEmbassy(drive.logicalname, password)
      if (!!this.stateService.recoverySource) {
        await this.navCtrl.navigateForward(`/loading`)
      } else {
        await this.navCtrl.navigateForward(`/init`)
      }
    } catch (e) {
      this.errorToastService.present(`${e.message}: ${e.details}. Restart Embassy to try again.`)
      console.error(e)
    } finally {
      loader.dismiss()
    }
  }
}
