import { Component } from '@angular/core'
import { AlertController, LoadingController, ModalController, NavController } from '@ionic/angular'
import { ApiService, DiskInfo, PartitionInfo } from 'src/app/services/api/api.service'
import { ErrorToastService } from 'src/app/services/error-toast.service'
import { StateService } from 'src/app/services/state.service'
import { PasswordPage } from '../password/password.page'
import { ProdKeyModal } from '../prod-key-modal/prod-key-modal.page'

@Component({
  selector: 'app-recover',
  templateUrl: 'recover.page.html',
  styleUrls: ['recover.page.scss'],
})
export class RecoverPage {
  selectedPartition: PartitionInfo = null
  loading = true
  drives: DiskInfo[] = []
  hasShownGuidAlert = false

  constructor (
    private readonly apiService: ApiService,
    private readonly navCtrl: NavController,
    private readonly modalController: ModalController,
    private readonly alertCtrl: AlertController,
    private readonly loadingCtrl: LoadingController,
    readonly stateService: StateService,
    private readonly errorToastService: ErrorToastService,
  ) { }

  async ngOnInit () {
    await this.getDrives()
  }

  async refresh () {
    this.selectedPartition = null
    this.loading = true
    await this.getDrives()
  }

  partitionClickable (partition: PartitionInfo) {
    return partition['embassy-os']?.full && ((!this.stateService.hasProductKey && partition['embassy-os']?.version.startsWith('0.2') ) || this.stateService.hasProductKey)
  }

  async getDrives () {
    try {
      this.drives = await this.apiService.getDrives()

      const importableDrive = this.drives.filter(d => !!d.guid)[0]
      if (!!importableDrive && !this.hasShownGuidAlert) {
        const alert = await this.alertCtrl.create({
          header: 'Warning',
          subHeader: 'Drive contains data!',
          message: 'All data stored on this drive will be permanently deleted.',
          buttons: [
            {
              role: 'cancel',
              text: 'Dismiss',
            },
            {
              text: 'Use',
              handler: async () => {
                await this.importDrive(importableDrive.guid)
              },
            },
          ],
        })
        await alert.present()
        this.hasShownGuidAlert = true
      }
    } catch (e) {
      this.errorToastService.present(`${e.message}: ${e.data}`)
    } finally {
      this.loading = false
    }
  }

  async importDrive (guid: string) {
    const loader = await this.loadingCtrl.create({
      message: 'Importing Drive',
    })
    await loader.present()
    try {
      await this.stateService.importDrive(guid)
      await this.navCtrl.navigateForward(`/success`)
    } catch (e) {
      this.errorToastService.present(`${e.message}: ${e.data}`)
    } finally {
      loader.dismiss()
    }
  }

  async choosePartition (partition: PartitionInfo) {
    this.selectedPartition = partition

    if (partition['embassy-os'].version.startsWith('0.2')) {
      return this.selectRecoveryPartition()
    }

    if (this.stateService.hasProductKey) {
      const modal = await this.modalController.create({
        component: PasswordPage,
        componentProps: {
          recoveryPartition: this.selectedPartition,
        },
        cssClass: 'alertlike-modal',
      })
      modal.onDidDismiss().then(async ret => {
        if (!ret.data) {
          this.selectedPartition = null
        } else if (ret.data.password) {
          this.selectRecoveryPartition(ret.data.password)
        }

      })
      await modal.present()
    } else {
      const modal = await this.modalController.create({
        component: ProdKeyModal,
        componentProps: {
          recoveryPartition: this.selectedPartition,
        },
        cssClass: 'alertlike-modal',
      })
      modal.onDidDismiss().then(async ret => {
        if (!ret.data) {
          this.selectedPartition = null
        } else if (ret.data.productKey) {
          this.selectRecoveryPartition()
        }

      })
      await modal.present()
    }
  }

  async selectRecoveryPartition (password?: string) {
    this.stateService.recoveryPartition = this.selectedPartition
    if (password) {
      this.stateService.recoveryPassword = password
    }
    await this.navCtrl.navigateForward(`/embassy`)
  }
}
