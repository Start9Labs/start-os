import { Component } from '@angular/core'
import { ModalController, NavController } from '@ionic/angular'
import { ApiService, DiskInfo } from 'src/app/services/api/api.service'
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
  passwords = { }
  prodKeys = { }
  recoveryDrives = []
  selectedDrive: DiskInfo = null
  loading = true

  constructor (
    private readonly apiService: ApiService,
    private readonly navCtrl: NavController,
    private readonly modalController: ModalController,
    readonly stateService: StateService,
    private readonly errorToastService: ErrorToastService,
  ) { }

  async ngOnInit () {
    await this.getDrives()
  }

  async refresh () {
    this.recoveryDrives = []
    this.selectedDrive = null
    this.loading = true
    await this.getDrives()
  }

  async getDrives () {
    try {
      let drives = (await this.apiService.getDrives()).filter(d => !!d['embassy-os'])

      if (!this.stateService.hasProductKey) {
        drives = drives.filter(d => d['embassy-os'].version.startsWith('0.2'))
      }

      this.recoveryDrives = drives

    } catch (e) {
      this.errorToastService.present(`${e.message}: ${e.data}`)
    } finally {
      this.loading = false
    }
  }

  async chooseDrive (drive: DiskInfo) {

    if (this.selectedDrive?.logicalname === drive.logicalname) {
      this.selectedDrive = null
      return
    } else {
      this.selectedDrive = drive
    }

    if ((drive['embassy-os'].version.startsWith('0.2') && this.stateService.hasProductKey) || this.passwords[drive.logicalname] || this.prodKeys[drive.logicalname]) return

    if (this.stateService.hasProductKey) {
      const modal = await this.modalController.create({
        component: PasswordPage,
        componentProps: {
          recoveryDrive: this.selectedDrive,
        },
        cssClass: 'alertlike-modal',
      })
      modal.onDidDismiss().then(async ret => {
        if (!ret.data) {
          this.selectedDrive = null
        } else if (ret.data.password) {
          this.passwords[drive.logicalname] = ret.data.password
        }

      })
      await modal.present()
    } else {
      const modal = await this.modalController.create({
        component: ProdKeyModal,
        componentProps: {
          recoveryDrive: this.selectedDrive,
        },
        cssClass: 'alertlike-modal',
      })
      modal.onDidDismiss().then(async ret => {
        if (!ret.data) {
          this.selectedDrive = null
        } else if (ret.data.productKey) {
          this.prodKeys[drive.logicalname] = ret.data.productKey
        }

      })
      await modal.present()
    }
  }

  async selectRecoveryDrive () {
    this.stateService.recoveryDrive = this.selectedDrive
    const pw = this.passwords[this.selectedDrive.logicalname]
    if (pw) {
      this.stateService.recoveryPassword = pw
    }
    await this.navCtrl.navigateForward(`/embassy`)
  }
}
