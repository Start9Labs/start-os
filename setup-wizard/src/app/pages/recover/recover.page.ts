import { Component } from '@angular/core'
import { iosTransitionAnimation, ModalController, NavController } from '@ionic/angular'
import { ApiService, DiskInfo } from 'src/app/services/api/api.service'
import { ErrorToastService } from 'src/app/services/error-toast.service'
import { StateService } from 'src/app/services/state.service'
import { PasswordPage } from '../password/password.page'

@Component({
  selector: 'app-recover',
  templateUrl: 'recover.page.html',
  styleUrls: ['recover.page.scss'],
})
export class RecoverPage {
  passwords = {}
  recoveryDrives = []
  selectedDrive: DiskInfo = null
  loading = true

  constructor(
    private readonly apiService: ApiService,
    private readonly navCtrl: NavController,
    private readonly  modalController: ModalController,
    private readonly stateService: StateService,
    private readonly errorToastService: ErrorToastService,
  ) {}

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
      this.recoveryDrives = (await this.apiService.getDrives()).filter(d => !!d['embassy_os'])
    } catch (e) {
      this.errorToastService.present(`${e.message}: ${e.data}`)
    } finally {
      this.loading = false
    }
  }

  async chooseDrive(drive: DiskInfo) {

    if (this.selectedDrive?.logicalname === drive.logicalname) {
      this.selectedDrive = null
      return
    } else {
      this.selectedDrive = drive
    }

    if (drive['embassy_os'].version.startsWith('0.2') || this.passwords[drive.logicalname]) return

    const modal = await this.modalController.create({
      component: PasswordPage,
      componentProps: {
        recoveryDrive: this.selectedDrive
      },
      cssClass: 'alertlike-modal',
    })
    modal.onDidDismiss().then(async ret => {
      if (!ret.data) {
        this.selectedDrive = null
      } else if(ret.data.password) {
        this.passwords[drive.logicalname] = ret.data.password
      }
      
    })
    await modal.present();
  }

  async selectRecoveryDrive() {
    this.stateService.recoveryDrive = this.selectedDrive
    const pw = this.passwords[this.selectedDrive.logicalname]
    if(pw) {
      this.stateService.recoveryPassword = pw
    } 
    await this.navCtrl.navigateForward(`/embassy`, { animationDirection: 'forward', animation: iosTransitionAnimation })
  }
}
