import { Component } from '@angular/core'
import { iosTransitionAnimation, ModalController, NavController } from '@ionic/angular'
import { ApiService, RecoveryDrive } from 'src/app/services/api/api.service'
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
  selectedDrive: RecoveryDrive = null
  loading = true

  constructor(
    private readonly apiService: ApiService,
    private readonly navCtrl: NavController,
    private modalController: ModalController,
    private stateService: StateService
  ) {}

  async ngOnInit() {
    this.recoveryDrives = await this.apiService.getRecoveryDrives()
    this.loading = false
  }

  async chooseDrive(drive: RecoveryDrive) {

    if (this.selectedDrive?.logicalname === drive.logicalname) {
      this.selectedDrive = null
      return
    } else {
      this.selectedDrive = drive
    }

    if (drive.version.startsWith('0.2') || this.passwords[drive.logicalname]) return

    const modal = await this.modalController.create({
      component: PasswordPage,
      componentProps: {
        recoveryDrive: this.selectedDrive
      }
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
