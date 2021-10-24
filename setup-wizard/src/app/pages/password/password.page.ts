import { Component, Input } from '@angular/core'
import { LoadingController, ModalController } from '@ionic/angular'
import { ApiService, DiskInfo, PartitionInfo } from 'src/app/services/api/api.service'
import * as argon2 from '@start9labs/argon2'

@Component({
  selector: 'app-password',
  templateUrl: 'password.page.html',
  styleUrls: ['password.page.scss'],
})
export class PasswordPage {
  @Input() recoveryPartition: PartitionInfo
  @Input() storageDrive: DiskInfo

  pwError = ''
  password = ''
  unmasked1 = false

  verError = ''
  passwordVer = ''
  unmasked2 = false

  hasData: boolean

  constructor (
    private modalController: ModalController,
    private apiService: ApiService,
    private loadingCtrl: LoadingController,
  ) { }

  ngOnInit () {
    if (this.storageDrive && this.storageDrive.partitions.find(p => p.used)) {
      this.hasData = true
    }
  }

  async verifyPw () {
    if (!this.recoveryPartition || !this.recoveryPartition['embassy-os']) this.pwError = 'No recovery drive' // unreachable

    try {
      argon2.verify( this.recoveryPartition['embassy-os']['password-hash'], this.password)
      this.modalController.dismiss({ password: this.password })
    } catch (e) {
      this.pwError = 'Incorrect password provided'
    }
  }

  async submitPw () {
    this.validate()
    if (this.password !== this.passwordVer) {
      this.verError = '*passwords do not match'
    }

    if (this.pwError || this.verError) return
    this.modalController.dismiss({ password: this.password })
  }

  validate () {
    if (!!this.recoveryPartition) return this.pwError = ''

    if (this.passwordVer) {
      this.checkVer()
    }

    if (this.password.length < 12) {
      this.pwError = '*password must be 12 characters or greater'
    } else {
      this.pwError = ''
    }
  }

  checkVer () {
    this.verError = this.password !== this.passwordVer ? '*passwords do not match' : ''
  }

  cancel () {
    this.modalController.dismiss()
  }
}
