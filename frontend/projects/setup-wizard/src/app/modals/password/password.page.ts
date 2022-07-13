import { Component, Input, ViewChild } from '@angular/core'
import { IonInput, ModalController } from '@ionic/angular'
import {
  DiskInfo,
  CifsBackupTarget,
  DiskBackupTarget,
} from 'src/app/services/api/api.service'
import * as argon2 from '@start9labs/argon2'

@Component({
  selector: 'app-password',
  templateUrl: 'password.page.html',
  styleUrls: ['password.page.scss'],
})
export class PasswordPage {
  @ViewChild('focusInput') elem?: IonInput
  @Input() target?: CifsBackupTarget | DiskBackupTarget
  @Input() storageDrive?: DiskInfo

  pwError = ''
  password = ''
  unmasked1 = false

  verError = ''
  passwordVer = ''
  unmasked2 = false

  constructor(private modalController: ModalController) {}

  ngAfterViewInit() {
    setTimeout(() => this.elem?.setFocus(), 400)
  }

  async verifyPw() {
    if (!this.target || !this.target['embassy-os'])
      this.pwError = 'No recovery target' // unreachable

    try {
      const passwordHash = this.target!['embassy-os']?.['password-hash'] || ''

      argon2.verify(passwordHash, this.password)
      this.modalController.dismiss({ password: this.password }, 'success')
    } catch (e) {
      this.pwError = 'Incorrect password provided'
    }
  }

  async submitPw() {
    this.validate()
    if (this.password !== this.passwordVer) {
      this.verError = '*passwords do not match'
    }

    if (this.pwError || this.verError) return
    this.modalController.dismiss({ password: this.password }, 'success')
  }

  validate() {
    if (!!this.target) return (this.pwError = '')

    if (this.passwordVer) {
      this.checkVer()
    }

    if (this.password.length < 12) {
      this.pwError = 'Must be 12 characters or greater'
    } else {
      this.pwError = ''
    }
  }

  checkVer() {
    this.verError =
      this.password !== this.passwordVer ? 'Passwords do not match' : ''
  }

  cancel() {
    this.modalController.dismiss()
  }
}
