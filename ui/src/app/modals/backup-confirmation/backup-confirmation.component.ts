import { Component, Input } from '@angular/core'
import { ModalController } from '@ionic/angular'

@Component({
  selector: 'backup-confirmation',
  templateUrl: './backup-confirmation.component.html',
  styleUrls: ['./backup-confirmation.component.scss'],
})
export class BackupConfirmationComponent {
  @Input() type: 'backup' | 'restore'
  unmasked = false
  password = ''
  error = ''

  constructor (
    private readonly modalCtrl: ModalController,
  ) { }

  toggleMask () {
    this.unmasked = !this.unmasked
  }

  cancel () {
    this.modalCtrl.dismiss({ cancel: true })
  }

  submit () {
    if (!this.password || this.password.length < 12) {
      this.error = 'Password must be at least 12 characters in length.'
      return
    }
    const { password } = this
    this.modalCtrl.dismiss({ password })
  }
}
