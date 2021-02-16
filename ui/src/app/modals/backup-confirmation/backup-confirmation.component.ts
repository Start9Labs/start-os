import { Component, Input } from '@angular/core'
import { ModalController } from '@ionic/angular'
import { PartitionInfo } from 'src/app/services/api/api-types'

@Component({
  selector: 'backup-confirmation',
  templateUrl: './backup-confirmation.component.html',
  styleUrls: ['./backup-confirmation.component.scss'],
})
export class BackupConfirmationComponent {
  @Input() name: string
  unmasked = false
  password: string
  message: string
  error = ''

  constructor (
    private readonly modalCtrl: ModalController,
  ) { }

  ngOnInit () {
    this.message = `Enter your master password to create an encrypted backup on "${this.name}".`
  }

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
