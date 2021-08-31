import { Component, Input } from '@angular/core'
import { IonicSafeString, LoadingController, ModalController } from '@ionic/angular'
import { ApiService } from 'src/app/services/api/embassy-api.service'
import { ErrorToastService, getErrorMessage } from 'src/app/services/error-toast.service'

@Component({
  selector: 'backup-confirmation',
  templateUrl: './backup-confirmation.component.html',
  styleUrls: ['./backup-confirmation.component.scss'],
})
export class BackupConfirmationComponent {
  @Input() title: string
  @Input() message: string
  @Input() label = 'Enter value'
  @Input() buttonText = 'Submit'
  @Input() useMask = false
  @Input() value = ''
  @Input() submitFn: (value: string) => Promise<any>
  unmasked = false
  error: string | IonicSafeString

  constructor (
    private readonly modalCtrl: ModalController,
    private readonly loadingCtrl: LoadingController,
  ) { }

  toggleMask () {
    this.unmasked = !this.unmasked
  }

  cancel () {
    this.modalCtrl.dismiss()
  }

  async submit () {
    const loader = await this.loadingCtrl.create({
      spinner: 'lines',
      cssClass: 'loader',
    })
    loader.present()

    try {
      await this.submitFn(this.value)
      this.modalCtrl.dismiss(undefined, 'success')
    } catch (e) {
      this.error = getErrorMessage(e)
    } finally {
      loader.dismiss()
    }
  }
}
