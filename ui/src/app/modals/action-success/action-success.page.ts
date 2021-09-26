import { Component, Input } from '@angular/core'
import { ModalController, ToastController } from '@ionic/angular'
import { ActionResponse } from 'src/app/services/api/api.types'
import { copyToClipboard } from 'src/app/util/web.util'

@Component({
  selector: 'action-success',
  templateUrl: './action-success.page.html',
  styleUrls: ['./action-success.page.scss'],
})
export class ActionSuccessPage {

  @Input() actionRes: ActionResponse

  constructor (
    private readonly modalCtrl: ModalController,
    private readonly toastCtrl: ToastController,
  ) { }

  async copy (address: string) {
    let message = ''
    await copyToClipboard(address || '')
      .then(success => { message = success ? 'copied to clipboard!' : 'failed to copy'})

    const toast = await this.toastCtrl.create({
      header: message,
      position: 'bottom',
      duration: 1000,
    })
    await toast.present()
  }

  async dismiss () {
    return this.modalCtrl.dismiss()
  }
}
