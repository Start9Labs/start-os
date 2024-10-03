import { Component, Input } from '@angular/core'
import {
  AlertController,
  ModalController,
  ToastController,
} from '@ionic/angular'
import { copyToClipboard } from '@start9labs/shared'
import { T } from '@start9labs/start-sdk'
import { QRComponent } from 'src/app/components/qr/qr.component'
import { RR } from 'src/app/services/api/api.types'

@Component({
  selector: 'action-success',
  templateUrl: './action-success.page.html',
  styleUrls: ['./action-success.page.scss'],
})
export class ActionSuccessPage {
  @Input()
  response!: NonNullable<RR.ActionRes>

  constructor(
    private readonly modalCtrl: ModalController,
    private readonly toastCtrl: ToastController,
    private readonly alertCtrl: AlertController,
  ) {}

  async presentDescription(
    value: T.ActionResultV1 & { type: 'string' },
    e: Event,
  ) {
    e.stopPropagation()

    const alert = await this.alertCtrl.create({
      header: value.name,
      message: value.description || undefined,
    })
    await alert.present()
  }

  async copy(text: string) {
    let message = ''
    await copyToClipboard(text).then(success => {
      message = success
        ? 'Copied to clipboard!'
        : 'Failed to copy to clipboard.'
    })

    const toast = await this.toastCtrl.create({
      header: message,
      position: 'bottom',
      duration: 1000,
    })
    await toast.present()
  }

  async showQR(text: string): Promise<void> {
    const modal = await this.modalCtrl.create({
      component: QRComponent,
      componentProps: {
        text,
      },
      cssClass: 'qr-modal',
    })
    await modal.present()
  }

  async dismiss() {
    return this.modalCtrl.dismiss()
  }
}
