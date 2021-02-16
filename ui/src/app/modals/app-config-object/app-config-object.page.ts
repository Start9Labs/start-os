import { Component, Input } from '@angular/core'
import { ModalController, AlertController } from '@ionic/angular'
import { ConfigCursor } from 'src/app/pkg-config/config-cursor'
import { ValueSpecObject } from 'src/app/pkg-config/config-types'

@Component({
  selector: 'app-config-object',
  templateUrl: './app-config-object.page.html',
  styleUrls: ['./app-config-object.page.scss'],
})
export class AppConfigObjectPage {
  @Input() cursor: ConfigCursor<'object'>
  spec: ValueSpecObject
  value: object
  error: string

  constructor (
    private readonly modalCtrl: ModalController,
    private readonly alertCtrl: AlertController,
  ) { }

  ngOnInit () {
    this.spec = this.cursor.spec()
    this.value = this.cursor.config()
    this.error = this.cursor.checkInvalid()
  }

  async dismiss (nullify = false) {
    this.modalCtrl.dismiss(nullify ? null : this.value)
  }

  async presentAlertDestroy () {
    const alert = await this.alertCtrl.create({
      backdropDismiss: false,
      header: 'Caution',
      message: 'Are you sure you want to delete this record?',
      buttons: [
        {
          text: 'Cancel',
          role: 'cancel',
        },
        {
          text: 'Delete',
          cssClass: 'alert-danger',
          handler: () => {
            this.dismiss(true)
          },
        },
      ],
    })
    await alert.present()
  }

  handleObjectEdit () {
    this.error = this.cursor.checkInvalid()
  }
}
