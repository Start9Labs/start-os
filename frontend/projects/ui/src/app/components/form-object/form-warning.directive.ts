import { Directive } from '@angular/core'
import {
  ValueSpec,
  ValueSpecUnion,
} from '@start9labs/start-sdk/lib/config/configTypes'
import { AlertButton, AlertController } from '@ionic/angular'

@Directive({
  selector: '[formWarning]',
  exportAs: 'formWarning',
})
export class FormWarningDirective {
  private warned = false

  constructor(private readonly alertCtrl: AlertController) {}

  async onChange<T extends ValueSpec>(
    key: string,
    spec: T extends ValueSpecUnion ? never : T,
    okFn?: Function,
    cancelFn?: Function,
  ) {
    if (!spec.warning || this.warned) return okFn ? okFn() : null

    this.warned = true

    const buttons: AlertButton[] = [
      {
        text: 'Ok',
        handler: () => {
          if (okFn) okFn()
        },
        cssClass: 'enter-click',
      },
    ]

    if (okFn || cancelFn) {
      buttons.unshift({
        text: 'Cancel',
        handler: () => {
          if (cancelFn) cancelFn()
        },
      })
    }

    const alert = await this.alertCtrl.create({
      header: 'Warning',
      subHeader: `Editing ${spec.name} has consequences:`,
      message: spec.warning,
      buttons,
    })
    await alert.present()
  }
}
