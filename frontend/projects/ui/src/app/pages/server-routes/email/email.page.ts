import { Component } from '@angular/core'
import { PatchDbService } from 'src/app/services/patch-db/patch-db.service'
import {
  AlertController,
  LoadingController,
  ModalController,
} from '@ionic/angular'
import {
  GenericInputComponent,
  GenericInputOptions,
} from 'src/app/modals/generic-input/generic-input.component'
import { ApiService } from 'src/app/services/api/embassy-api.service'
import { getServerInfo } from 'src/app/util/get-server-info'
import { ErrorToastService } from '@start9labs/shared'
import { ServerConfigService } from 'src/app/services/server-config.service'

@Component({
  selector: 'email',
  templateUrl: './email.page.html',
  styleUrls: ['./email.page.scss'],
})
export class EmailPage {
  readonly ui$ = this.patch.watch$('ui')
  readonly server$ = this.patch.watch$('server-info')

  constructor(
    private readonly loadingCtrl: LoadingController,
    private readonly modalCtrl: ModalController,
    private readonly alertCtrl: AlertController,
    private readonly patch: PatchDbService,
    private readonly api: ApiService,
    private readonly errToast: ErrorToastService,
    readonly serverConfig: ServerConfigService,
  ) {}

  async presentModalEmail(): Promise<void> {
    const options: GenericInputOptions = {
      title: 'Email Address',
      message:
        'Enter an email address to receive email notificaitons from your Embassy.',
      label: 'Email Address',
      nullable: true,
      initialValue: await getServerInfo(this.patch).then(s => s.email.address),
      buttonText: 'Save',
      submitFn: async (value: string) => {
        await this.saveEmail(value)
        this.presentAlertEmailSaved(value)
      },
    }

    const modal = await this.modalCtrl.create({
      componentProps: { options },
      cssClass: 'alertlike-modal',
      presentingElement: await this.modalCtrl.getTop(),
      component: GenericInputComponent,
    })

    await modal.present()
  }

  private async saveEmail(address: string) {
    const loader = await this.loadingCtrl.create({
      message: `Sending test email...`,
    })
    await loader.present()

    try {
      await this.api.restartServer({ address })
    } catch (e: any) {
      this.errToast.present(e)
    } finally {
      loader.dismiss()
    }
  }

  private async presentAlertEmailSaved(address: string) {
    const alert = await this.alertCtrl.create({
      header: 'Success',
      message: `A test email has been sent to ${address}.<br /><br /><b>Check your spam folder and mark as not spam</b>`,
      buttons: [
        {
          text: 'OK',
          cssClass: 'enter-click',
        },
      ],
      cssClass: 'alert-success-message',
    })
    await alert.present()
  }
}
