import { Component } from '@angular/core'
import {
  AlertController,
  LoadingController,
  ModalController,
} from '@ionic/angular'
import { ApiService } from 'src/app/services/api/embassy-api.service'
import { ErrorToastService } from '@start9labs/shared'
import { ServerConfigService } from 'src/app/services/server-config.service'
import { PatchDB } from 'patch-db-client'
import { DataModel } from 'src/app/services/patch-db/data-model'
import {
  GenericFormOptions,
  GenericFormPage,
} from 'src/app/modals/generic-form/generic-form.page'
import { ConfigSpec } from 'src/app/pkg-config/config-types'
import { RR, SMTP } from 'src/app/services/api/api.types'

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
    private readonly patch: PatchDB<DataModel>,
    private readonly api: ApiService,
    private readonly errToast: ErrorToastService,
    readonly serverConfig: ServerConfigService,
  ) {}

  async presentModalEmail(): Promise<void> {
    const options: GenericFormOptions = {
      title: 'Configure SMTP',
      spec: smtpSpec,
      buttons: [
        {
          text: 'Save',
          handler: async creds => {
            return this.saveConfig(creds)
          },
          isSubmit: true,
        },
      ],
    }

    const modal = await this.modalCtrl.create({
      componentProps: options,
      presentingElement: await this.modalCtrl.getTop(),
      component: GenericFormPage,
    })

    await modal.present()
  }

  private async saveConfig(smtp: SMTP): Promise<boolean> {
    const loader = await this.loadingCtrl.create({
      message: `Saving...`,
    })
    await loader.present()

    try {
      await this.api.configureEmail({ smtp })
      return true
    } catch (e: any) {
      this.errToast.present(e)
      return false
    } finally {
      loader.dismiss()
    }
  }

  private async sendTestEmail(address: string) {
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

const smtpSpec: ConfigSpec = {
  'smtp-server': {
    type: 'string',
    name: 'SMTP Server',
    description: 'Hostname of the SMTP server',
    placeholder: 'e.g. smtp.mailgun.org',
    nullable: false,
    masked: false,
    copyable: false,
  },
  port: {
    type: 'number',
    name: 'Port',
    description: 'Port of the SMTP server',
    placeholder: 'e.g. 587',
    nullable: false,
    range: '*',
    integral: true,
  },
  from: {
    type: 'string',
    name: 'From Address',
    description: 'The address that will send the emails',
    placeholder: 'First  Last <email@example.com>',
    nullable: false,
    masked: false,
    copyable: false,
  },
  login: {
    type: 'string',
    name: 'Login',
    description: 'Login username for SMTP server',
    nullable: false,
    masked: false,
    copyable: false,
  },
  password: {
    type: 'string',
    name: 'Password',
    description: 'Password username for SMTP server',
    nullable: false,
    masked: true,
    copyable: false,
  },
  tls: {
    type: 'boolean',
    name: 'Enable TLS',
    description: 'Whether or not to enable TLS certificate security checks',
    default: true,
  },
}
