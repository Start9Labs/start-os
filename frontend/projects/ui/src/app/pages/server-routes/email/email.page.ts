import { Component } from '@angular/core'
import { AlertController, LoadingController } from '@ionic/angular'
import { ApiService } from 'src/app/services/api/embassy-api.service'
import { ErrorToastService } from '@start9labs/shared'
import { PatchDB } from 'patch-db-client'
import { DataModel } from 'src/app/services/patch-db/data-model'
import { UntypedFormGroup } from '@angular/forms'
import { FormService } from 'src/app/services/form.service'
import { InputSpec } from 'start-sdk/types/config-types'

@Component({
  selector: 'email',
  templateUrl: './email.page.html',
  styleUrls: ['./email.page.scss'],
})
export class EmailPage {
  readonly email$ = this.patch.watch$('server-info', 'email')

  configForm?: UntypedFormGroup

  readonly configSpec: InputSpec = {
    enabled: {
      type: 'boolean',
      name: 'Enable Email Notifications',
      description:
        'Whether or not to receive email notifications from your Embassy',
      warning: null,
      default: false,
    },
    address: {
      type: 'string',
      name: 'Receive Address',
      description: 'The address you want to receive email notifications',
      warning: null,
      placeholder: 'e.g. you@protonmail.com',
      nullable: false,
      textarea: false,
      masked: false,
      pattern: null,
      'pattern-description': null,
      default: null,
    },
    smtp: {
      type: 'object',
      name: 'SMTP Settings',
      description: 'Settings and credentials for your chosen SMTP server',
      warning: null,
      spec: smtpSpec,
    },
  }

  constructor(
    private readonly loadingCtrl: LoadingController,
    private readonly alertCtrl: AlertController,
    private readonly patch: PatchDB<DataModel>,
    private readonly api: ApiService,
    private readonly errToast: ErrorToastService,
    private readonly formService: FormService,
  ) {}

  ngOnInit() {
    this.configForm = this.formService.createForm(this.configSpec!)
  }

  async saveConfig(): Promise<boolean> {
    const loader = await this.loadingCtrl.create({
      message: `Saving...`,
    })
    await loader.present()

    try {
      await this.api.configureEmail(this.configForm!.value)
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

const smtpSpec: InputSpec = {
  host: {
    type: 'string',
    name: 'Host',
    description: 'Hostname of the SMTP server',
    placeholder: 'e.g. smtp.mailgun.org',
    nullable: false,
    masked: false,
    warning: null,
    textarea: false,
    pattern: null,
    'pattern-description': null,
    default: null,
  },
  port: {
    type: 'number',
    name: 'Port',
    description: 'Port of the SMTP server',
    warning: null,
    placeholder: 'e.g. 587',
    nullable: false,
    range: '*',
    units: null,
    integral: true,
    default: null,
  },
  from: {
    type: 'string',
    name: 'From Address',
    description: 'The address that will send the emails',
    placeholder: 'First  Last <email@example.com>',
    nullable: false,
    masked: false,
    warning: null,
    textarea: false,
    pattern: null,
    'pattern-description': null,
    default: null,
  },
  login: {
    type: 'string',
    name: 'Login',
    description: 'Login username for SMTP server',
    nullable: false,
    masked: false,
    warning: null,
    placeholder: 'e.g. you@protonmail.com',
    textarea: false,
    pattern: null,
    'pattern-description': null,
    default: null,
  },
  password: {
    type: 'string',
    name: 'Password',
    description: 'Password username for SMTP server',
    nullable: false,
    masked: true,
    warning: null,
    placeholder: 'e.g. you@protonmail.com',
    textarea: false,
    pattern: null,
    'pattern-description': null,
    default: null,
  },
  tls: {
    type: 'boolean',
    name: 'Enable TLS',
    description: 'Whether or not to enable TLS certificate security checks',
    warning: null,
    default: true,
  },
}
