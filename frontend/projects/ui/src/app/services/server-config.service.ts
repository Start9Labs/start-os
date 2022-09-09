import { Injectable } from '@angular/core'
import { AlertInput, AlertButton } from '@ionic/core'
import { ApiService } from './api/embassy-api.service'
import { ConfigSpec } from 'src/app/pkg-config/config-types'
import { AlertController, LoadingController } from '@ionic/angular'
import { ErrorToastService } from '@start9labs/shared'

@Injectable({
  providedIn: 'root',
})
export class ServerConfigService {
  constructor(
    private readonly loadingCtrl: LoadingController,
    private readonly errToast: ErrorToastService,
    private readonly alertCtrl: AlertController,
    private readonly embassyApi: ApiService,
  ) {}

  async presentAlert(
    key: string,
    current?: any,
  ): Promise<HTMLIonAlertElement | null> {
    const spec = serverConfig[key]

    let inputs: AlertInput[]
    let buttons: AlertButton[] = [
      {
        text: 'Cancel',
        role: 'cancel',
      },
      {
        text: 'Save',
        handler: async (data: any) => {
          const loader = await this.loadingCtrl.create({
            message: 'Saving...',
          })
          loader.present()

          try {
            await this.saveFns[key](data)
          } catch (e: any) {
            this.errToast.present(e)
          } finally {
            loader.dismiss()
          }
        },
        cssClass: 'enter-click',
      },
    ]

    switch (spec.type) {
      case 'boolean':
        inputs = [
          {
            name: 'enabled',
            type: 'radio',
            label: 'Enabled',
            value: true,
            checked: current,
          },
          {
            name: 'disabled',
            type: 'radio',
            label: 'Disabled',
            value: false,
            checked: !current,
          },
        ]
        break
      default:
        return null
    }

    const alert = await this.alertCtrl.create({
      header: spec.name,
      message: spec.description,
      inputs,
      buttons,
    })
    await alert.present()
    return alert
  }

  // async presentModalForm (key: string) {
  //   const modal = await this.modalCtrl.create({
  //     component: AppActionInputPage,
  //     componentProps: {
  //       title: serverConfig[key].name,
  //       spec: (serverConfig[key] as ValueSpecObject).spec,
  //     },
  //   })

  //   modal.onWillDismiss().then(res => {
  //     if (!res.data) return
  //     this.saveFns[key](res.data)
  //   })

  //   await modal.present()
  // }

  saveFns: { [key: string]: (val: any) => Promise<any> } = {
    'auto-check-updates': async (enabled: boolean) => {
      return this.embassyApi.setDbValue({
        pointer: '/auto-check-updates',
        value: enabled,
      })
    },
    'email-notifications-os': async (enabled: boolean) => {
      return this.embassyApi.configureEmail({ notifications: { os: enabled } })
    },
    'email-notifications-services': async (enabled: boolean) => {
      return this.embassyApi.configureEmail({
        notifications: { services: enabled },
      })
    },
  }
}

export const serverConfig: ConfigSpec = {
  'auto-check-updates': {
    type: 'boolean',
    name: 'Auto Check for Updates',
    description:
      'If enabled, EmbassyOS will automatically check for updates of itself and installed services. Updating will still require your approval and action. Updates will never be performed automatically.',
    default: true,
  },
  'email-notifications-os': {
    type: 'boolean',
    name: 'Receive OS Notifications',
    description:
      'If enabled, EmbassyOS will deliver an email to your saved address whenever a notificaiton is issued.',
    default: true,
  },
  'email-notifications-services': {
    type: 'boolean',
    name: 'Permit Usage By Services',
    description:
      'If enabled, installed services will be able to use your EmbassyOS SMTP server to send mail for their own purposes.',
    default: true,
  },
}