import { Injectable } from '@angular/core'
import { AlertInput, AlertButton, IonicSafeString } from '@ionic/core'
import { ApiService } from './api/embassy-api.service'
import { ConfigSpec } from '../pkg-config/config-types'
import { AlertController, LoadingController } from '@ionic/angular'
import { ErrorToastService } from './error-toast.service'

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

  async presentAlert(key: string, current?: any): Promise<HTMLIonAlertElement> {
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
            spinner: 'lines',
            message: 'Saving...',
            cssClass: 'loader',
          })
          loader.present()

          try {
            await this.saveFns[key](data)
          } catch (e) {
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
        return
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
    'share-stats': async (enabled: boolean) => {
      return this.embassyApi.setShareStats({ value: enabled })
    },
  }
}

export const serverConfig: ConfigSpec = {
  'auto-check-updates': {
    type: 'boolean',
    name: 'Auto Check for Updates',
    description:
      'If enabled, EmbassyOS will automatically check for updates of itself and any installed services. Updating will still require your approval and action. Updates will never be performed automatically.',
    default: true,
  },
  'share-stats': {
    type: 'boolean',
    name: 'Report Bugs',
    description: new IonicSafeString(
      `If enabled, generic error codes will be anonymously transmitted over Tor to the Start9 team. This helps us identify and fix bugs quickly. <a href="https://docs.start9.com/user-manual/general/user-preferences/report-bugs.html" target="_blank" rel="noreferrer">Read more</a> `,
    ) as any,
    default: false,
  },
}
