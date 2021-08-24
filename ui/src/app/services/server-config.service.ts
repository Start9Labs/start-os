import { Injectable } from '@angular/core'
import { AlertInput, AlertButton } from '@ionic/core'
import { ApiService } from './api/embassy-api.service'
import { ConfigSpec } from '../pkg-config/config-types'
import { AlertController, LoadingController } from '@ionic/angular'
import { ErrorToastService } from './error-toast.service'

@Injectable({
  providedIn: 'root',
})
export class ServerConfigService {

  constructor (
    private readonly loadingCtrl: LoadingController,
    private readonly errToast: ErrorToastService,
    private readonly alertCtrl: AlertController,
    private readonly embassyApi: ApiService,
  ) { }

  async presentAlert (key: string, current?: any): Promise<void> {
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
      return this.embassyApi.setDbValue({ pointer: '/auto-check-updates', value: enabled })
    },
    // 'eos-marketplace': async () => {
    //   return this.embassyApi.setEosMarketplace()
    // },
    // 'package-marketplace': async (url: string) => {
    //   return this.embassyApi.setPackageMarketplace({ url })
    // },
    'share-stats': async (enabled: boolean) => {
      return this.embassyApi.setShareStats({ value: enabled })
    },
    // password: async (password: string) => {
    //   return this.embassyApi.updatePassword({ password })
    // },
  }
}

export const serverConfig: ConfigSpec = {
  'auto-check-updates': {
    type: 'boolean',
    name: 'Auto Check for Updates',
    description: 'On launch, EmbassyOS will automatically check for updates of itself and your installed services. Updating still requires user approval and action. No updates will ever be performed automatically.',
    default: true,
  },
  // 'eos-marketplace': {
  //   type: 'boolean',
  //   name: 'Tor Only Marketplace',
  //   description: `Use Start9's Tor (instead of clearnet) Marketplace.`,
  //   warning: 'This will result in higher latency and slower download times.',
  //   default: false,
  // },
  // 'package-marketplace': {
  //   type: 'string',
  //   name: 'Package Marketplace',
  //   description: `Use for alternative embassy marketplace. Leave empty to use start9's marketplace.`,
  //   nullable: true,
  //   // @TODO regex for URL
  //   // pattern: '',
  //   'pattern-description': 'Must be a valid URL.',
  //   masked: false,
  //   copyable: false,
  // },
  'share-stats': {
    type: 'boolean',
    name: 'Share Anonymous Statistics',
    description: 'Start9 uses this information to identify bugs quickly and improve EmbassyOS. The information is 100% anonymous and transmitted over Tor.',
    default: false,
  },
  // password: {
  //   type: 'string',
  //   name: 'Change Password',
  //   description: `Your Embassy's master password, used for authentication and disk encryption.`,
  //   nullable: false,
  //   // @TODO regex for 12 chars
  //   // pattern: '',
  //   'pattern-description': 'Must contain at least 12 characters.',
  //   warning: 'If you forget your master password, there is absolutely no way to recover your data. This can result in loss of money! Keep in mind, old backups will still be encrypted by the password used to encrypt them.',
  //   masked: false,
  //   copyable: false,
  // },
}
