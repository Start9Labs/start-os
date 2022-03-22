import { Component } from '@angular/core'
import {
  ActionSheetController,
  AlertController,
  LoadingController,
  ModalController,
  ToastController,
} from '@ionic/angular'
import { AlertInput } from '@ionic/core'
import { ApiService } from 'src/app/services/api/embassy-api.service'
import { ActionSheetButton } from '@ionic/core'
import { ValueSpecObject } from 'src/app/pkg-config/config-types'
import { RR } from 'src/app/services/api/api.types'
import { pauseFor, ErrorToastService } from '@start9labs/shared'
import { GenericFormPage } from 'src/app/modals/generic-form/generic-form.page'
import { ConfigService } from 'src/app/services/config.service'

@Component({
  selector: 'wifi',
  templateUrl: 'wifi.page.html',
  styleUrls: ['wifi.page.scss'],
})
export class WifiPage {
  loading = true
  wifi: RR.GetWifiRes = {} as any
  countries = require('../../../util/countries.json') as {
    [key: string]: string
  }

  constructor(
    private readonly api: ApiService,
    private readonly toastCtrl: ToastController,
    private readonly alertCtrl: AlertController,
    private readonly loadingCtrl: LoadingController,
    private readonly modalCtrl: ModalController,
    private readonly errToast: ErrorToastService,
    private readonly actionCtrl: ActionSheetController,
    private readonly config: ConfigService,
  ) {}

  async ngOnInit() {
    await this.getWifi()
  }

  async getWifi(timeout?: number): Promise<void> {
    this.loading = true
    try {
      this.wifi = await this.api.getWifi({}, timeout)
      if (!this.wifi.country) {
        await this.presentAlertCountry()
      }
    } catch (e) {
      this.errToast.present(e)
    } finally {
      this.loading = false
    }
  }

  async presentAlertCountry(): Promise<void> {
    if (!this.config.isLan) {
      const alert = await this.alertCtrl.create({
        header: 'Cannot Complete Action',
        message:
          'You must be connected to your Emassy via LAN to change the country.',
        buttons: [
          {
            text: 'OK',
            role: 'cancel',
          },
        ],
        cssClass: 'wide-alert enter-click',
      })
      await alert.present()
      return
    }

    const inputs: AlertInput[] = Object.entries(this.countries).map(
      ([country, fullName]) => {
        return {
          name: fullName,
          type: 'radio',
          label: `${country} - ${fullName}`,
          value: country,
          checked: country === this.wifi.country,
        }
      },
    )

    const alert = await this.alertCtrl.create({
      header: 'Select Country',
      subHeader:
        'Warning: Changing the country will delete all saved networks from the Embassy.',
      inputs,
      buttons: [
        {
          text: 'Cancel',
          role: 'cancel',
        },
        {
          text: 'Save',
          handler: async (country: string) => {
            this.setCountry(country)
          },
        },
      ],
      cssClass: 'wide-alert enter-click select-warning',
    })
    await alert.present()
  }

  async presentModalAdd(ssid?: string, needsPW: boolean = true) {
    const wifiSpec = getWifiValueSpec(ssid, needsPW)
    const modal = await this.modalCtrl.create({
      component: GenericFormPage,
      componentProps: {
        title: wifiSpec.name,
        spec: wifiSpec.spec,
        buttons: [
          {
            text: 'Save for Later',
            handler: async (value: { ssid: string; password: string }) => {
              await this.save(value.ssid, value.password)
            },
          },
          {
            text: 'Save and Connect',
            handler: async (value: { ssid: string; password: string }) => {
              await this.saveAndConnect(value.ssid, value.password)
            },
            isSubmit: true,
          },
        ],
      },
    })

    await modal.present()
  }

  async presentAction(ssid: string) {
    const buttons: ActionSheetButton[] = [
      {
        text: 'Forget',
        icon: 'trash',
        role: 'destructive',
        handler: () => {
          this.delete(ssid)
        },
      },
    ]

    if (ssid !== this.wifi.connected) {
      buttons.unshift({
        text: 'Connect',
        icon: 'wifi',
        handler: () => {
          this.connect(ssid)
        },
      })
    }

    const action = await this.actionCtrl.create({
      header: ssid,
      subHeader: 'Manage network',
      mode: 'ios',
      buttons,
    })

    await action.present()
  }

  private async setCountry(country: string): Promise<void> {
    const loader = await this.loadingCtrl.create({
      spinner: 'lines',
      cssClass: 'loader',
    })
    await loader.present()

    try {
      await this.api.setWifiCountry({ country })
      await this.getWifi()
      this.wifi.country = country
    } catch (e) {
      this.errToast.present(e)
    } finally {
      loader.dismiss()
    }
  }

  private async confirmWifi(
    ssid: string,
    deleteOnFailure = false,
  ): Promise<void> {
    const maxAttempts = 5
    let attempts = 0

    while (attempts < maxAttempts) {
      if (attempts > maxAttempts) {
        this.presentToastFail()
        if (deleteOnFailure) {
          delete this.wifi.ssids[ssid]
        }
        break
      }

      try {
        const start = new Date().valueOf()
        await this.getWifi()
        const end = new Date().valueOf()
        if (this.wifi.connected === ssid) {
          this.presentAlertSuccess(ssid)
          break
        } else {
          attempts++
          const diff = end - start
          // depending on the response time, wait a min of 1000 ms, and a max of 4000 ms in between retries.  Both 1000 and 4000 are arbitrary
          await pauseFor(Math.max(1000, 4000 - diff))
        }
      } catch (e) {
        attempts++
        console.error(e)
      }
    }
  }

  private async presentAlertSuccess(ssid: string): Promise<void> {
    const alert = await this.alertCtrl.create({
      header: `Connected to "${ssid}"`,
      message:
        'Note. It may take several minutes to an hour for your Embassy to reconnect over Tor.',
      buttons: [
        {
          text: 'Ok',
          role: 'cancel',
          cssClass: 'enter-click',
        },
      ],
    })

    await alert.present()
  }

  private async presentToastFail(): Promise<void> {
    const toast = await this.toastCtrl.create({
      header: 'Failed to connect:',
      message: `Check credentials and try again`,
      position: 'bottom',
      duration: 4000,
      buttons: [
        {
          side: 'start',
          icon: 'close',
          handler: () => {
            return true
          },
        },
      ],
      cssClass: 'warning-toast',
    })

    await toast.present()
  }

  private async connect(ssid: string): Promise<void> {
    const loader = await this.loadingCtrl.create({
      spinner: 'lines',
      message: 'Connecting. This could take a while...',
      cssClass: 'loader',
    })
    await loader.present()

    try {
      await this.api.connectWifi({ ssid })
      await this.confirmWifi(ssid)
    } catch (e) {
      this.errToast.present(e)
    } finally {
      loader.dismiss()
    }
  }

  private async delete(ssid: string): Promise<void> {
    const loader = await this.loadingCtrl.create({
      spinner: 'lines',
      message: 'Deleting...',
      cssClass: 'loader',
    })
    await loader.present()

    try {
      await this.api.deleteWifi({ ssid })
      await this.getWifi()
      delete this.wifi.ssids[ssid]
    } catch (e) {
      this.errToast.present(e)
    } finally {
      loader.dismiss()
    }
  }

  private async save(ssid: string, password: string): Promise<void> {
    const loader = await this.loadingCtrl.create({
      spinner: 'lines',
      message: 'Saving...',
      cssClass: 'loader',
    })
    await loader.present()

    try {
      await this.api.addWifi({
        ssid,
        password,
        priority: 0,
        connect: false,
      })
      await this.getWifi()
    } catch (e) {
      this.errToast.present(e)
    } finally {
      loader.dismiss()
    }
  }

  private async saveAndConnect(ssid: string, password: string): Promise<void> {
    const loader = await this.loadingCtrl.create({
      spinner: 'lines',
      message: 'Connecting. This could take a while...',
      cssClass: 'loader',
    })
    await loader.present()

    try {
      await this.api.addWifi({
        ssid,
        password,
        priority: 0,
        connect: true,
      })

      await this.confirmWifi(ssid, true)
    } catch (e) {
      this.errToast.present(e)
    } finally {
      loader.dismiss()
    }
  }
}

function getWifiValueSpec(
  ssid?: string,
  needsPW: boolean = true,
): ValueSpecObject {
  return {
    type: 'object',
    name: 'WiFi Credentials',
    description:
      'Enter the network SSID and password. You can connect now or save the network for later.',
    spec: {
      ssid: {
        type: 'string',
        name: 'Network SSID',
        nullable: false,
        masked: false,
        copyable: false,
        default: ssid,
      },
      password: {
        type: 'string',
        name: 'Password',
        nullable: !needsPW,
        masked: true,
        copyable: false,
        pattern: '^.{8,}$',
        'pattern-description': 'Must be longer than 8 characters',
      },
    },
  }
}
