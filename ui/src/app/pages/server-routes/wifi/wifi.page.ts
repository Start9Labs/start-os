import { Component } from '@angular/core'
import { ActionSheetController, AlertController, LoadingController, ModalController, ToastController } from '@ionic/angular'
import { AlertInput } from '@ionic/core'
import { ApiService } from 'src/app/services/api/embassy-api.service'
import { ActionSheetButton } from '@ionic/core'
import { ErrorToastService } from 'src/app/services/error-toast.service'
import { ValueSpecObject } from 'src/app/pkg-config/config-types'
import { RR } from 'src/app/services/api/api.types'
import { pauseFor } from 'src/app/util/misc.util'
import { GenericFormPage } from 'src/app/modals/generic-form/generic-form.page'

@Component({
  selector: 'wifi',
  templateUrl: 'wifi.page.html',
  styleUrls: ['wifi.page.scss'],
})
export class WifiPage {
  loading = true
  wifi: RR.GetWifiRes = { } as any
  countries = require('../../../util/countries.json') as { [key: string]: string }

  constructor (
    private readonly api: ApiService,
    private readonly toastCtrl: ToastController,
    private readonly alertCtrl: AlertController,
    private readonly loadingCtrl: LoadingController,
    private readonly modalCtrl: ModalController,
    private readonly errToast: ErrorToastService,
    private readonly actionCtrl: ActionSheetController,
  ) { }

  async ngOnInit () {
    try {
      await this.getWifi()
    } catch (e) {
      this.errToast.present(e)
    } finally {
      this.loading = false
    }
  }

  async getWifi (timeout?: number): Promise<void> {
    this.wifi = await this.api.getWifi({ }, timeout)
    if (!this.wifi.country) {
      await this.presentAlertCountry()
    }
  }

  async presentAlertCountry (): Promise<void> {
    const inputs: AlertInput[] = Object.entries(this.countries).map(([country, fullName]) => {
      return {
        name: fullName,
        type: 'radio',
        label: `${country} - ${fullName}`,
        value: country,
        checked: country === this.wifi.country,
      }
    })

    const alert = await this.alertCtrl.create({
      header: 'Select Country',
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
      cssClass: 'wide-alert enter-click',
    })
    await alert.present()
  }

  async presentModalAdd () {
    const modal = await this.modalCtrl.create({
      component: GenericFormPage,
      componentProps: {
        title: wifiSpec.name,
        spec: wifiSpec.spec,
        buttons: [
          {
            text: 'Save for Later',
            handler: async (value: { ssid: string, password: string }) => {
              await this.save(value.ssid, value.password)
            },
          },
          {
            text: 'Save and Connect',
            handler: async (value: { ssid: string, password: string }) => {
              await this.saveAndConnect(value.ssid, value.password)
            },
            isSubmit: true,
          },
        ],
      },
    })

    await modal.present()
  }

  async presentAction (ssid: string, i: number) {
    const buttons: ActionSheetButton[] = [
      {
        text: 'Forget',
        icon: 'trash',
        role: 'destructive',
        handler: () => {
          this.delete(ssid, i)
        },
      },
    ]

    if (ssid !== this.wifi.connected) {
      buttons.unshift(
        {
          text: 'Connect',
          icon: 'wifi',
          handler: () => {
            this.connect(ssid)
          },
        },
      )
    }

    const action = await this.actionCtrl.create({
      header: ssid,
      subHeader: 'Manage network',
      mode: 'ios',
      buttons,
    })

    await action.present()
  }

  private async setCountry (country: string): Promise<void> {
    const loader = await this.loadingCtrl.create({
      spinner: 'lines',
      cssClass: 'loader',
    })
    await loader.present()

    try {
      await this.api.setWifiCountry({ country })
      this.wifi.country = country
    } catch (e) {
      this.errToast.present(e)
    } finally {
      loader.dismiss()
    }
  }

  private async confirmWifi (ssid: string, deleteOnFailure = false): Promise<void> {
    const timeout = 4000
    const maxAttempts = 5
    let attempts = 0

    while (attempts < maxAttempts) {
      if (attempts > maxAttempts) {
        this.presentToastFail()
        if (deleteOnFailure) {
          this.wifi.ssids = this.wifi.ssids.filter(s => s !== ssid)
        }
        break
      }

      try {
        const start = new Date().valueOf()
        await this.getWifi(timeout)
        const end = new Date().valueOf()
        if (this.wifi.connected === ssid) {
          this.presentAlertSuccess(ssid)
          break
        } else {
          attempts++
          const diff = end - start
          await pauseFor(Math.max(1000, timeout - diff))
        }
      } catch (e) {
        attempts++
        console.error(e)
      }
    }
  }

  private async presentAlertSuccess (ssid: string): Promise<void> {
    const alert = await this.alertCtrl.create({
      header: `Connected to "${ssid}"`,
      message: 'Note. It may take several minutes to an hour for your Embassy to reconnect over Tor.',
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

  private async presentToastFail (): Promise<void> {
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

  private async connect (ssid: string): Promise<void> {
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

  private async delete (ssid: string, i: number): Promise<void> {
    const loader = await this.loadingCtrl.create({
      spinner: 'lines',
      message: 'Deleting...',
      cssClass: 'loader',
    })
    await loader.present()

    try {
      await this.api.deleteWifi({ ssid })
      this.wifi.ssids = this.wifi.ssids.filter((w, index) => index !== i)
    } catch (e) {
      this.errToast.present(e)
    } finally {
      loader.dismiss()
    }
  }

  private async save (ssid: string, password: string): Promise<void> {
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

  private async saveAndConnect (ssid: string, password: string): Promise<void> {
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

const wifiSpec: ValueSpecObject = {
  type: 'object',
  name: 'WiFi Credentials',
  description: 'Enter the network SSID and password. You can connect now or save the network for later.',
  'unique-by': null,
  spec: {
    ssid: {
      type: 'string',
      name: 'Network SSID',
      nullable: false,
      masked: false,
      copyable: false,
    },
    password: {
      type: 'string',
      name: 'Password',
      nullable: false,
      masked: true,
      copyable: false,
    },
  },
}
