import { Component, Inject } from '@angular/core'
import {
  ActionSheetController,
  AlertController,
  ToastController,
} from '@ionic/angular'
import { ActionSheetButton, AlertInput } from '@ionic/core'
import { WINDOW } from '@ng-web-apis/common'
import { ErrorService, LoadingService, pauseFor } from '@start9labs/shared'
import { CT } from '@start9labs/start-sdk'
import { TuiDialogOptions } from '@taiga-ui/core'
import { FormComponent, FormContext } from 'src/app/components/form.component'
import { RR } from 'src/app/services/api/api.types'
import { ApiService } from 'src/app/services/api/embassy-api.service'
import { ConfigService } from 'src/app/services/config.service'
import { FormDialogService } from 'src/app/services/form-dialog.service'

export interface WiFiForm {
  ssid: string
  password: string
}

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
    private readonly loader: LoadingService,
    private readonly formDialog: FormDialogService,
    private readonly errorService: ErrorService,
    private readonly actionCtrl: ActionSheetController,
    private readonly config: ConfigService,
    @Inject(WINDOW) private readonly windowRef: Window,
  ) {}

  async ngOnInit() {
    await this.getWifi()
  }

  async openDocs() {
    this.windowRef.open(
      'https://docs.start9.com/user-manual/wifi.html',
      '_blank',
      'noreferrer',
    )
  }

  async getWifi(timeout: number = 0): Promise<void> {
    this.loading = true
    try {
      this.wifi = await this.api.getWifi({}, timeout)
      if (!this.wifi.country) {
        await this.presentAlertCountry()
      }
    } catch (e: any) {
      this.errorService.handleError(e)
    } finally {
      this.loading = false
    }
  }

  async presentAlertCountry(): Promise<void> {
    if (this.config.isTor()) {
      const alert = await this.alertCtrl.create({
        header: 'Cannot Complete Action',
        message:
          'You must be connected to your server via LAN to change the country.',
        buttons: [
          {
            text: 'OK',
            role: 'cancel',
          },
        ],
        cssClass: 'enter-click',
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
        'Warning: Changing the country will delete all saved networks from StartOS.',
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
      cssClass: 'enter-click select-warning',
    })
    await alert.present()
  }

  async presentModalAdd(ssid?: string, needsPW: boolean = true) {
    const wifiSpec = getWifiValueSpec(ssid, needsPW)
    const options: Partial<TuiDialogOptions<FormContext<WiFiForm>>> = {
      label: wifiSpec.name,
      data: {
        spec: wifiSpec.spec,
        buttons: [
          {
            text: 'Save for Later',
            handler: async ({ ssid, password }) => this.save(ssid, password),
          },
          {
            text: 'Save and Connect',
            handler: async ({ ssid, password }) =>
              this.saveAndConnect(ssid, password),
          },
        ],
      },
    }

    this.formDialog.open(FormComponent, options)
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
    const loader = this.loader.open('Setting country...').subscribe()

    try {
      await this.api.setWifiCountry({ country })
      await this.getWifi()
      this.wifi.country = country
    } catch (e: any) {
      this.errorService.handleError(e)
    } finally {
      loader.unsubscribe()
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
        console.warn(e)
      }
    }
  }

  private async presentAlertSuccess(ssid: string): Promise<void> {
    const alert = await this.alertCtrl.create({
      header: `Connected to "${ssid}"`,
      message:
        'Note. It may take several minutes to an hour for StartOS to reconnect over Tor.',
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
    const loader = this.loader
      .open('Connecting. This could take a while...')
      .subscribe()

    try {
      await this.api.connectWifi({ ssid })
      await this.confirmWifi(ssid)
    } catch (e: any) {
      this.errorService.handleError(e)
    } finally {
      loader.unsubscribe()
    }
  }

  private async delete(ssid: string): Promise<void> {
    const loader = this.loader.open('Deleting...').subscribe()

    try {
      await this.api.deleteWifi({ ssid })
      await this.getWifi()
      delete this.wifi.ssids[ssid]
    } catch (e: any) {
      this.errorService.handleError(e)
    } finally {
      loader.unsubscribe()
    }
  }

  private async save(ssid: string, password: string): Promise<void> {
    const loader = this.loader.open('Saving...').subscribe()

    try {
      await this.api.addWifi({
        ssid,
        password,
        priority: 0,
        connect: false,
      })
      await this.getWifi()
    } catch (e: any) {
      this.errorService.handleError(e)
    } finally {
      loader.unsubscribe()
    }
  }

  private async saveAndConnect(ssid: string, password: string): Promise<void> {
    const loader = this.loader
      .open('Connecting. This could take a while...')
      .subscribe()

    try {
      await this.api.addWifi({
        ssid,
        password,
        priority: 0,
        connect: true,
      })

      await this.confirmWifi(ssid, true)
    } catch (e: any) {
      this.errorService.handleError(e)
    } finally {
      loader.unsubscribe()
    }
  }
}

function getWifiValueSpec(
  ssid: string | null = null,
  needsPW: boolean = true,
): CT.ValueSpecObject {
  return {
    warning: null,
    type: 'object',
    name: 'WiFi Credentials',
    description:
      'Enter the network SSID and password. You can connect now or save the network for later.',
    spec: {
      ssid: {
        type: 'text',
        minLength: null,
        maxLength: null,
        patterns: [],
        name: 'Network SSID',
        description: null,
        inputmode: 'text',
        placeholder: null,
        required: true,
        masked: false,
        default: ssid,
        warning: null,
        disabled: false,
        immutable: false,
        generate: null,
      },
      password: {
        type: 'text',
        minLength: null,
        maxLength: null,
        patterns: [
          {
            regex: '^.{8,}$',
            description: 'Must be longer than 8 characters',
          },
        ],
        name: 'Password',
        description: null,
        inputmode: 'text',
        placeholder: null,
        required: needsPW,
        masked: true,
        default: null,
        warning: null,
        disabled: false,
        immutable: false,
        generate: null,
      },
    },
  }
}
