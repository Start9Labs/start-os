import { Component } from '@angular/core'
import {
  ActionSheetController,
  AlertController,
  ToastController,
} from '@ionic/angular'
import { TuiDialogOptions } from '@taiga-ui/core'
import { ToggleChangeEventDetail } from '@ionic/core'
import { ApiService } from 'src/app/services/api/embassy-api.service'
import { ActionSheetButton } from '@ionic/core'
import { ValueSpecObject } from 'start-sdk/lib/config/configTypes'
import { RR } from 'src/app/services/api/api.types'
import { pauseFor, ErrorToastService } from '@start9labs/shared'
import { FormDialogService } from 'src/app/services/form-dialog.service'
import { FormContext, FormPage } from 'src/app/modals/form/form.page'
import { LoadingService } from 'src/app/modals/loading/loading.service'

interface WiFiForm {
  ssid: string
  password: string
}
import { catchError, filter, from, Observable, Subject, switchMap, tap } from 'rxjs'
import { PatchDB } from 'patch-db-client'
import { DataModel } from 'src/app/services/patch-db/data-model'

@Component({
  selector: 'wifi',
  templateUrl: 'wifi.page.html',
  styleUrls: ['wifi.page.scss'],
})
export class WifiPage {
  readonly enabled$ = this.patch.watch$('server-info', 'wifi-enabled').pipe(
    filter(Boolean),
    tap(() => this.trigger$.next('')),
  )
  readonly trigger$ = new Subject<string>()
  readonly wifi$ = this.trigger$.pipe(
    switchMap(() => this.getWifi$()),
  )

  constructor(
    private readonly api: ApiService,
    private readonly toastCtrl: ToastController,
    private readonly alertCtrl: AlertController,
    private readonly loader: LoadingService,
    private readonly formDialog: FormDialogService,
    private readonly errToast: ErrorToastService,
    private readonly actionCtrl: ActionSheetController,
    private readonly patch: PatchDB<DataModel>,
  ) {}

  async toggleWifi(e: ToggleChangeEventDetail) {
    console.error(e)
    const enabled = e.checked
    const loader = await this.loadingCtrl.create({
      message: enabled ? 'Enabling Wifi' : 'Disabling WiFi',
    })
    await loader.present()

    try {
      await this.api.enableWifi({ enabled })
    } catch (e: any) {
      this.errToast.present(e)
    } finally {
      loader.dismiss()
    }
  }

  getWifi$(): Observable<RR.GetWifiRes> {
    return from(this.api.getWifi({}, 0)).pipe(
      catchError((e: any) => {
        this.errToast.present(e)
        return []
      }),
    )
  }

  presentModalAdd(ssid?: string, needsPW: boolean = true) {
    const { name, spec } = getWifiValueSpec(ssid, needsPW)
    const options: Partial<TuiDialogOptions<FormContext<WiFiForm>>> = {
      label: name,
      data: {
        spec,
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

    this.formDialog.open(FormPage, options)
  }

  async presentAction(ssid: string, connectedSsid: string | null) {
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

    if (ssid !== connectedSsid) {
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
    const loader = this.loader
      .open('Connecting. This could take a while...')
      .subscribe()

    try {
      await this.api.connectWifi({ ssid })
      await this.confirmWifi(ssid)
    } catch (e: any) {
      this.errToast.present(e)
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
      this.errToast.present(e)
    } finally {
      loader.unsubscribe()
    }
  }

  private async save(ssid: string, password: string): Promise<boolean> {
    const loader = this.loader.open('Saving...').subscribe()

    try {
      await this.api.addWifi({
        ssid,
        password,
        priority: 0,
        connect: false,
      })
      await this.getWifi()
      return true
    } catch (e: any) {
      this.errToast.present(e)
      return false
    } finally {
      loader.unsubscribe()
    }
  }

  private async saveAndConnect(
    ssid: string,
    password: string,
  ): Promise<boolean> {
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
      return true
    } catch (e: any) {
      this.errToast.present(e)
      return false
    } finally {
      loader.unsubscribe()
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
    warning: null,
    spec: {
      ssid: {
        type: 'text',
        name: 'Network SSID',
        description: null,
        inputmode: 'text',
        placeholder: null,
        patterns: [],
        minLength: null,
        maxLength: null,
        required: true,
        masked: false,
        default: ssid || null,
        warning: null,
      },
      password: {
        type: 'text',
        name: 'Password',
        description: null,
        inputmode: 'text',
        placeholder: null,
        required: needsPW,
        masked: true,
        minLength: null,
        maxLength: null,
        patterns: [
          {
            regex: '^.{8,}$',
            description: 'Must be longer than 8 characters',
          },
        ],
        default: null,
        warning: null,
      },
    },
  }
}
