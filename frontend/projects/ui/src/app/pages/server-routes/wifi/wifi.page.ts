import { Component } from '@angular/core'
import {
  ActionSheetController,
  AlertController,
  ToastController,
} from '@ionic/angular'
import { TuiDialogOptions } from '@taiga-ui/core'
import { ToggleChangeEventDetail } from '@ionic/core'
import { ToggleCustomEvent } from '@ionic/core'
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
import {
  GenericFormPage,
  GenericFormOptions,
} from 'src/app/modals/generic-form/generic-form.page'
import {
  BehaviorSubject,
  catchError,
  distinctUntilChanged,
  filter,
  from,
  merge,
  Observable,
  Subject,
  switchMap,
  tap,
} from 'rxjs'
import { PatchDB } from 'patch-db-client'
import { DataModel } from 'src/app/services/patch-db/data-model'
import { ConnectionService } from 'src/app/services/connection.service'

@Component({
  selector: 'wifi',
  templateUrl: 'wifi.page.html',
  styleUrls: ['wifi.page.scss'],
})
export class WifiPage {
  readonly connected$ = this.connectionService.connected$.pipe(filter(Boolean))
  readonly enabled$ = this.patch.watch$('server-info', 'wifi-enabled').pipe(
    distinctUntilChanged(),
    tap(enabled => {
      if (enabled) this.trigger$.next('')
    }),
  )
  readonly trigger$ = new BehaviorSubject('')
  readonly localChanges$ = new Subject<RR.GetWifiRes>()
  readonly wifi$ = merge(
    this.trigger$.pipe(switchMap(() => this.getWifi$())),
    this.localChanges$.asObservable(),
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
    private readonly connectionService: ConnectionService,
  ) {}

  async toggleWifi(e: ToggleCustomEvent) {
    const enable = e.detail.checked
    const loader = await this.loadingCtrl.create({
      message: enable ? 'Enabling Wifi' : 'Disabling WiFi',
    })
    await loader.present()

    try {
      await this.api.enableWifi({ enable })
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

  async presentAction(ssid: string, wifi: RR.GetWifiRes) {
    const buttons: ActionSheetButton[] = [
      {
        text: 'Forget',
        icon: 'trash',
        role: 'destructive',
        handler: () => {
          this.delete(ssid, wifi)
        },
      },
    ]

    if (ssid !== wifi.connected) {
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

  private getWifi$(): Observable<RR.GetWifiRes> {
    return from(this.api.getWifi({}, 10000)).pipe(
      catchError((e: any) => {
        this.errToast.present(e)
        return []
      }),
    )
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

  private async delete(ssid: string, wifi: RR.GetWifiRes): Promise<void> {
    const loader = this.loader.open('Deleting...').subscribe()

    try {
      await this.api.deleteWifi({ ssid })
      delete wifi.ssids[ssid]
      this.localChanges$.next(wifi)
    } catch (e: any) {
      this.errToast.present(e)
    } finally {
      loader.unsubscribe()
    }
  }

  private async save(
    ssid: string,
    password: string,
    wifi: RR.GetWifiRes,
  ): Promise<boolean> {
    const loader = this.loader.open('Saving...').subscribe()

    try {
      await this.api.addWifi({
        ssid,
        password,
        priority: 0,
        connect: false,
      })
      wifi.ssids[ssid] = 0
      this.localChanges$.next(wifi)
      this.trigger$.next('')
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
    wifi: RR.GetWifiRes,
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
      await this.confirmWifi(ssid)
      return true
    } catch (e: any) {
      this.errToast.present(e)
      return false
    } finally {
      loader.unsubscribe()
    }
  }

  private async confirmWifi(ssid: string): Promise<void> {
    const maxAttempts = 5
    let attempts = 0

    while (true) {
      if (attempts > maxAttempts) {
        this.presentToastFail()
        break
      }

      try {
        const start = new Date().valueOf()
        const newWifi = await this.api.getWifi({}, 10000)
        const end = new Date().valueOf()
        if (newWifi.connected === ssid) {
          this.localChanges$.next(newWifi)
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
