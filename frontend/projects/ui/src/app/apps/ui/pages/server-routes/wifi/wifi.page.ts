import { Component } from '@angular/core'
import { ToastController } from '@ionic/angular'
import { TuiDialogOptions } from '@taiga-ui/core'
import { ToggleCustomEvent } from '@ionic/core'
import { ApiService } from 'src/app/services/api/embassy-api.service'
import { AvailableWifi, RR } from 'src/app/services/api/api.types'
import { pauseFor, ErrorToastService } from '@start9labs/shared'
import { FormDialogService } from 'src/app/services/form-dialog.service'
import { FormContext, FormPage } from 'src/app/modals/form/form.page'
import { LoadingService } from 'src/app/modals/loading/loading.service'
import { PatchDB } from 'patch-db-client'
import { DataModel } from 'src/app/services/patch-db/data-model'
import { ConnectionService } from 'src/app/services/connection.service'
import { Pipe, PipeTransform } from '@angular/core'
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
import { wifiSpec } from './wifi.const'

interface WiFiForm {
  ssid: string
  password: string
}

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
    this.localChanges$,
  )

  constructor(
    private readonly api: ApiService,
    private readonly toastCtrl: ToastController,
    private readonly loader: LoadingService,
    private readonly formDialog: FormDialogService,
    private readonly errToast: ErrorToastService,
    private readonly patch: PatchDB<DataModel>,
    private readonly connectionService: ConnectionService,
  ) {}

  async toggleWifi(e: ToggleCustomEvent): Promise<void> {
    const enable = e.detail.checked
    const loader = this.loader
      .open(enable ? 'Enabling Wifi' : 'Disabling WiFi')
      .subscribe()

    try {
      await this.api.enableWifi({ enable })
    } catch (e: any) {
      this.errToast.present(e)
    } finally {
      loader.unsubscribe()
    }
  }

  async connect(ssid: string): Promise<void> {
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

  async forget(ssid: string, wifi: RR.GetWifiRes): Promise<void> {
    const loader = this.loader.open('Deleting...').subscribe()

    try {
      await this.api.deleteWifi({ ssid })
      delete wifi.ssids[ssid]
      this.localChanges$.next(wifi)
      this.trigger$.next('')
    } catch (e: any) {
      this.errToast.present(e)
    } finally {
      loader.unsubscribe()
    }
  }

  async presentModalAdd(network: AvailableWifi) {
    if (!network.security.length) {
      this.connect(network.ssid)
    } else {
      const options: Partial<TuiDialogOptions<FormContext<WiFiForm>>> = {
        label: 'Password Needed',
        data: {
          spec: wifiSpec.spec,
          buttons: [
            {
              text: 'Connect',
              handler: async ({ ssid, password }) =>
                this.saveAndConnect(ssid, password),
            },
          ],
        },
      }
      this.formDialog.open(FormPage, options)
    }
  }

  presentModalAddOther(wifi: RR.GetWifiRes) {
    const options: Partial<TuiDialogOptions<FormContext<WiFiForm>>> = {
      label: wifiSpec.name,
      data: {
        spec: wifiSpec.spec,
        buttons: [
          {
            text: 'Save for Later',
            handler: async ({ ssid, password }) =>
              this.save(ssid, password, wifi),
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

  private getWifi$(): Observable<RR.GetWifiRes> {
    return from(this.api.getWifi({}, 10000)).pipe(
      catchError((e: any) => {
        this.errToast.present(e)
        return []
      }),
    )
  }

  private async presentToastSuccess(): Promise<void> {
    const toast = await this.toastCtrl.create({
      header: 'Connection successful!',
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
      cssClass: 'success-toast',
    })

    await toast.present()
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
          this.presentToastSuccess()
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

@Pipe({
  name: 'toWifiIcon',
})
export class ToWifiIconPipe implements PipeTransform {
  transform(signal: number): string {
    if (signal >= 0 && signal < 5) {
      return 'assets/img/icons/wifi-0.png'
    } else if (signal >= 5 && signal < 50) {
      return 'assets/img/icons/wifi-1.png'
    } else if (signal >= 50 && signal < 90) {
      return 'assets/img/icons/wifi-2.png'
    } else {
      return 'assets/img/icons/wifi-3.png'
    }
  }
}
