import { Component } from '@angular/core'
import { ActionSheetController } from '@ionic/angular'
import { ApiService } from 'src/app/services/api/api.service'
import { ActionSheetButton } from '@ionic/core'
import { pauseFor } from 'src/app/util/misc.util'
import { WifiService } from './wifi.service'
import { PropertySubject } from 'src/app/util/property-subject.util'
import { S9Server } from 'src/app/models/server-model'
import { LoaderService } from 'src/app/services/loader.service'
import { ModelPreload } from 'src/app/models/model-preload'

@Component({
  selector: 'wifi',
  templateUrl: 'wifi.page.html',
  styleUrls: ['wifi.page.scss'],
})
export class WifiListPage {
  server: PropertySubject<S9Server> = { } as any
  error: string

  constructor (
    private readonly apiService: ApiService,
    private readonly loader: LoaderService,
    private readonly actionCtrl: ActionSheetController,
    private readonly wifiService: WifiService,
    private readonly preload: ModelPreload,
  ) { }

  async ngOnInit () {
    this.loader.displayDuring$(
      this.preload.server(),
    ).subscribe(s => this.server = s)
  }

  async doRefresh (event: any) {
    await Promise.all([
      this.apiService.getServer(),
      pauseFor(600),
    ])
    event.target.complete()
  }

  async presentAction (ssid: string) {
    const buttons: ActionSheetButton[] = [
      {
        text: 'Forget',
        cssClass: 'alert-danger',
        handler: () => {
          this.delete(ssid)
        },
      },
    ]

    if (ssid !== this.server.wifi.getValue().current) {
      buttons.unshift(
        {
          text: 'Connect',
          handler: () => {
            this.connect(ssid)
          },
        },
      )
    }

    const action = await this.actionCtrl.create({
      buttons,
    })

    await action.present()
  }

  // Let's add country code here.
  async connect (ssid: string): Promise<void> {
    this.error = ''
    this.loader.of({
      message: 'Connecting. This could take while...',
      spinner: 'lines',
      cssClass: 'loader',
    }).displayDuringAsync(async () => {
      const current = this.server.wifi.getValue().current
      await this.apiService.connectWifi(ssid)
      const success = await this.wifiService.confirmWifi(ssid)
      if (success) {
        this.wifiService.presentAlertSuccess(ssid, current)
      } else {
        this.wifiService.presentToastFail()
      }
    }).catch(e => {
      console.error(e)
      this.error = e.message
    })
  }

  async delete (ssid: string): Promise<void> {
    this.error = ''
    this.loader.of({
      message: 'Deleting...',
      spinner: 'lines',
      cssClass: 'loader',
    }).displayDuringAsync( async () => {
      await this.apiService.deleteWifi(ssid)
      this.wifiService.removeWifi(ssid)
      this.error = ''
    }).catch(e => {
      console.error(e)
      this.error = e.message
    })
  }
}
