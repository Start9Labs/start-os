import { Component } from '@angular/core'
import { ActionSheetController } from '@ionic/angular'
import { ApiService } from 'src/app/services/api/embassy/embassy-api.service'
import { ActionSheetButton } from '@ionic/core'
import { WifiService } from './wifi.service'
import { LoaderService } from 'src/app/services/loader.service'
import { WiFiInfo } from 'src/app/services/patch-db/data-model'
import { PatchDbService } from 'src/app/services/patch-db/patch-db.service'
import { Subscription } from 'rxjs'
import { ErrorToastService } from 'src/app/services/error-toast.service'

@Component({
  selector: 'wifi',
  templateUrl: 'wifi.page.html',
  styleUrls: ['wifi.page.scss'],
})
export class WifiListPage {
  subs: Subscription[] = []

  constructor (
    private readonly embassyApi: ApiService,
    private readonly loader: LoaderService,
    private readonly errToast: ErrorToastService,
    private readonly actionCtrl: ActionSheetController,
    private readonly wifiService: WifiService,
    public readonly patch: PatchDbService,
  ) { }

  async presentAction (ssid: string, wifi: WiFiInfo) {
    const buttons: ActionSheetButton[] = [
      {
        text: 'Forget',
        cssClass: 'alert-danger',
        handler: () => {
          this.delete(ssid)
        },
      },
    ]

    if (ssid !== wifi.connected) {
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

  // Let's add country code here
  async connect (ssid: string): Promise<void> {
    this.loader.of({
      message: 'Connecting. This could take while...',
      spinner: 'lines',
      cssClass: 'loader',
    }).displayDuringAsync(async () => {
      await this.embassyApi.connectWifi({ ssid })
      this.wifiService.confirmWifi(ssid)
    }).catch(e => {
      console.error(e)
      this.errToast.present(e.message)
    })
  }

  async delete (ssid: string): Promise<void> {
    this.loader.of({
      message: 'Deleting...',
      spinner: 'lines',
      cssClass: 'loader',
    }).displayDuringAsync(async () => {
      await this.embassyApi.deleteWifi({ ssid })
    }).catch(e => {
      console.error(e)
      this.errToast.present(e.message)
    })
  }
}
