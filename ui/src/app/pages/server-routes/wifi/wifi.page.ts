import { Component } from '@angular/core'
import { ActionSheetController } from '@ionic/angular'
import { ApiService } from 'src/app/services/api/api.service'
import { ActionSheetButton } from '@ionic/core'
import { WifiService } from './wifi.service'
import { LoaderService } from 'src/app/services/loader.service'
import { WiFiInfo } from 'src/app/models/patch-db/data-model'
import { PatchDbModel } from 'src/app/models/patch-db/patch-db-model'

@Component({
  selector: 'wifi',
  templateUrl: 'wifi.page.html',
  styleUrls: ['wifi.page.scss'],
})
export class WifiListPage {
  error = ''

  constructor (
    private readonly apiService: ApiService,
    private readonly loader: LoaderService,
    private readonly actionCtrl: ActionSheetController,
    private readonly wifiService: WifiService,
    public readonly patch: PatchDbModel,
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
    this.error = ''
    this.loader.of({
      message: 'Connecting. This could take while...',
      spinner: 'lines',
      cssClass: 'loader',
    }).displayDuringAsync(async () => {
      await this.apiService.connectWifi({ ssid })
      this.wifiService.confirmWifi(ssid)
    }).catch(e => {
      console.error(e)
      this.error = ''
    })
  }

  async delete (ssid: string): Promise<void> {
    this.error = ''
    this.loader.of({
      message: 'Deleting...',
      spinner: 'lines',
      cssClass: 'loader',
    }).displayDuringAsync(async () => {
      await this.apiService.deleteWifi({ ssid })
      this.error = ''
    }).catch(e => {
      console.error(e)
      this.error = ''
    })
  }
}
