import { Component } from '@angular/core'
import { NavController } from '@ionic/angular'
import { ApiService } from 'src/app/services/api/embassy/embassy-api.service'
import { WifiService } from '../wifi.service'
import { LoaderService } from 'src/app/services/loader.service'
import { ErrorToastService } from 'src/app/services/error-toast.service'

@Component({
  selector: 'wifi-add',
  templateUrl: 'wifi-add.page.html',
  styleUrls: ['wifi-add.page.scss'],
})
export class WifiAddPage {
  countries = require('../../../../util/countries.json')
  countryCode = 'US'
  ssid = ''
  password = ''

  constructor (
    private readonly navCtrl: NavController,
    private readonly errToast: ErrorToastService,
    private readonly apiService: ApiService,
    private readonly loader: LoaderService,
    private readonly wifiService: WifiService,
  ) { }

  async save (): Promise<void> {
    this.loader.of({
      message: 'Saving...',
      spinner: 'lines',
      cssClass: 'loader',
    }).displayDuringAsync(async () => {
      await this.apiService.addWifi({
        ssid: this.ssid,
        password: this.password,
        country: this.countryCode,
        priority: 0,
        connect: false,
      })
      this.navCtrl.back()
    }).catch(e => {
      console.error(e)
      this.errToast.present(e.message)
    })
  }

  async saveAndConnect (): Promise<void> {
    this.loader.of({
      message: 'Connecting. This could take while...',
      spinner: 'lines',
      cssClass: 'loader',
    }).displayDuringAsync(async () => {
      await this.apiService.addWifi({
        ssid: this.ssid,
        password: this.password,
        country: this.countryCode,
        priority: 0,
        connect: true,
      })
      const success = this.wifiService.confirmWifi(this.ssid)
        if (success) {
          this.navCtrl.back()
        }
      }).catch (e => {
      console.error(e)
      this.errToast.present(e.message)
    })
  }

  asIsOrder (a: any, b: any) {
    return 0
  }
}
