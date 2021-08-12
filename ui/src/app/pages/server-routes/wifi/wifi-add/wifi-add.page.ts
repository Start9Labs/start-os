import { Component } from '@angular/core'
import { LoadingController, NavController } from '@ionic/angular'
import { ApiService } from 'src/app/services/api/embassy-api.service'
import { WifiService } from '../wifi.service'
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
    private readonly embassyApi: ApiService,
    private readonly loadingCtrl: LoadingController,
    private readonly wifiService: WifiService,
  ) { }

  async save (): Promise<void> {
    const loader = await this.loadingCtrl.create({
      spinner: 'lines',
      message: 'Saving...',
      cssClass: 'loader',
    })
    await loader.present()

    try {
      await this.embassyApi.addWifi({
        ssid: this.ssid,
        password: this.password,
        country: this.countryCode,
        priority: 0,
        connect: false,
      })
      this.navCtrl.back()
    } catch (e) {
      this.errToast.present(e)
    } finally {
      loader.dismiss()
    }
  }

  async saveAndConnect (): Promise<void> {
    const loader = await this.loadingCtrl.create({
      spinner: 'lines',
      message: 'Connecting. This could take while...',
      cssClass: 'loader',
    })
    await loader.present()

    try {
      await this.embassyApi.addWifi({
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
    } catch (e) {
      this.errToast.present(e)
    } finally {
      loader.dismiss()
    }
  }

  asIsOrder (a: any, b: any) {
    return 0
  }
}
