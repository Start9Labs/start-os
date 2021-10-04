import { Component } from '@angular/core'
import { LoadingController, NavController } from '@ionic/angular'
import { ApiService } from 'src/app/services/api/api.service'
import { HttpService } from 'src/app/services/api/http.service'
import { StateService } from 'src/app/services/state.service'

@Component({
  selector: 'app-product-key',
  templateUrl: 'product-key.page.html',
  styleUrls: ['product-key.page.scss'],
})
export class ProductKeyPage {
  productKey: string
  error: string

  constructor(
    private readonly navCtrl: NavController,
    private readonly stateService: StateService,
    private readonly apiService: ApiService,
    private readonly loadingCtrl: LoadingController,
    private readonly httpService: HttpService
  ) {}

  async submit () {
    if(!this.productKey) return this.error = "Must enter product key"

    const loader = await this.loadingCtrl.create({
      message: 'Verifying Product Key'
    })
    await loader.present()

    try {
      this.httpService.productKey = this.productKey
      const state = await this.apiService.verifyProductKey()
      const torAddress = state['tor-address']
      if(state['is-recovering']) {
        await this.navCtrl.navigateForward(`/loading`)
      } else if (!!torAddress) {
        this.stateService.torAddress = torAddress
        await this.navCtrl.navigateForward(`/success`)
      } else {
        await this.navCtrl.navigateForward(`/embassy`)
      }
    } catch (e) {
      this.error = e.message
      this.httpService.productKey = undefined
    } finally {
      loader.dismiss()
    }
  }
}

