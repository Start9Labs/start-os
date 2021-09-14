import { Component } from '@angular/core'
import { iosTransitionAnimation, LoadingController, NavController } from '@ionic/angular'
import { ApiService } from 'src/app/services/api/api.service'
import { AES_CTR, decodeUtf8, encodeUtf8, HttpService } from 'src/app/services/api/http.service'
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
    private stateService: StateService,
    private apiService: ApiService,
    private loadingCtrl: LoadingController,
    private httpService: HttpService
  ) {}

  async submit () {
    if(!this.productKey) return this.error = "Must enter product key"

    const loader = await this.loadingCtrl.create({
      message: 'Verifying Product Key'
    })
    await loader.present()

    try {
      const state = await this.apiService.verifyProductKey(this.productKey)
      this.httpService.productKey = this.productKey
      if(state['is-recovering']) {
        await this.navCtrl.navigateForward(`/loading`, { animationDirection: 'forward', animation: iosTransitionAnimation })
      } else if (!!state['tor-address']) {
        this.stateService.torAddress = state['tor-address']
        await this.navCtrl.navigateForward(`/success`, { animationDirection: 'forward', animation: iosTransitionAnimation })
      } else {
        await this.navCtrl.navigateForward(`/home`, { animationDirection: 'forward', animation: iosTransitionAnimation })
      }
    } catch (e) {
      this.error = e.message
      this.httpService.productKey = undefined
    } finally {
      loader.dismiss()
    }
  }
}

