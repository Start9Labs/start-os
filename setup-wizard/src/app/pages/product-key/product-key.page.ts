import { Component } from '@angular/core'
import { LoadingController, NavController } from '@ionic/angular'
import { ApiService } from 'src/app/services/api/api.service'
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
  ) {}

  async submit () {
    if(!this.productKey) return this.error = "Must enter product key"

    const loader = await this.loadingCtrl.create({
      message: 'Verifying Product Key'
    })
    await loader.present()

    try {
      const state = await this.apiService.verifyProductKey(this.productKey)
      this.stateService.productKey = this.productKey
      if(state['is-recovering']) {
        await this.navCtrl.navigateForward(`/loading`, { animationDirection: 'forward' })
      } else if (!!state['tor-address']) {
        this.stateService.torAddress = state['tor-address']
        await this.navCtrl.navigateForward(`/success`, { animationDirection: 'forward' })
      } else {
        await this.navCtrl.navigateForward(`/home`, { animationDirection: 'forward' })
      }
    } catch (e) {
      this.error = e.message
    } finally {
      loader.dismiss()
    }
  }

  async recoverNav () {
    await this.navCtrl.navigateForward(`/recover`, { animationDirection: 'forward' })
  }

  async embassyNav () {
    await this.navCtrl.navigateForward(`/embassy`, { animationDirection: 'forward' })
  }
}

