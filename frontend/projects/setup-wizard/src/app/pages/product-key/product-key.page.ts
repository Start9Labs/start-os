import { Component, ViewChild } from '@angular/core'
import { IonInput, LoadingController, NavController } from '@ionic/angular'
import { ApiService } from 'src/app/services/api/api.service'
import { RPCEncryptedService } from 'src/app/services/rpc-encrypted.service'
import { StateService } from 'src/app/services/state.service'

@Component({
  selector: 'app-product-key',
  templateUrl: 'product-key.page.html',
  styleUrls: ['product-key.page.scss'],
})
export class ProductKeyPage {
  @ViewChild('focusInput') elem?: IonInput
  productKey = ''
  error = ''

  constructor(
    private readonly navCtrl: NavController,
    private readonly stateService: StateService,
    private readonly apiService: ApiService,
    private readonly loadingCtrl: LoadingController,
    private readonly encrypted: RPCEncryptedService,
  ) {}

  ionViewDidEnter() {
    setTimeout(() => this.elem?.setFocus(), 400)
  }

  async submit() {
    if (!this.productKey) return (this.error = 'Must enter product key')

    const loader = await this.loadingCtrl.create({
      message: 'Verifying Product Key',
    })
    await loader.present()

    try {
      this.encrypted.productKey = this.productKey
      await this.apiService.verifyProductKey()
      if (this.stateService.isMigrating) {
        await this.navCtrl.navigateForward(`/loading`)
      } else {
        await this.navCtrl.navigateForward(`/home`)
      }
    } catch (e) {
      this.error = 'Invalid Product Key'
      this.encrypted.productKey = undefined
    } finally {
      loader.dismiss()
    }
  }
}
