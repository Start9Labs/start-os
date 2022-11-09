import { Component } from '@angular/core'
import {
  AlertController,
  IonicSlides,
  LoadingController,
  ModalController,
  NavController,
} from '@ionic/angular'
import { PasswordPage } from 'src/app/modals/password/password.page'
import { ApiService } from 'src/app/services/api/api.service'
import { StateService } from 'src/app/services/state.service'
import SwiperCore, { Swiper } from 'swiper'
import { ErrorToastService } from '@start9labs/shared'

SwiperCore.use([IonicSlides])

@Component({
  selector: 'app-home',
  templateUrl: 'home.page.html',
  styleUrls: ['home.page.scss'],
})
export class HomePage {
  swiper?: Swiper
  error = false
  loaded = false

  constructor(
    private readonly api: ApiService,
    private readonly modalCtrl: ModalController,
    private readonly alertCtrl: AlertController,
    private readonly loadingCtrl: LoadingController,
    private readonly stateService: StateService,
    private readonly navCtrl: NavController,
    private readonly errToastService: ErrorToastService,
  ) {}

  async ionViewDidEnter() {
    try {
      await this.api.getPubKey()
    } catch (e: any) {
      this.error = true
      this.errToastService.present(e)
    } finally {
      this.loaded = true // needed to accommodate autoHight="true" on swiper. Otherwise Swiper height might be 0 when navigating *to* this page from later page. Happens on refresh.
      if (this.swiper) {
        this.swiper.allowTouchMove = false
      }
    }
  }

  setSwiperInstance(swiper: any) {
    this.swiper = swiper
  }

  next() {
    this.swiper?.slideNext(500)
  }

  previous() {
    this.swiper?.slidePrev(500)
  }
}
