import { Component } from '@angular/core'
import { IonicSlides } from '@ionic/angular'
import { ApiService } from 'src/app/services/api/api.service'
import SwiperCore, { Swiper } from 'swiper'
import { ErrorToastService } from '@start9labs/shared'
import { StateService } from 'src/app/services/state.service'

SwiperCore.use([IonicSlides])

@Component({
  selector: 'app-home',
  templateUrl: 'home.page.html',
  styleUrls: ['home.page.scss'],
})
export class HomePage {
  swiper?: Swiper
  error = false
  loading = true

  constructor(
    private readonly api: ApiService,
    private readonly errToastService: ErrorToastService,
    private readonly stateService: StateService,
  ) {}

  async ionViewDidEnter() {
    this.stateService.setupType = 'fresh'
    if (this.swiper) {
      this.swiper.allowTouchMove = false
    }

    try {
      await this.api.getPubKey()
    } catch (e: any) {
      this.error = true
      this.errToastService.present(e)
    } finally {
      this.loading = false
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
