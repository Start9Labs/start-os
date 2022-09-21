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
  guid?: string | null
  error = false

  constructor(
    private readonly api: ApiService,
    private readonly modalCtrl: ModalController,
    private readonly alertCtrl: AlertController,
    private readonly loadingCtrl: LoadingController,
    private readonly stateService: StateService,
    private readonly navCtrl: NavController,
    private readonly errToastService: ErrorToastService,
  ) {}

  async ngOnInit() {
    try {
      await this.api.getPubKey()
      const disks = await this.api.getDrives()
      this.guid = disks.find(d => !!d.guid)?.guid
    } catch (e: any) {
      this.error = true
      this.errToastService.present(e)
    }
  }

  async ionViewDidEnter() {
    if (this.swiper) {
      this.swiper.allowTouchMove = false
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

  async import() {
    if (this.guid) {
      const modal = await this.modalCtrl.create({
        component: PasswordPage,
        componentProps: { storageDrive: true },
      })
      modal.onDidDismiss().then(res => {
        if (res.data && res.data.password) {
          this.importDrive(res.data.password)
        }
      })
      await modal.present()
    } else {
      const alert = await this.alertCtrl.create({
        header: 'Drive Not Found',
        message:
          'Please make sure the drive is a valid Embassy data drive (not a backup) and is firmly connected, then refresh the page.',
      })
      await alert.present()
    }
  }

  private async importDrive(password: string) {
    const loader = await this.loadingCtrl.create({
      message: 'Importing Drive',
    })
    await loader.present()
    try {
      await this.stateService.importDrive(this.guid!, password)
      await this.navCtrl.navigateForward(`/success`)
    } catch (e: any) {
      this.errToastService.present(e)
    } finally {
      loader.dismiss()
    }
  }
}

function decodeHex(hex: string) {
  let str = ''
  for (let n = 0; n < hex.length; n += 2) {
    str += String.fromCharCode(parseInt(hex.substring(n, 2), 16))
  }
  return str
}
