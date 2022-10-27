import { Component } from '@angular/core'
import { AlertController, IonicSlides, LoadingController } from '@ionic/angular'
import { ApiService, Disk } from 'src/app/services/api/api.service'
import SwiperCore, { Swiper } from 'swiper'

SwiperCore.use([IonicSlides])

@Component({
  selector: 'app-home',
  templateUrl: 'home.page.html',
  styleUrls: ['home.page.scss'],
})
export class HomePage {
  swiper?: Swiper
  disks: Disk[] = []
  selectedDisk?: Disk
  error = ''

  constructor(
    private readonly loadingCtrl: LoadingController,
    private readonly api: ApiService,
    private readonly alertCtrl: AlertController,
  ) {}

  async ngOnInit() {
    this.disks = await this.api.getDisks()
  }

  async ionViewDidEnter() {
    if (this.swiper) {
      this.swiper.allowTouchMove = false
    }
  }

  setSwiperInstance(swiper: any) {
    this.swiper = swiper
  }

  next(disk: Disk) {
    this.selectedDisk = disk
    this.swiper?.slideNext(500)
  }

  previous() {
    this.swiper?.slidePrev(500)
  }

  async tryInstall(overwrite: boolean) {
    if (!this.selectedDisk) return

    const { logicalname, 'embassy-data': embassyData } = this.selectedDisk

    if (embassyData && !overwrite) {
      return this.install(logicalname, overwrite)
    }

    await this.presentAlertDanger(logicalname, embassyData)
  }

  private async presentAlertDanger(logicalname: string, embassyData: boolean) {
    const message = embassyData
      ? 'This action is COMPLETELY erase your existing Emabssy data'
      : `This action is COMPLETELY erase the disk ${logicalname} and install embassyOS`

    const alert = await this.alertCtrl.create({
      header: 'Warning',
      message,
      buttons: [
        {
          text: 'Cancel',
          role: 'cancel',
        },
        {
          text: 'Continue',
          handler: () => {
            this.install(logicalname, true)
          },
        },
      ],
      cssClass: 'alert-warning-message',
    })
    await alert.present()
  }

  async install(logicalname: string, overwrite: boolean) {
    const loader = await this.loadingCtrl.create({
      message: 'Installing embassyOS...',
    })
    await loader.present()

    try {
      await this.api.install({ logicalname, overwrite })
    } catch (e: any) {
      this.error = e.message
    } finally {
      loader.dismiss()
    }
  }
}
