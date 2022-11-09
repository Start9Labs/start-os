import { Component } from '@angular/core'
import { AlertController, IonicSlides, LoadingController } from '@ionic/angular'
import { ApiService } from 'src/app/services/api/api.service'
import SwiperCore, { Swiper } from 'swiper'
import { DiskInfo } from '@start9labs/shared'

SwiperCore.use([IonicSlides])

@Component({
  selector: 'app-home',
  templateUrl: 'home.page.html',
  styleUrls: ['home.page.scss'],
})
export class HomePage {
  swiper?: Swiper
  disks: DiskInfo[] = []
  selectedDisk?: DiskInfo
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

  next(disk: DiskInfo) {
    this.selectedDisk = disk
    this.swiper?.slideNext(500)
  }

  previous() {
    this.swiper?.slidePrev(500)
  }

  async tryInstall(overwrite: boolean) {
    if (!this.selectedDisk) return

    const { logicalname, guid } = this.selectedDisk

    const hasEmbassyData = !!guid

    if (hasEmbassyData && !overwrite) {
      return this.install(logicalname, overwrite)
    }

    await this.presentAlertDanger(logicalname, hasEmbassyData)
  }

  private async install(logicalname: string, overwrite: boolean) {
    const loader = await this.loadingCtrl.create({
      message: 'Installing embassyOS...',
    })
    await loader.present()

    try {
      await this.api.install({ logicalname, overwrite })
      this.presentAlertReboot()
    } catch (e: any) {
      this.error = e.message
    } finally {
      loader.dismiss()
    }
  }

  private async presentAlertDanger(
    logicalname: string,
    hasEmbassyData: boolean,
  ) {
    const message = hasEmbassyData
      ? 'This action COMPLETELY erases your existing Embassy data'
      : `This action COMPLETELY erases the disk ${logicalname} and installs embassyOS`

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
      cssClass: 'alert-danger-message',
    })
    await alert.present()
  }

  private async presentAlertReboot() {
    const alert = await this.alertCtrl.create({
      header: 'Install Success',
      message: 'Reboot your device to begin using your new Embassy',
      buttons: [
        {
          text: 'Reboot',
          handler: () => {
            this.reboot()
          },
        },
      ],
      cssClass: 'alert-success-message',
    })
    await alert.present()
  }

  private async reboot() {
    const loader = await this.loadingCtrl.create()
    await loader.present()

    try {
      await this.api.reboot()
      this.presentAlertComplete()
    } catch (e: any) {
      this.error = e.message
    } finally {
      loader.dismiss()
    }
  }

  private async presentAlertComplete() {
    const alert = await this.alertCtrl.create({
      header: 'Rebooting',
      message: 'Please wait for embassyOS to restart, then refresh this page',
      buttons: ['OK'],
    })
    await alert.present()
  }
}
