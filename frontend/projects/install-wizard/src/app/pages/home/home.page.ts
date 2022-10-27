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
  loaded = false

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

  private async presentAlertDanger(logicalname: string, embassyData: boolean) {
    const message = embassyData
      ? 'This action COMPLETELY erases your existing Embassy data'
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

  private async presentAlertReboot() {
    const alert = await this.alertCtrl.create({
      header: 'Install Success',
      message: 'Reboot your device to begin using your new Emabssy',
      buttons: [
        {
          text: 'Reboot',
          handler: () => {
            this.reboot()
          },
        },
      ],
      cssClass: 'alert-warning-message',
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
      message: 'Please wait for Embassy to restart, then refresh this page',
      buttons: ['OK'],
      cssClass: 'alert-warning-message',
    })
    await alert.present()
  }
}
