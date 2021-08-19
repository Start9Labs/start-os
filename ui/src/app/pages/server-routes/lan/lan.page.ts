import { Component } from '@angular/core'
import { LoadingController, ToastController } from '@ionic/angular'
import { ConfigService } from 'src/app/services/config.service'
import { ApiService } from 'src/app/services/api/embassy-api.service'
import { ErrorToastService } from 'src/app/services/error-toast.service'

@Component({
  selector: 'lan',
  templateUrl: './lan.page.html',
  styleUrls: ['./lan.page.scss'],
})
export class LANPage {
  lanAddress: string
  lanDisabled: string

  constructor (
    private readonly toastCtrl: ToastController,
    private readonly config: ConfigService,
    private readonly loadingCtrl: LoadingController,
    private readonly errToast: ErrorToastService,
    private readonly embassyApi: ApiService,
  ) { }

  ngOnInit () {
    if (!this.config.isTor()) {
      this.lanDisabled = 'For security reasons, you must setup LAN over a Tor connection. Please navigate to your Embassy Tor Address and try again.'
    }
  }

  installCert (): void {
    document.getElementById('install-cert').click()
  }

  async refreshLAN (): Promise<void> {
    const loader = await this.loadingCtrl.create({
      spinner: 'lines',
      message: 'Refreshing LAN...',
      cssClass: 'loader',
    })
    await loader.present()

    try {
      await this.embassyApi.refreshLan({ })
      this.presentToastSuccess()
    } catch (e) {
      this.errToast.present(e)
    } finally {
      loader.dismiss()
    }
  }

  private async presentToastSuccess (): Promise<void> {
    const toast = await this.toastCtrl.create({
      header: 'Success',
      message: `LAN refreshed.`,
      position: 'bottom',
      duration: 3000,
      buttons: [
        {
          side: 'start',
          icon: 'close',
          handler: () => {
            return true
          },
        },
      ],
      cssClass: 'success-toast',
    })

    await toast.present()
  }
}
