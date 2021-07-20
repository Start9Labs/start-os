import { Component } from '@angular/core'
import { isPlatform, LoadingController, ToastController } from '@ionic/angular'
import { copyToClipboard } from 'src/app/util/web.util'
import { ConfigService } from 'src/app/services/config.service'
import { ApiService } from 'src/app/services/api/embassy/embassy-api.service'
import { PatchDbService } from 'src/app/services/patch-db/patch-db.service'
import { Subscription } from 'rxjs'
import { ErrorToastService } from 'src/app/services/error-toast.service'

@Component({
  selector: 'lan',
  templateUrl: './lan.page.html',
  styleUrls: ['./lan.page.scss'],
})
export class LANPage {
  lanAddress: string
  lanDisabled: LanSetupIssue
  readonly lanDisabledExplanation: { [k in LanSetupIssue]: string } = {
    NotDesktop: `You are using a mobile device. To setup LAN on a mobile device, please use the Start9 Setup App.`,
    NotTor: `For security reasons, you must setup LAN over a Tor connection. Please navigate to your Embassy Tor Address and try again.`,
  }
  readonly docsUrl = 'https://docs.start9.com/user-manual/general/lan-setup'
  subs: Subscription[] = []

  constructor (
    private readonly toastCtrl: ToastController,
    private readonly config: ConfigService,
    private readonly loadingCtrl: LoadingController,
    private readonly errToast: ErrorToastService,
    private readonly embassyApi: ApiService,
    private readonly patch: PatchDbService,
  ) { }

  ngOnInit () {
    if (isPlatform('ios') || isPlatform('android')) {
      this.lanDisabled = LanSetupIssue.NOT_DESKTOP
    } else if (!this.config.isTor()) {
      this.lanDisabled = LanSetupIssue.NOT_TOR
    }
    this.subs = [
      this.patch.watch$('server-info', 'lan-address')
      .subscribe(addr => {
        this.lanAddress = `https://${addr}`
      }),
    ]
  }

  ngOnDestroy () {
    this.subs.forEach(sub => sub.unsubscribe())
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

  async copyLAN (): Promise <void> {
    const message = await copyToClipboard(this.lanAddress).then(success => success ? 'copied to clipboard!' :  'failed to copy')

    const toast = await this.toastCtrl.create({
      header: message,
      position: 'bottom',
      duration: 1000,
    })
    await toast.present()
  }

  installCert (): void {
    document.getElementById('install-cert').click()
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

enum LanSetupIssue {
  NOT_TOR = 'NotTor',
  NOT_DESKTOP = 'NotDesktop',
}
