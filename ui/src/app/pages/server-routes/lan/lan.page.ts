import { Component } from '@angular/core'
import { isPlatform, ToastController } from '@ionic/angular'
import { copyToClipboard } from 'src/app/util/web.util'
import { ConfigService } from 'src/app/services/config.service'
import { LoaderService } from 'src/app/services/loader.service'
import { ApiService } from 'src/app/services/api/api.service'
import { PatchDbModel } from 'src/app/services/patch-db/patch-db.service'
import { Subscription } from 'rxjs'

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
    private readonly loader: LoaderService,
    private readonly apiService: ApiService,
    private readonly patch: PatchDbModel,
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
    this.loader.of({
      message: 'Refreshing Network',
      spinner: 'lines',
      cssClass: 'loader',
    }).displayDuringAsync( async () => {
      await this.apiService.refreshLan({ })
    }).catch(e => {
      console.error(e)
    })
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
}

enum LanSetupIssue {
  NOT_TOR = 'NotTor',
  NOT_DESKTOP = 'NotDesktop',
}
