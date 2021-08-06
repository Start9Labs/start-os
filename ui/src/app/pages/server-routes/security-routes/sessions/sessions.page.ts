import { Component } from '@angular/core'
import { AlertController, getPlatforms, LoadingController } from '@ionic/angular'
import { ErrorToastService } from 'src/app/services/error-toast.service'
import { ApiService } from 'src/app/services/api/embassy/embassy-api.service'
import { PlatformType, RR } from 'src/app/services/api/api.types'

@Component({
  selector: 'sessions',
  templateUrl: 'sessions.page.html',
  styleUrls: ['sessions.page.scss'],
})
export class SessionsPage {
  loading = true
  sessionInfo: RR.GetSessionsRes

  constructor (
    private readonly loadingCtrl: LoadingController,
    private readonly errToast: ErrorToastService,
    private readonly alertCtrl: AlertController,
    private readonly embassyApi: ApiService,
  ) { }

  async ngOnInit () {
    getPlatforms()
    this.sessionInfo = await this.embassyApi.getSessions({ })
    this.loading = false
  }

  async presentAlertKill (hash: string) {
    const alert = await this.alertCtrl.create({
      backdropDismiss: false,
      header: 'Caution',
      message: `Are you sure you want to kill this session?`,
      buttons: [
        {
          text: 'Cancel',
          role: 'cancel',
        },
        {
          text: 'Kill',
          handler: () => {
            this.kill(hash)
          },
        },
      ],
    })
    await alert.present()
  }

  async kill (hash: string): Promise<void> {
    const loader = await this.loadingCtrl.create({
      spinner: 'lines',
      message: 'Killing session...',
      cssClass: 'loader',
    })
    await loader.present()

    try {
      await this.embassyApi.killSessions({ hashes: [hash] })
      delete this.sessionInfo.sessions[hash]
    } catch (e) {
      this.errToast.present(e)
    } finally {
      loader.dismiss()
    }
  }

  getPlatformIcon (platforms: PlatformType[]): string {
    if (platforms.includes('cli')) {
      return 'terminal-outline'
    } else if (platforms.includes('desktop')) {
      return 'desktop-outline'
    } else {
      return 'phone-portrait-outline'
    }
  }

  getPlatformName (platforms: PlatformType[]): string {
    if (platforms.includes('cli')) {
      return 'CLI'
    } else if (platforms.includes('desktop')) {
      return 'Desktop/Laptop'
    } else if (platforms.includes('android')) {
      return 'Android Device'
    } else if (platforms.includes('iphone')) {
      return 'iPhone'
    } else if (platforms.includes('ipad')) {
      return 'iPad'
    } else if (platforms.includes('ios')) {
      return 'iOS Device'
    } else {
      return 'Unknown Device'
    }
  }

  asIsOrder (a: any, b: any) {
    return 0
  }
}
