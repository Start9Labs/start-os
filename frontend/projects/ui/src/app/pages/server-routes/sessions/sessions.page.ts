import { Component } from '@angular/core'
import { AlertController, LoadingController } from '@ionic/angular'
import { ErrorToastService } from '@start9labs/shared'
import { ApiService } from 'src/app/services/api/embassy-api.service'
import { PlatformType, Session } from 'src/app/services/api/api.types'

@Component({
  selector: 'sessions',
  templateUrl: 'sessions.page.html',
  styleUrls: ['sessions.page.scss'],
})
export class SessionsPage {
  loading = true
  currentSession?: Session
  otherSessions: SessionWithId[] = []

  constructor(
    private readonly loadingCtrl: LoadingController,
    private readonly errToast: ErrorToastService,
    private readonly alertCtrl: AlertController,
    private readonly embassyApi: ApiService,
  ) {}

  async ngOnInit() {
    try {
      const sessionInfo = await this.embassyApi.getSessions({})
      this.currentSession = sessionInfo.sessions[sessionInfo.current]
      delete sessionInfo.sessions[sessionInfo.current]
      this.otherSessions = Object.entries(sessionInfo.sessions)
        .map(([id, session]) => {
          return {
            id,
            ...session,
          }
        })
        .sort((a, b) => {
          return (
            new Date(b['last-active']).valueOf() -
            new Date(a['last-active']).valueOf()
          )
        })
    } catch (e: any) {
      this.errToast.present(e)
    } finally {
      this.loading = false
    }
  }

  async presentAlertKillAll() {
    const alert = await this.alertCtrl.create({
      header: 'Confirm',
      message: `Terminate <b>all</b> other web sessions?<br /><br />Note: you will <b>not</b> be logged out of your current session on this device.`,
      buttons: [
        {
          text: 'Cancel',
          role: 'cancel',
        },
        {
          text: 'Terminate all',
          handler: () => {
            this.kill(this.otherSessions.map(s => s.id))
          },
          cssClass: 'enter-click',
        },
      ],
    })
    await alert.present()
  }

  async kill(ids: string[]): Promise<void> {
    const loader = await this.loadingCtrl.create({
      message: `Terminating session${ids.length > 1 ? 's' : ''}...`,
    })
    await loader.present()

    try {
      await this.embassyApi.killSessions({ ids })
      this.otherSessions = this.otherSessions.filter(s => !ids.includes(s.id))
    } catch (e: any) {
      this.errToast.present(e)
    } finally {
      loader.dismiss()
    }
  }

  getPlatformIcon(platforms: PlatformType[]): string {
    if (platforms.includes('cli')) {
      return 'terminal-outline'
    } else if (platforms.includes('desktop')) {
      return 'desktop-outline'
    } else {
      return 'phone-portrait-outline'
    }
  }

  getPlatformName(platforms: PlatformType[]): string {
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

  asIsOrder(a: any, b: any) {
    return 0
  }
}

interface SessionWithId extends Session {
  id: string
}
