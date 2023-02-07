import { Component } from '@angular/core'
import { CheckboxCustomEvent, LoadingController } from '@ionic/angular'
import { ErrorToastService } from '@start9labs/shared'
import { ApiService } from 'src/app/services/api/embassy-api.service'
import { PlatformType, Session } from 'src/app/services/api/api.types'
import { BehaviorSubject } from 'rxjs'

@Component({
  selector: 'sessions',
  templateUrl: 'sessions.page.html',
  styleUrls: ['sessions.page.scss'],
})
export class SessionsPage {
  currentSession?: Session
  otherSessions: SessionWithId[] = []

  selected: Set<string> = new Set()

  loading$ = new BehaviorSubject(true)

  constructor(
    private readonly loadingCtrl: LoadingController,
    private readonly errToast: ErrorToastService,
    private readonly api: ApiService,
  ) {}

  async ngOnInit() {
    try {
      const sessionInfo = await this.api.getSessions({})
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
      this.loading$.next(false)
    }
  }

  async toggleChecked(id: string, e: CheckboxCustomEvent) {
    if (e.detail.checked) {
      this.selected.add(id)
    } else {
      this.selected.delete(id)
    }
  }

  async kill(): Promise<void> {
    const ids = Array.from(this.selected)

    const loader = await this.loadingCtrl.create({
      message: `Terminating session${ids.length > 1 ? 's' : ''}...`,
    })
    await loader.present()

    try {
      await this.api.killSessions({ ids })
      this.selected.clear()
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
}

interface SessionWithId extends Session {
  id: string
}
