import { Component } from '@angular/core'
import { Pipe, PipeTransform } from '@angular/core'
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

  async toggleAll(e: CheckboxCustomEvent) {
    if (e.detail.checked) {
      this.otherSessions.forEach(s => this.selected.add(s.id))
    } else {
      this.selected.clear()
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
}

interface SessionWithId extends Session {
  id: string
}

@Pipe({
  name: 'platformInfo',
})
export class PlatformInfoPipe implements PipeTransform {
  transform(platforms: PlatformType[]): { name: string; icon: string } {
    const info = {
      name: '',
      icon: 'phone-portrait-outline',
    }

    if (platforms.includes('cli')) {
      info.name = 'CLI'
      info.icon = 'terminal-outline'
    } else if (platforms.includes('desktop')) {
      info.name = 'Desktop/Laptop'
      info.icon = 'desktop-outline'
    } else if (platforms.includes('android')) {
      info.name = 'Android Device'
    } else if (platforms.includes('iphone')) {
      info.name = 'iPhone'
    } else if (platforms.includes('ipad')) {
      info.name = 'iPad'
    } else if (platforms.includes('ios')) {
      info.name = 'iOS Device'
    } else {
      info.name = 'Unknown Device'
    }

    return info
  }
}
