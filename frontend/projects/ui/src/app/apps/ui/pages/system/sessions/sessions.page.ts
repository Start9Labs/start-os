import { Component } from '@angular/core'
import { Pipe, PipeTransform } from '@angular/core'
import { ErrorService, LoadingService } from '@start9labs/shared'
import { ApiService } from 'src/app/services/api/embassy-api.service'
import { PlatformType, Session } from 'src/app/services/api/api.types'
import { Observable, Subject, from, map, merge, shareReplay } from 'rxjs'

@Component({
  selector: 'sessions',
  templateUrl: 'sessions.page.html',
  styleUrls: ['sessions.page.scss'],
})
export class SessionsPage {
  private readonly sessions$ = from(this.api.getSessions({}))
  private readonly localOther$ = new Subject<SessionWithId[]>()
  private readonly remoteOther$: Observable<SessionWithId[]> =
    this.sessions$.pipe(
      map(s =>
        Object.entries(s.sessions)
          .filter(([id, _]) => id !== s.current)
          .map(([id, session]) => ({
            id,
            ...session,
          }))
          .sort(
            (a, b) =>
              new Date(b['last-active']).valueOf() -
              new Date(a['last-active']).valueOf(),
          ),
      ),
    )

  readonly currentSession$ = this.sessions$.pipe(
    map(s => s.sessions[s.current]),
    shareReplay(),
  )

  readonly otherSessions$ = merge(this.localOther$, this.remoteOther$)

  selected: Record<string, boolean> = {}

  constructor(
    private readonly loader: LoadingService,
    private readonly errorService: ErrorService,
    private readonly api: ApiService,
  ) {}

  get empty() {
    return this.count === 0
  }

  get count() {
    return Object.keys(this.selected).length
  }

  async toggleChecked(id: string) {
    if (this.selected[id]) {
      delete this.selected[id]
    } else {
      this.selected[id] = true
    }
  }

  async toggleAll(otherSessions: SessionWithId[]) {
    if (this.empty) {
      otherSessions.forEach(s => (this.selected[s.id] = true))
    } else {
      this.selected = {}
    }
  }

  async kill(otherSessions: SessionWithId[]): Promise<void> {
    const ids = Object.keys(this.selected)

    const loader = this.loader
      .open(`Terminating session${ids.length > 1 ? 's' : ''}...`)
      .subscribe()

    try {
      await this.api.killSessions({ ids })
      this.selected = {}
      this.localOther$.next(otherSessions.filter(s => !ids.includes(s.id)))
    } catch (e: any) {
      this.errorService.handleError(e)
    } finally {
      loader.unsubscribe()
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
