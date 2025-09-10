import { CommonModule } from '@angular/common'
import {
  ChangeDetectionStrategy,
  Component,
  inject,
  viewChild,
} from '@angular/core'
import { RouterLink } from '@angular/router'
import { ErrorService, i18nPipe, LoadingService } from '@start9labs/shared'
import { TuiButton } from '@taiga-ui/core'
import { from, map, merge, Observable, Subject } from 'rxjs'
import { Session } from 'src/app/services/api/api.types'
import { ApiService } from 'src/app/services/api/embassy-api.service'
import { TitleDirective } from 'src/app/services/title.service'
import { SessionsTableComponent } from './table.component'

@Component({
  template: `
    <ng-container *title>
      <a routerLink=".." tuiIconButton iconStart="@tui.arrow-left">
        {{ 'Back' | i18n }}
      </a>
      {{ 'Active Sessions' | i18n }}
    </ng-container>

    <section class="g-card">
      <header>{{ 'Current session' | i18n }}</header>
      <div [single]="true" [sessions]="current$ | async"></div>
    </section>

    @let others = other$ | async;
    <section class="g-card">
      <header>
        {{ 'Other sessions' | i18n }}
        <button
          tuiButton
          size="xs"
          appearance="primary-destructive"
          [style.margin-inline-start]="'auto'"
          [disabled]="!sessions()?.selected()?.length"
          (click)="terminate(others || [])"
        >
          {{ 'Terminate selected' | i18n }}
        </button>
      </header>
      <div #table [sessions]="others"></div>
    </section>
  `,
  styles: `
    :host {
      max-width: 80rem;
    }
  `,
  changeDetection: ChangeDetectionStrategy.OnPush,
  imports: [
    CommonModule,
    TuiButton,
    SessionsTableComponent,
    RouterLink,
    TitleDirective,
    i18nPipe,
  ],
})
export default class SystemSessionsComponent {
  private readonly loader = inject(LoadingService)
  private readonly errorService = inject(ErrorService)
  private readonly api = inject(ApiService)
  private readonly sessions$ = from(this.api.getSessions({}))
  private readonly local$ = new Subject<readonly SessionWithId[]>()

  protected sessions = viewChild<SessionsTableComponent<SessionWithId>>('table')

  readonly current$ = this.sessions$.pipe(
    map(s => {
      const current = s.sessions[s.current]

      return current ? [current] : []
    }),
  )
  readonly other$: Observable<readonly SessionWithId[]> = merge(
    this.local$,
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
              new Date(b.lastActive).valueOf() -
              new Date(a.lastActive).valueOf(),
          ),
      ),
    ),
  )

  async terminate(all: readonly SessionWithId[]) {
    const ids =
      this.sessions()
        ?.selected()
        .map(s => s.id) || []
    const loader = this.loader.open('Terminating sessions').subscribe()

    try {
      await this.api.killSessions({ ids })
      this.local$.next(all.filter(s => !ids.includes(s.id)))
      this.sessions()?.selected.set([])
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
