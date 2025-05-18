import { RouterLink } from '@angular/router'
import { TuiLet } from '@taiga-ui/cdk'
import { TuiButton, TuiTitle } from '@taiga-ui/core'
import { CommonModule } from '@angular/common'
import { ChangeDetectionStrategy, Component, inject } from '@angular/core'
import { ErrorService, i18nPipe, LoadingService } from '@start9labs/shared'
import { TuiHeader } from '@taiga-ui/layout'
import { from, map, merge, Observable, Subject } from 'rxjs'
import { ApiService } from 'src/app/services/api/embassy-api.service'
import { Session } from 'src/app/services/api/api.types'
import { TitleDirective } from 'src/app/services/title.service'
import { SessionsTableComponent } from './table.component'

@Component({
  template: `
    <ng-container *title>
      <a routerLink=".." tuiIconButton iconStart="@tui.arrow-left">Back</a>
      {{ 'Active Sessions' | i18n }}
    </ng-container>
    <header tuiHeader>
      <hgroup tuiTitle>
        <h3>{{ 'Active Sessions' | i18n }}</h3>
        <p tuiSubtitle>
          {{
            'A session is a device that is currently logged into StartOS. For best security, terminate sessions you do not recognize or no longer use.'
              | i18n
          }}
        </p>
      </hgroup>
    </header>
    <section class="g-card">
      <header>{{ 'Current session' | i18n }}</header>
      <div [single]="true" [sessions]="current$ | async"></div>
    </section>

    <section *tuiLet="other$ | async as others" class="g-card">
      <header>
        {{ 'Other sessions' | i18n }}
        @if (table.selected$ | async; as selected) {
          <button
            tuiButton
            size="xs"
            appearance="primary-destructive"
            [style.margin-inline-start]="'auto'"
            [disabled]="!selected.length"
            (click)="terminate(selected, others || [])"
          >
            {{ 'Terminate selected' | i18n }}
          </button>
        }
      </header>
      <div #table [sessions]="others"></div>
    </section>
  `,
  changeDetection: ChangeDetectionStrategy.OnPush,
  standalone: true,
  imports: [
    CommonModule,
    TuiButton,
    SessionsTableComponent,
    TuiLet,
    RouterLink,
    TitleDirective,
    TuiHeader,
    TuiTitle,
    i18nPipe,
  ],
})
export default class SystemSessionsComponent {
  private readonly loader = inject(LoadingService)
  private readonly errorService = inject(ErrorService)
  private readonly api = inject(ApiService)
  private readonly sessions$ = from(this.api.getSessions({}))
  private readonly local$ = new Subject<readonly SessionWithId[]>()

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

  async terminate(
    sessions: readonly SessionWithId[],
    all: readonly SessionWithId[],
  ) {
    const ids = sessions.map(s => s.id)
    const loader = this.loader.open('Terminating sessions').subscribe()

    try {
      await this.api.killSessions({ ids })
      this.local$.next(all.filter(s => !ids.includes(s.id)))
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
