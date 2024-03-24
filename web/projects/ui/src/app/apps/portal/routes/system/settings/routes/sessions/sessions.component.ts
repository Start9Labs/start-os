import { CommonModule } from '@angular/common'
import { ChangeDetectionStrategy, Component, inject } from '@angular/core'
import { ErrorService, LoadingService } from '@start9labs/shared'
import { TuiButtonModule } from '@taiga-ui/experimental'
import { from, map, merge, Observable, Subject } from 'rxjs'
import { ApiService } from 'src/app/services/api/embassy-api.service'
import { Session } from 'src/app/services/api/api.types'
import { SSHTableComponent } from './table.component'
import { TuiLetModule } from '@taiga-ui/cdk'

@Component({
  template: `
    <h3 class="g-title">Current session</h3>
    <table
      class="g-table"
      [single]="true"
      [sessions]="current$ | async"
    ></table>

    <ng-container *tuiLet="other$ | async as others">
      <h3 class="g-title">
        Other sessions
        <button
          *ngIf="table.selected$ | async as selected"
          tuiButton
          size="xs"
          appearance="error"
          [disabled]="!selected.length"
          (click)="terminate(selected, others || [])"
        >
          Terminate selected
        </button>
      </h3>
      <table #table class="g-table" [sessions]="others"></table>
    </ng-container>
  `,
  changeDetection: ChangeDetectionStrategy.OnPush,
  standalone: true,
  imports: [CommonModule, TuiButtonModule, SSHTableComponent, TuiLetModule],
})
export class SettingsSessionsComponent {
  private readonly loader = inject(LoadingService)
  private readonly errorService = inject(ErrorService)
  private readonly api = inject(ApiService)
  private readonly sessions$ = from(this.api.getSessions({}))
  private readonly local$ = new Subject<readonly SessionWithId[]>()

  readonly current$ = this.sessions$.pipe(map(s => [s.sessions[s.current]]))
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
    const loader = this.loader
      .open(`Terminating session${ids.length > 1 ? 's' : ''}...`)
      .subscribe()

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
