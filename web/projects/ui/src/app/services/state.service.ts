import { inject, Injectable } from '@angular/core'
import { CanActivateFn, IsActiveMatchOptions, Router } from '@angular/router'
import { i18nPipe } from '@start9labs/shared'
import { TUI_TRUE_HANDLER } from '@taiga-ui/cdk'
import { TuiAlertService } from '@taiga-ui/core'
import {
  BehaviorSubject,
  combineLatest,
  concat,
  EMPTY,
  exhaustMap,
  from,
  merge,
  Observable,
  startWith,
  Subject,
  timer,
} from 'rxjs'
import {
  catchError,
  filter,
  map,
  shareReplay,
  skip,
  switchMap,
  take,
  takeUntil,
  tap,
} from 'rxjs/operators'
import { RR } from 'src/app/services/api/api.types'
import { ApiService } from 'src/app/services/api/embassy-api.service'
import { NetworkService } from 'src/app/services/network.service'

const OPTIONS: IsActiveMatchOptions = {
  paths: 'subset',
  queryParams: 'exact',
  fragment: 'ignored',
  matrixParams: 'ignored',
}

@Injectable({
  providedIn: 'root',
})
export class StateService extends Observable<RR.ServerState | null> {
  private readonly alerts = inject(TuiAlertService)
  private readonly i18n = inject(i18nPipe)
  private readonly api = inject(ApiService)
  private readonly router = inject(Router)
  private readonly network$ = inject(NetworkService)
  private readonly single$ = new Subject<RR.ServerState>()
  private readonly trigger$ = new BehaviorSubject<void>(undefined)
  private readonly poll$ = this.trigger$.pipe(
    switchMap(() =>
      timer(0, 2000).pipe(
        switchMap(() =>
          from(this.api.getState()).pipe(catchError(() => EMPTY)),
        ),
        take(1),
      ),
    ),
  )

  private readonly stream$ = merge(this.single$, this.poll$).pipe(
    tap(state => {
      switch (state) {
        case 'initializing':
          this.router.navigate(['initializing'], { replaceUrl: true })
          break
        case 'error':
          this.router.navigate(['diagnostic'], { replaceUrl: true })
          break
        case 'running':
          if (
            this.router.isActive('initializing', OPTIONS) ||
            this.router.isActive('diagnostic', OPTIONS)
          ) {
            this.router.navigate([''], { replaceUrl: true })
          }

          break
      }
    }),
    startWith(null),
    shareReplay(1),
  )

  private readonly alert = merge(
    this.trigger$.pipe(skip(1)),
    this.network$.pipe(filter(v => !v)),
  )
    .pipe(
      exhaustMap(() =>
        concat(
          this.alerts
            .open(this.i18n.transform('Trying to reach server'), {
              label: this.i18n.transform('State unknown'),
              closeable: false,
              appearance: 'negative',
            })
            .pipe(
              takeUntil(
                combineLatest([this.stream$.pipe(skip(1)), this.network$]).pipe(
                  filter(state => state.every(Boolean)),
                ),
              ),
            ),
          this.alerts.open(this.i18n.transform('Connection restored'), {
            label: this.i18n.transform('Server connected'),
            appearance: 'positive',
          }),
        ),
      ),
    )
    .subscribe()

  constructor() {
    super(subscriber => this.stream$.subscribe(subscriber))
  }

  retrigger() {
    this.trigger$.next()
  }

  async syncState() {
    const state = await this.api.getState()
    this.single$.next(state)
  }
}

export function stateNot(state: RR.ServerState[]): CanActivateFn {
  return () =>
    inject(StateService).pipe(
      filter(current => !current || !state.includes(current)),
      map(TUI_TRUE_HANDLER),
    )
}
