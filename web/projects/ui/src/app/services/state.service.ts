import { inject, Injectable } from '@angular/core'
import { CanActivateFn, IsActiveMatchOptions, Router } from '@angular/router'
import { ALWAYS_TRUE_HANDLER } from '@taiga-ui/cdk'
import { TuiAlertService, TuiNotification } from '@taiga-ui/core'
import {
  BehaviorSubject,
  combineLatest,
  concat,
  exhaustMap,
  from,
  merge,
  Observable,
  retry,
  startWith,
} from 'rxjs'
import {
  filter,
  map,
  shareReplay,
  skip,
  switchMap,
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
  private readonly api = inject(ApiService)
  private readonly router = inject(Router)
  private readonly network$ = inject(NetworkService)
  private readonly trigger$ = new BehaviorSubject<void>(undefined)
  private readonly stream$ = this.trigger$.pipe(
    switchMap(() =>
      from(this.api.getState()).pipe(retry({ delay: 2000 }), startWith(null)),
    ),
    tap(state => {
      switch (state) {
        case 'initializing':
          this.router.navigate(['initializing'])
          break
        case 'error':
          this.router.navigate(['diagnostic'])
          break
        case 'running':
          if (
            this.router.isActive('initializing', OPTIONS) ||
            this.router.isActive('diagnostic', OPTIONS)
          ) {
            this.router.navigate([''])
          }

          break
      }
    }),
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
            .open('Trying to reach server', {
              label: 'State unknown',
              autoClose: false,
              status: TuiNotification.Error,
            })
            .pipe(
              takeUntil(
                combineLatest([this.stream$, this.network$]).pipe(
                  filter(state => state.every(Boolean)),
                ),
              ),
            ),
          this.alerts.open('Connection restored', {
            label: 'Server reached',
            status: TuiNotification.Success,
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
}

export function stateNot(state: RR.ServerState[]): CanActivateFn {
  return () =>
    inject(StateService).pipe(
      filter(current => !current || !state.includes(current)),
      map(ALWAYS_TRUE_HANDLER),
    )
}
