import { InjectionToken } from '@angular/core'
import { catchError, retry, switchMap, tap, timeout } from 'rxjs/operators'
import { Bootstrapper, DBCache, Update } from 'patch-db-client'
import { DataModel } from './data-model'
import { WebSocketSubjectConfig } from 'rxjs/webSocket'
import { combineLatest, EMPTY, from, Observable, of } from 'rxjs'
import { AuthService } from '../auth.service'
import { ConnectionFailure, ConnectionService } from '../connection.service'
import { ApiService } from '../api/embassy-api.service'

export const PATCH_SOURCE = new InjectionToken<Observable<Update<DataModel>>>(
  '',
)
export const PATCH_CACHE = new InjectionToken<DBCache<DataModel>>('', {
  factory: () => ({} as any),
})
export const BOOTSTRAPPER = new InjectionToken<Bootstrapper<DataModel>>('')

export function sourceFactory(
  api: ApiService,
  authService: AuthService,
  connectionService: ConnectionService,
): Observable<Update<DataModel>> {
  const config: WebSocketSubjectConfig<Update<DataModel>> = {
    url: `/db`,
    closeObserver: {
      next: val => {
        if (val.reason === 'UNAUTHORIZED') authService.setUnverified()
      },
    },
  }

  const websocket$ = api.openPatchWebsocket$(config).pipe(
    timeout({ first: 21000 }),
    catchError((e, watch$) => {
      connectionService.setPatchError(e)

      return of('').pipe(
        switchMap(() => from(api.echo({ message: 'ping' }))),
        retry({ delay: 4000 }),
        switchMap(() => watch$),
      )
    }),
    tap(() => connectionService.setPatchError(null)),
  )

  return combineLatest([
    authService.isVerified$,
    connectionService.watchFailure$,
  ]).pipe(
    switchMap(([verified, failure]) => {
      return verified && failure !== ConnectionFailure.Network
        ? websocket$
        : EMPTY
    }),
  )
}
