import { InjectionToken, Injector } from '@angular/core'
import {
  bufferTime,
  catchError,
  filter,
  startWith,
  switchMap,
  take,
  tap,
} from 'rxjs/operators'
import { Revision, Update } from 'patch-db-client'
import { DataModel } from './data-model'
import { defer, EMPTY, from, interval, Observable } from 'rxjs'
import { AuthService } from '../auth.service'
import { ConnectionService } from '../connection.service'
import { ApiService } from '../api/embassy-api.service'
import { ConfigService } from '../config.service'
import { LocalStorageBootstrap } from './local-storage-bootstrap'

export const PATCH_SOURCE = new InjectionToken<Observable<Update<DataModel>[]>>(
  '',
)

export function sourceFactory(
  injector: Injector,
): Observable<Update<DataModel>[]> {
  // defer() needed to avoid circular dependency with ApiService, since PatchDB is needed there
  return defer(() => {
    const api = injector.get(ApiService)
    const authService = injector.get(AuthService)
    const connectionService = injector.get(ConnectionService)
    const configService = injector.get(ConfigService)
    const bootstrapper = injector.get(LocalStorageBootstrap)
    const isTor = configService.isTor()
    const timeout = isTor ? 16000 : 4000

    return from(api.subscribeToPatchDB({})).pipe(
      switchMap(({ dump, guid }) => {
        const websocket$ = api.openWebsocket$<Revision>(guid, {}).pipe(
          bufferTime(250),
          filter(revisions => !!revisions.length),
          // @TODO catch error should defer back to the global poll for state
          catchError((_, watch$) => {
            connectionService.websocketConnected$.next(false)

            return interval(timeout).pipe(
              switchMap(() =>
                from(api.getState()).pipe(catchError(() => EMPTY)),
              ),
              take(1),
              switchMap(() => watch$),
            )
          }),
          tap(() => connectionService.websocketConnected$.next(true)),
          startWith([dump]),
        )

        return authService.isVerified$.pipe(
          switchMap(verified => (verified ? websocket$ : EMPTY)),
        )
      }),
      startWith([bootstrapper.init()]),
    )
  })
}
