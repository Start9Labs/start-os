import { InjectionToken, Injector } from '@angular/core'
import { Update } from 'patch-db-client'
import {
  bufferTime,
  catchError,
  filter,
  switchMap,
  take,
  tap,
  defer,
  EMPTY,
  from,
  interval,
  Observable,
} from 'rxjs'
import { DataModel } from './data-model'
import { AuthService } from '../auth.service'
import { ConnectionService } from '../connection.service'
import { ApiService } from '../api/embassy-api.service'
import { ConfigService } from '../config.service'

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
    const isTor = configService.isTor()
    const timeout = isTor ? 16000 : 4000

    const websocket$ = api.openPatchWebsocket$().pipe(
      bufferTime(250),
      filter(updates => !!updates.length),
      catchError((_, watch$) => {
        connectionService.websocketConnected$.next(false)

        return interval(timeout).pipe(
          switchMap(() =>
            from(api.echo({ message: 'ping', timeout })).pipe(
              catchError(() => EMPTY),
            ),
          ),
          take(1),
          switchMap(() => watch$),
        )
      }),
      tap(() => connectionService.websocketConnected$.next(true)),
    )

    return authService.isVerified$.pipe(
      switchMap(verified => (verified ? websocket$ : EMPTY)),
    )
  })
}
