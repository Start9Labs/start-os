import { InjectionToken, Injector } from '@angular/core'
import { bufferTime, catchError, switchMap, take, tap } from 'rxjs/operators'
import { Update } from 'patch-db-client'
import { DataModel } from './data-model'
import { defer, EMPTY, from, interval, merge, Observable } from 'rxjs'
import { AuthService } from '../auth.service'
import { ConnectionService } from '../connection.service'
import { ApiService } from '../api/embassy-api.service'

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

    const websocket$ = api.openPatchWebsocket$().pipe(
      bufferTime(250),
      catchError((_, watch$) => {
        connectionService.websocketConnected$.next(false)

        return interval(4000).pipe(
          switchMap(() =>
            from(api.echo({ message: 'ping' })).pipe(catchError(() => EMPTY)),
          ),
          take(1),
          switchMap(() => watch$),
        )
      }),
      tap(() => connectionService.websocketConnected$.next(true)),
    )

    return authService.isVerified$.pipe(
      switchMap(verified =>
        verified ? merge(websocket$, api.patchStream$) : EMPTY,
      ),
    )
  })
}
