import { InjectionToken } from '@angular/core'
import { catchError, switchMap, take, tap } from 'rxjs/operators'
import { Bootstrapper, DBCache, Update } from 'patch-db-client'
import { DataModel } from './data-model'
import { EMPTY, from, interval, Observable } from 'rxjs'
import { AuthService } from '../auth.service'
import { ConnectionService } from '../connection.service'
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
  const websocket$ = api.openPatchWebsocket$().pipe(
    catchError((e, watch$) => {
      connectionService.setPatchError(e)

      return interval(4000).pipe(
        switchMap(() =>
          from(api.echo({ message: 'ping' })).pipe(catchError(() => EMPTY)),
        ),
        take(1),
        switchMap(() => watch$),
      )
    }),
    tap(() => connectionService.setPatchError(null)),
  )

  return authService.isVerified$.pipe(
    switchMap(verified => (verified ? websocket$ : EMPTY)),
  )
}
