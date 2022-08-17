import { InjectionToken } from '@angular/core'
import { catchError, retry, switchMap, tap, timeout } from 'rxjs/operators'
import { Bootstrapper, DBCache, Update } from 'patch-db-client'
import { DataModel } from './data-model'
import { WebSocketSubjectConfig } from 'rxjs/webSocket'
import { EMPTY, from, Observable } from 'rxjs'
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
  const config: WebSocketSubjectConfig<Update<DataModel>> = {
    url: `/db`,
    closeObserver: {
      next: val => {
        if (val.reason === 'UNAUTHORIZED') {
          authService.setUnverified()
        }
      },
    },
  }

  const websocket$ = api.openPatchWebsocket$(config).pipe(
    timeout({ first: 21000 }),
    catchError((e, watch$) => {
      connectionService.setPatchError(e)

      return from(api.echo({ message: 'ping' })).pipe(
        retry({ delay: 4000 }),
        switchMap(() => watch$),
      )
    }),
    tap(() => connectionService.setPatchError(null)),
  )

  return authService.isVerified$.pipe(
    switchMap(verified => (verified ? websocket$ : EMPTY)),
  )
}

// export function mockSourceFactory(api: MockApiService): Observable<Update<DataModel>> {
//   return api.mockPatch$
// }

// export function realSourceFactory(
//   api: ApiService,
//   authService: AuthService,
//   connectionService: ConnectionService,
// ): Observable<Update<DataModel>> {

//   const config: WebSocketSubjectConfig<Update<DataModel>> = {
//     url: `/db`,
//     closeObserver: {
//       next: (val) => {
//         if (val.reason === 'UNAUTHORIZED') {
//           authService.setUnverified()
//         }
//       }
//     }
//   }

//   const websocket$ = api.openPatchWebsocket$(config).pipe(
//     timeout({ first: 21000 }),
//     catchError((e, watch$) => {
//       connectionService.setPatchDbError(e)

//       return from(api.echo({ message: 'ping' })).pipe(
//         retry({ delay: 4000 }),
//         switchMap(() => watch$)
//       )
//     }),
//     tap(() => connectionService.setPatchDbError(null))
//   )

//   return authService.isVerified$.pipe(
//     switchMap(verified => verified ? websocket$ : EMPTY)
//   )
// }
