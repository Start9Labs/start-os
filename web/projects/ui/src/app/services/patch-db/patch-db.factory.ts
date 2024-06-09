import { InjectionToken, Injector } from '@angular/core'
import { Revision, Update } from 'patch-db-client'
import { defer, EMPTY, from, Observable } from 'rxjs'
import {
  bufferTime,
  catchError,
  filter,
  startWith,
  switchMap,
  take,
  tap,
} from 'rxjs/operators'
import { StateService } from 'src/app/services/state.service'
import { ApiService } from '../api/embassy-api.service'
import { AuthService } from '../auth.service'
import { ConnectionService } from '../connection.service'
import { DataModel } from './data-model'
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
    const auth = injector.get(AuthService)
    const state = injector.get(StateService)
    const connectionService = injector.get(ConnectionService)
    const bootstrapper = injector.get(LocalStorageBootstrap)

    return defer(() => from(api.subscribeToPatchDB({}))).pipe(
      switchMap(({ dump, guid }) =>
        auth.isVerified$.pipe(
          switchMap(verified =>
            verified
              ? api.openWebsocket$<Revision>(guid, {}).pipe(
                  bufferTime(250),
                  filter(revisions => !!revisions.length),
                  tap(() => connectionService.websocketConnected$.next(true)),
                  startWith([dump]),
                )
              : EMPTY,
          ),
        ),
      ),
      catchError((_, original$) => {
        connectionService.websocketConnected$.next(false)
        state.retrigger()

        return state.pipe(
          filter(current => current === 'running'),
          take(1),
          switchMap(() => original$),
        )
      }),
      startWith([bootstrapper.init()]),
    )
  })
}
