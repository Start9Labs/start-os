import { inject, Injectable, InjectionToken } from '@angular/core'
import { Dump, Revision, Update } from 'patch-db-client'
import { BehaviorSubject, EMPTY, Observable } from 'rxjs'
import { bufferTime, filter, startWith, switchMap } from 'rxjs/operators'
import { retryWithState } from 'src/app/util/retry-with-state'
import { ApiService } from '../api/embassy-api.service'
import { AuthService } from '../auth.service'
import { DataModel } from './data-model'
import { LocalStorageBootstrap } from './local-storage-bootstrap'

export const PATCH_CACHE = new InjectionToken('', {
  factory: () =>
    new BehaviorSubject<Dump<DataModel>>({
      id: 0,
      value: {} as DataModel,
    }),
})

@Injectable({
  providedIn: 'root',
})
export class PatchDbSource extends Observable<Update<DataModel>[]> {
  private readonly api = inject(ApiService)
  private readonly stream$ = inject(AuthService).isVerified$.pipe(
    switchMap(verified => (verified ? this.api.subscribeToPatchDB({}) : EMPTY)),
    switchMap(({ dump, guid }) =>
      this.api.openWebsocket$<Revision>(guid).pipe(
        bufferTime(250),
        filter(revisions => !!revisions.length),
        startWith([dump]),
      ),
    ),
    retryWithState(),
    startWith([inject(LocalStorageBootstrap).init()]),
  )

  constructor() {
    super(subscriber => this.stream$.subscribe(subscriber))
  }
}
