import { Inject, Injectable, InjectionToken } from '@angular/core'
import { PatchDB, PatchDbConfig, Store } from 'patch-db-client'
import { Observable, of, Subscription } from 'rxjs'
import { catchError, finalize } from 'rxjs/operators'
import { DataModel } from './data-model'

export const PATCH_CONFIG = new InjectionToken<PatchDbConfig<DataModel>>('app.config')

@Injectable({
  providedIn: 'root',
})
export class PatchDbModel {
  private patchDb: PatchDB<DataModel>
  private syncSub: Subscription
  initialized = false

  constructor (
    @Inject(PATCH_CONFIG) private readonly conf: PatchDbConfig<DataModel>,
  ) { }

  async init (): Promise<void> {
    if (this.patchDb) return console.warn('Cannot re-init patchDbModel')
    try {
      this.patchDb = await PatchDB.init<DataModel>(this.conf)
      this.initialized = true
    } catch (e) {
      console.log('Failed to initialize PatchDB', e)
    }
  }

  start (): void {
    if (this.syncSub) this.stop()
    this.syncSub = this.patchDb.sync$().subscribe({
      error: e => console.error('Critical, patch-db-sync sub error', e),
      complete: () => console.error('Critical, patch-db-sync sub complete'),
    })
  }

  stop (): void {
    if (this.syncSub) {
      this.syncSub.unsubscribe()
      this.syncSub = undefined
    }
  }

  watch$: Store<DataModel>['watch$'] = (...args: (string | number)[]): Observable<DataModel> => {
    // console.log('WATCHING')
    return this.patchDb.store.watch$(...(args as [])).pipe(
      catchError(e => {
        console.error(e)
        return of(e.message)
      }),
      // finalize(() => console.log('unSUBSCRIBing')),
    )
  }
}
