import { Injectable } from '@angular/core'
import { initPatchDb, PatchDB, PatchDbConfig, PatchDocument, Store } from 'patch-db-client'
import { BehaviorSubject, combineLatest, Subscription } from 'rxjs'
import { filter, map } from 'rxjs/operators'
import { exists } from '../../util/misc.util'
import { DataModel } from './data-model'

@Injectable({
  providedIn: 'root',
})
export class PatchDbModel {
  private patchDb: PatchDB<DataModel>
  private store: Store<DataModel>
  private syncSub: Subscription
  constructor (private readonly conf: PatchDbConfig<DataModel>) { }

  get peek (): DataModel { return this.store.peek }

  watch: Store<DataModel>['watch'] = (...args: (string | number)[]) => {
    const overlay = this.getOverlay(...args).pipe(filter(exists))
    const base = (this.store.watch as any)(...args)
    return combineLatest([overlay, base]).pipe(
      map(([o, b]) => {
        if (!o) return b
        if (o.expired(b)) {
          this.clearOverlay(...args)
          return b
        } else {
          return o
        }
      }),
    )
  }

  /* overlays allow the FE to override the patch-db values for FE behavior not represented in the BE. For example, the server status of 'Unreachable' is set with
    `patchDbModel.overlay({ expired: () => true, value: 'UNREACHABLE' }, 'server', 'status')`
    And will expire as soon as a genuine server status emits from the BE.
  */
  private readonly overlays: { [path: string]: BehaviorSubject<{ value: any, expired: (newValue: any) => boolean }>} = { }

  setOverlay (args: { expired: (newValue: any) => boolean, value: any }, ...path: (string | number)[]) {
    this.watch('apps', 'bitcoind', 'actions') // @TODO why is this here?
    this.getOverlay(...path).next(args)
  }

  private getOverlay (...path: (string | number)[]): BehaviorSubject<{ value: any, expired: (newValue: any) => boolean } | undefined> {
    const singlePath = '/' + path.join('/')
    this.overlays[singlePath] = this.overlays[singlePath] || new BehaviorSubject(undefined)
    return this.overlays[singlePath]
  }

  private clearOverlay (...path: (string | number)[]): void {
    this.getOverlay(...path).next(undefined)
  }

  async init () {
    if (this.patchDb || this.store) return console.warn('Cannot re-init patchDbModel')
    await this.conf.bootstrap.init()
    const { patchDb, store } = await initPatchDb<DataModel>(this.conf)
    this.patchDb = patchDb
    this.store = store

    this.start()
  }

  stop () {
    this.syncSub.unsubscribe()
    this.syncSub = undefined
  }

  start () {
    if (this.syncSub) this.stop()

    this.syncSub = this.patchDb.startSync().subscribe({
      error: e => console.error('Critical, patch-db-sync sub error', e),
      complete: () => console.error('Critical, patch-db-sync sub complete'),
    })
  }

  patch (ops: PatchDocument): void {
    this.store.applyPatchDocument(ops)
  }
}