import { Injectable } from "@angular/core";
import { initPatchDb, PatchDB, PatchDbConfig, Store } from "patch-db-client";
import { BehaviorSubject, combineLatest, Observable, Subscription } from "rxjs";
import { filter, map } from "rxjs/operators";
import { exists } from "../../util/misc.util";
import { DataModel } from "./data-model";
import { LocalStorageBootstrap } from "./local-storage-bootstrap";

@Injectable({
  providedIn: 'root'
})
export class PatchDbModel {
  private patchDb: PatchDB<DataModel>
  private store: Store<DataModel>
  private syncSub: Subscription

  constructor(private readonly conf: PatchDbConfig<DataModel>) {}

  get peek(): DataModel { return this.store.peek }
  watch(): Observable<DataModel>;
  watch<P1 extends keyof DataModel>(p1: P1): Observable<DataModel[P1]>;
  watch<P1 extends keyof DataModel, P2 extends keyof DataModel[P1]>(p1: P1, p2: P2): Observable<DataModel[P1][P2]>;
  watch<P1 extends keyof DataModel, P2 extends keyof DataModel[P1], P3 extends keyof DataModel[P1][P2]>(p1: P1, p2: P2, p3: P3): Observable<DataModel[P1][P2][P3]>;
  watch<P1 extends keyof DataModel, P2 extends keyof DataModel[P1], P3 extends keyof DataModel[P1][P2], P4 extends keyof DataModel[P1][P2][P3]>(p1: P1, p2: P2, p3: P3, p4: P4): Observable<DataModel[P1][P2][P3][P4]>;
  watch<P1 extends keyof DataModel, P2 extends keyof DataModel[P1], P3 extends keyof DataModel[P1][P2], P4 extends keyof DataModel[P1][P2][P3], P5 extends keyof DataModel[P1][P2][P3][P4]>(p1: P1, p2: P2, p3: P3, p4: P4, p5: P5): Observable<DataModel[P1][P2][P3][P4][P5]>;
  watch<P1 extends keyof DataModel, P2 extends keyof DataModel[P1], P3 extends keyof DataModel[P1][P2], P4 extends keyof DataModel[P1][P2][P3], P5 extends keyof DataModel[P1][P2][P3][P4], P6 extends keyof DataModel[P1][P2][P3][P4][P5]>(p1: P1, p2: P2, p3: P3, p4: P4, p5: P5, p6: P6): Observable<DataModel[P1][P2][P3][P4][P5][P6]>;
  watch(...args: (string | number)[]): Observable<any> {
    const overlay = this.getOverlay(...args).pipe(filter(exists))
    const base = (this.store.watch as any)(...args)
    return combineLatest([overlay, base]).pipe(
      map(([o, b]) => {
        if(!o) return b
        if(o.shouldExpire(b)) {
          this.clearOverlay(...args)
          return b
        } else {
          return o
        }
      })
    )
  }

  private readonly overlays: { [path: string]: BehaviorSubject<{ value: any, shouldExpire: (newValue: any) => boolean }>} = { }
  setOverlay(shouldExpire: (newValue: any) => boolean, value: any, ...path: (string | number)[]) {
    this.getOverlay(...path).next({ shouldExpire, value })
  }
  getOverlay(...path: (string | number)[]): BehaviorSubject<{ value: any, shouldExpire: (newValue: any) => boolean } | undefined> {
    const singlePath = '/' + path.join('/')
    this.overlays[singlePath] = this.overlays[singlePath] || new BehaviorSubject(undefined)
    return this.overlays[singlePath]
  }
  clearOverlay(...path: (string | number)[]): void {
    this.getOverlay(...path).next(undefined)
  }

  async init() {
    if(this.patchDb || this.store) return console.warn('Cannot re-init patchDbModel')

    const { patchDb, store } = await initPatchDb<DataModel>(this.conf)
    this.patchDb = patchDb
    this.store = store

    this.start()
  }

  stop() {
    this.syncSub.unsubscribe()
    this.syncSub = undefined
  }
  start() {
    if(this.syncSub) this.stop()

    this.syncSub = this.patchDb.startSync().subscribe({
      error: e => console.error('Critical, patch-db-sync sub error', e),
      complete: () => console.error('Critical, patch-db-sync sub complete')
    })
  }
}