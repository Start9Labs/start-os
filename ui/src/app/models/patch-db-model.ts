import { Injectable } from "@angular/core";
import { initPatchDb, PatchDB, RxStore, Store } from "patch-db-client";
import { Observable, Subscription } from "rxjs";
import { DataModel } from "./data-model";
import { LocalStorageBootstrap } from "./local-storage-bootstrap";

@Injectable({
  providedIn: 'root'
})
export class PatchDBClient {
  private patchDb: PatchDB<DataModel>
  private store: Store<DataModel>
  private syncSub: Subscription
  constructor(private readonly storage: Storage) { }

  get peek(): DataModel { return this.store.peek }
  watch(): Observable<DataModel>;
  watch<P1 extends keyof DataModel>(p1: P1): Observable<DataModel[P1]>;
  watch<P1 extends keyof DataModel, P2 extends keyof DataModel[P1]>(p1: P1, p2: P2): Observable<DataModel[P1][P2]>;
  watch<P1 extends keyof DataModel, P2 extends keyof DataModel[P1], P3 extends keyof DataModel[P1][P2]>(p1: P1, p2: P2, p3: P3): Observable<DataModel[P1][P2][P3]>;
  watch<P1 extends keyof DataModel, P2 extends keyof DataModel[P1], P3 extends keyof DataModel[P1][P2], P4 extends keyof DataModel[P1][P2][P3]>(p1: P1, p2: P2, p3: P3, p4: P4): Observable<DataModel[P1][P2][P3][P4]>;
  watch<P1 extends keyof DataModel, P2 extends keyof DataModel[P1], P3 extends keyof DataModel[P1][P2], P4 extends keyof DataModel[P1][P2][P3], P5 extends keyof DataModel[P1][P2][P3][P4]>(p1: P1, p2: P2, p3: P3, p4: P4, p5: P5): Observable<DataModel[P1][P2][P3][P4][P5]>;
  watch<P1 extends keyof DataModel, P2 extends keyof DataModel[P1], P3 extends keyof DataModel[P1][P2], P4 extends keyof DataModel[P1][P2][P3], P5 extends keyof DataModel[P1][P2][P3][P4], P6 extends keyof DataModel[P1][P2][P3][P4][P5]>(p1: P1, p2: P2, p3: P3, p4: P4, p5: P5, p6: P6): Observable<DataModel[P1][P2][P3][P4][P5][P6]>;
  watch(...args: (string | number)[]): Observable<any> {
    return this.store.watch<any>(args)
  }

  async init() {
    await this.storage.ready()
    if(this.patchDb || this.store) return console.warn('Cannot re-init patchDbModel')

    const { patchDb, store } = await initPatchDb<DataModel>({
      source: undefined as any,
      http: undefined as any,
      store: undefined as any,
      bootstrap: new LocalStorageBootstrap(this.storage),
    })
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