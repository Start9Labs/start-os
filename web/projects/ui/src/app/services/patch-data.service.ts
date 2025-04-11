import { Injectable } from '@angular/core'
import { Observable } from 'rxjs'
import { filter, map, share, switchMap } from 'rxjs/operators'
import { PatchDB } from 'patch-db-client'
import { DataModel } from 'src/app/services/patch-db/data-model'
import { EOSService } from 'src/app/services/eos.service'
import { ConnectionService } from 'src/app/services/connection.service'
import { LocalStorageBootstrap } from './patch-db/local-storage-bootstrap'

// Get data from PatchDb after is starts and act upon it
@Injectable({
  providedIn: 'root',
})
export class PatchDataService extends Observable<void> {
  private readonly stream$ = this.connection$.pipe(
    filter(Boolean),
    switchMap(() => this.patch.watch$()),
    map((cache, index) => {
      this.bootstrapper.update(cache)

      if (index === 0) {
        // check for updates to StartOS and services
        this.checkForUpdates()
      }
    }),
    share(),
  )

  constructor(
    private readonly patch: PatchDB<DataModel>,
    private readonly eosService: EOSService,
    private readonly connection$: ConnectionService,
    private readonly bootstrapper: LocalStorageBootstrap,
  ) {
    super(subscriber => this.stream$.subscribe(subscriber))
  }

  private checkForUpdates(): void {
    this.eosService.loadEos()
    // this.marketplaceService.getMarketplace$().pipe(take(1)).subscribe()
  }
}
