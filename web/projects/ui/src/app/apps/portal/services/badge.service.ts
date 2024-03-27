import { inject, Injectable } from '@angular/core'
import { AbstractMarketplaceService } from '@start9labs/marketplace'
import { Emver } from '@start9labs/shared'
import { PatchDB } from 'patch-db-client'
import {
  combineLatest,
  EMPTY,
  filter,
  first,
  map,
  Observable,
  pairwise,
  shareReplay,
  startWith,
  switchMap,
} from 'rxjs'
import { EOSService } from 'src/app/services/eos.service'
import { DataModel } from 'src/app/services/patch-db/data-model'
import { MarketplaceService } from 'src/app/services/marketplace.service'
import { ConnectionService } from 'src/app/services/connection.service'
import { getManifest } from 'src/app/util/get-package-data'

@Injectable({
  providedIn: 'root',
})
export class BadgeService {
  private readonly emver = inject(Emver)
  private readonly patch = inject(PatchDB<DataModel>)
  private readonly settings$ = combineLatest([
    this.patch.watch$('serverInfo', 'ntpSynced'),
    inject(EOSService).updateAvailable$,
  ]).pipe(map(([synced, update]) => Number(!synced) + Number(update)))
  private readonly marketplace = inject(
    AbstractMarketplaceService,
  ) as MarketplaceService

  private readonly local$ = inject(ConnectionService).connected$.pipe(
    filter(Boolean),
    switchMap(() => this.patch.watch$('packageData').pipe(first())),
    switchMap(outer =>
      this.patch.watch$('packageData').pipe(
        pairwise(),
        filter(([prev, curr]) =>
          Object.values(prev).some(p => {
            const { id } = getManifest(p)

            return (
              !curr[id] ||
              (p.stateInfo.installingInfo && !curr[id].stateInfo.installingInfo)
            )
          }),
        ),
        map(([_, curr]) => curr),
        startWith(outer),
      ),
    ),
  )

  private readonly updates$ = combineLatest([
    this.marketplace.getMarketplace$(true),
    this.local$,
  ]).pipe(
    map(
      ([marketplace, local]) =>
        Object.entries(marketplace).reduce(
          (list, [_, store]) =>
            store?.packages.reduce(
              (result, { manifest: { id, version } }) =>
                local[id] &&
                this.emver.compare(version, getManifest(local[id]).version) ===
                  1
                  ? result.add(id)
                  : result,
              list,
            ) || list,
          new Set<string>(),
        ).size,
    ),
    shareReplay(1),
  )

  getCount(id: string): Observable<number> {
    switch (id) {
      case '/portal/system/updates':
        return this.updates$
      case '/portal/system/settings':
        return this.settings$
      default:
        return EMPTY
    }
  }
}
