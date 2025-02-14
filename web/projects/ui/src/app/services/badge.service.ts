import { inject, Injectable } from '@angular/core'
import { Exver } from '@start9labs/shared'
import { PatchDB } from 'patch-db-client'
import {
  combineLatest,
  EMPTY,
  filter,
  first,
  map,
  Observable,
  pairwise,
  startWith,
  switchMap,
} from 'rxjs'
import { EOSService } from 'src/app/services/eos.service'
import { NotificationService } from 'src/app/services/notification.service'
import { DataModel } from 'src/app/services/patch-db/data-model'
import { MarketplaceService } from 'src/app/services/marketplace.service'
import { ConnectionService } from 'src/app/services/connection.service'
import { getManifest } from 'src/app/utils/get-package-data'

@Injectable({
  providedIn: 'root',
})
export class BadgeService {
  private readonly notifications = inject(NotificationService)
  private readonly exver = inject(Exver)
  private readonly patch = inject<PatchDB<DataModel>>(PatchDB)
  private readonly settings$ = combineLatest([
    this.patch.watch$('serverInfo', 'ntpSynced'),
    inject(EOSService).updateAvailable$,
  ]).pipe(map(([synced, update]) => Number(!synced) + Number(update)))
  private readonly marketplaceService = inject(MarketplaceService)

  private readonly local$ = inject(ConnectionService).pipe(
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

  // private readonly updates$ = combineLatest([
  //   this.marketplaceService.getMarketplace$(true),
  //   this.local$,
  // ]).pipe(
  //   map(
  //     ([marketplace, local]) =>
  //       Object.entries(marketplace).reduce(
  //         (list, [_, store]) =>
  //           store?.packages.reduce(
  //             (result, { id, version }) =>
  //               local[id] &&
  //               this.exver.compareExver(
  //                 version,
  //                 getManifest(local[id]).version,
  //               ) === 1
  //                 ? result.add(id)
  //                 : result,
  //             list,
  //           ) || list,
  //         new Set<string>(),
  //       ).size,
  //   ),
  //   shareReplay(1),
  // )

  getCount(id: string): Observable<number> {
    switch (id) {
      // case '/portal/updates':
      //   return this.updates$
      case '/portal/settings':
        return this.settings$
      case '/portal/notifications':
        return this.notifications.unreadCount$
      default:
        return EMPTY
    }
  }
}
