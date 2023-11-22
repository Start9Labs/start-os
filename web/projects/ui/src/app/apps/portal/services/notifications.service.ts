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
  startWith,
  switchMap,
} from 'rxjs'
import { DataModel } from 'src/app/services/patch-db/data-model'
import { MarketplaceService } from 'src/app/services/marketplace.service'
import { ConnectionService } from 'src/app/services/connection.service'

@Injectable({
  providedIn: 'root',
})
export class NotificationsService {
  private readonly emver = inject(Emver)
  private readonly patch = inject(PatchDB<DataModel>)
  private readonly marketplace = inject(
    AbstractMarketplaceService,
  ) as MarketplaceService

  private readonly local$ = inject(ConnectionService).connected$.pipe(
    filter(Boolean),
    switchMap(() => this.patch.watch$('package-data').pipe(first())),
    switchMap(outer =>
      this.patch.watch$('package-data').pipe(
        pairwise(),
        filter(([prev, curr]) =>
          Object.values(prev).some(
            p =>
              !curr[p.manifest.id] ||
              (p['install-progress'] &&
                !curr[p.manifest.id]['install-progress']),
          ),
        ),
        map(([_, curr]) => curr),
        startWith(outer),
      ),
    ),
  )

  private readonly updateCount$ = combineLatest([
    this.marketplace.getMarketplace$(true),
    this.local$,
  ]).pipe(
    map(
      ([marketplace, local]) =>
        Object.entries(marketplace).reduce(
          (list, [_, store]) =>
            store?.packages.reduce(
              (result, { manifest: { id, version } }) =>
                this.emver.compare(version, local[id]?.manifest.version) === 1
                  ? result.add(id)
                  : result,
              list,
            ) || list,
          new Set<string>(),
        ).size,
    ),
  )

  private readonly notificationCount$ = this.patch.watch$(
    'server-info',
    'unreadNotifications',
    'count',
  )

  getNotificationCount(id: string): Observable<number> {
    switch (id) {
      case '/portal/system/updates':
        return this.updateCount$
      case 'notifications':
        return this.notificationCount$
      default:
        return EMPTY
    }
  }
}
