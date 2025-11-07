import { inject, Injectable } from '@angular/core'
import { PatchDB } from 'patch-db-client'
import { combineLatest, EMPTY, map, Observable, shareReplay } from 'rxjs'
import { LocalPackagesService } from 'src/app/services/local-packages.service'
import { MarketplaceService } from 'src/app/services/marketplace.service'
import { NotificationService } from 'src/app/services/notification.service'
import { OSService } from 'src/app/services/os.service'
import { DataModel } from 'src/app/services/patch-db/data-model'
import { FilterUpdatesPipe } from '../routes/portal/routes/updates/filter-updates.pipe'

@Injectable({
  providedIn: 'root',
})
export class BadgeService {
  private readonly notifications = inject(NotificationService)
  private readonly patch = inject<PatchDB<DataModel>>(PatchDB)
  private readonly system$ = inject(OSService).updateAvailable$.pipe(
    map(Number),
  )
  private readonly metrics$ = this.patch
    .watch$('serverInfo', 'ntpSynced')
    .pipe(map(synced => Number(!synced)))
  private readonly marketplaceService = inject(MarketplaceService)
  private readonly filterUpdatesPipe = inject(FilterUpdatesPipe)

  private readonly updates$ = combineLatest([
    this.marketplaceService.marketplace$,
    inject(LocalPackagesService),
  ]).pipe(
    map(
      ([marketplace, local]) =>
        Object.entries(marketplace).reduce(
          (list, [_, store]) =>
            this.filterUpdatesPipe
              .transform(store?.packages || [], local)
              .reduce((result, { id }) => result.add(id), list),
          new Set<string>(),
        ).size,
    ),
    shareReplay(1),
  )

  getCount(id: string): Observable<number> {
    switch (id) {
      case 'updates':
        return this.updates$
      case 'system':
        return this.system$
      case 'metrics':
        return this.metrics$
      case 'notifications':
        return this.notifications.unreadCount$
      default:
        return EMPTY
    }
  }
}
