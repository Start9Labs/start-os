import { ChangeDetectionStrategy, Component, Inject } from '@angular/core'
import { EOSService } from '../../services/eos.service'
import { PatchDB } from 'patch-db-client'
import { combineLatest, first, map, Observable } from 'rxjs'
import { AbstractMarketplaceService } from '@start9labs/marketplace'
import { MarketplaceService } from 'src/app/services/marketplace.service'
import { DataModel } from 'src/app/services/patch-db/data-model'
import { SplitPaneTracker } from 'src/app/services/split-pane.service'
import { Emver } from '@start9labs/shared'

@Component({
  selector: 'app-menu',
  templateUrl: 'menu.component.html',
  styleUrls: ['menu.component.scss'],
  changeDetection: ChangeDetectionStrategy.OnPush,
})
export class MenuComponent {
  readonly pages = [
    {
      title: 'Services',
      url: '/services',
      icon: 'grid-outline',
    },
    {
      title: 'Marketplace',
      url: '/marketplace',
      icon: 'storefront-outline',
    },
    {
      title: 'Updates',
      url: '/updates',
      icon: 'globe-outline',
    },
    {
      title: 'Notifications',
      url: '/notifications',
      icon: 'notifications-outline',
    },
    {
      title: 'System',
      url: '/system',
      icon: 'construct-outline',
    },
  ]

  readonly notificationCount$ = this.patch.watch$(
    'server-info',
    'unread-notification-count',
  )

  readonly snekScore$ = this.patch.watch$('ui', 'gaming', 'snake', 'high-score')

  readonly showEOSUpdate$ = this.eosService.showUpdate$

  readonly updateCount$: Observable<number> = combineLatest([
    this.marketplaceService.getMarketplace$(true),
    this.patch.watch$('package-data').pipe(first()),
  ]).pipe(
    map(([marketplace, local]) =>
      Object.entries(marketplace).reduce((list, [_, store]) => {
        store?.packages.forEach(({ manifest: { id, version } }) => {
          if (
            this.emver.compare(
              version,
              local[id]?.installed?.manifest.version || '',
            ) === 1
          )
            list.add(id)
        })
        return list
      }, new Set<string>()),
    ),
    map(list => list.size),
  )

  readonly sidebarOpen$ = this.splitPane.sidebarOpen$

  constructor(
    private readonly patch: PatchDB<DataModel>,
    private readonly eosService: EOSService,
    @Inject(AbstractMarketplaceService)
    private readonly marketplaceService: MarketplaceService,
    private readonly splitPane: SplitPaneTracker,
    private readonly emver: Emver,
  ) {}
}
