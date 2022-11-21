import { ChangeDetectionStrategy, Component, Inject } from '@angular/core'
import { EOSService } from '../../services/eos.service'
import { PatchDB } from 'patch-db-client'
import { combineLatest, map, Observable, of, startWith } from 'rxjs'
import { AbstractMarketplaceService } from '@start9labs/marketplace'
import { MarketplaceService } from 'src/app/services/marketplace.service'
import { DataModel } from 'src/app/services/patch-db/data-model'
import { SplitPaneTracker } from 'src/app/services/split-pane.service'
import { Emver } from '@start9labs/shared'
import { marketplaceSame, versionLower } from '../../pages/updates/updates.page'

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
      title: 'System Settings',
      url: '/settings',
      icon: 'settings-outline',
    },
  ]

  readonly notificationCount$ = this.patch.watch$(
    'server-info',
    'unread-notification-count',
  )

  readonly snekScore$ = this.patch.watch$('ui', 'gaming', 'snake', 'high-score')

  readonly showEOSUpdate$ = this.eosService.showUpdate$

  readonly updateCount$: Observable<number> = combineLatest([
    this.marketplaceService.getMarketplace$(),
    this.patch.watch$('package-data'),
  ]).pipe(
    map(([marketplace, local]) =>
      Object.entries(marketplace).reduce(
        (length, [url, store]) =>
          length +
          (store?.packages.filter(
            ({ manifest }) =>
              marketplaceSame(manifest, local, url) &&
              versionLower(manifest, local, this.emver),
          ).length || 0),
        0,
      ),
    ),
    startWith(0),
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
