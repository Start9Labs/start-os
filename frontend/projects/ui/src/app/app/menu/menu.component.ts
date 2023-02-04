import { ChangeDetectionStrategy, Component, Inject } from '@angular/core'
import { EOSService } from '../../services/eos.service'
import { PatchDB } from 'patch-db-client'
import { combineLatest, map, Observable, of, startWith } from 'rxjs'
import { AbstractMarketplaceService } from '@start9labs/marketplace'
import { MarketplaceService } from 'src/app/services/marketplace.service'
import { DataModel } from 'src/app/services/patch-db/data-model'
import { SplitPaneTracker } from 'src/app/services/split-pane.service'
import { Emver, sameDomain } from '@start9labs/shared'
import { versionLower } from '../../pages/updates/updates.page'
import { ClientStorageService } from 'src/app/services/client-storage.service'

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
    this.clientStorageService.showDevTools$,
    this.marketplaceService.getMarketplace$(),
    this.patch.watch$('package-data'),
  ]).pipe(
    map(([devMode, marketplace, local]) =>
      Object.entries(marketplace).reduce((length, [url, store]) => {
        // If not dev mode, exclude alpha and beta
        if (!devMode && (url.includes('alpha') || url.includes('beta'))) {
          return length
        }
        // otherwise
        return (
          length +
          (store?.packages.filter(({ manifest }) => {
            const localUri = local[manifest.id]?.installed?.['marketplace-url']
            return (
              sameDomain(localUri, url) &&
              versionLower(manifest, local, this.emver)
            )
          }).length || 0)
        )
      }, 0),
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
    private readonly clientStorageService: ClientStorageService,
  ) {}
}
