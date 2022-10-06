import { ChangeDetectionStrategy, Component, Inject } from '@angular/core'
import { EOSService } from '../../services/eos.service'
import { PatchDB } from 'patch-db-client'
import { iif, Observable } from 'rxjs'
import { filter, map, switchMap } from 'rxjs/operators'
import { AbstractMarketplaceService } from '@start9labs/marketplace'
import { MarketplaceService } from 'src/app/services/marketplace.service'
import { DataModel } from 'src/app/services/patch-db/data-model'
import { SplitPaneTracker } from 'src/app/services/split-pane.service'

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
      title: 'Embassy',
      url: '/embassy',
      icon: 'cube-outline',
    },
    {
      title: 'Marketplace',
      url: '/marketplace',
      icon: 'storefront-outline',
    },
    {
      title: 'Notifications',
      url: '/notifications',
      icon: 'notifications-outline',
    },
  ]

  readonly notificationCount$ = this.patch.watch$(
    'server-info',
    'unread-notification-count',
  )

  readonly snekScore$ = this.patch.watch$('ui', 'gaming', 'snake', 'high-score')

  readonly showEOSUpdate$ = this.eosService.showUpdate$

  readonly updateCount$: Observable<number> = this.patch
    .watch$('ui', 'auto-check-updates')
    .pipe(
      filter(Boolean),
      switchMap(() =>
        this.marketplaceService.getUpdates$().pipe(
          map(arr => {
            return arr.reduce(
              (acc, marketplace) => acc + marketplace.pkgs.length,
              0,
            )
          }),
        ),
      ),
    )

  readonly sidebarOpen$ = this.splitPane.sidebarOpen$

  constructor(
    private readonly patch: PatchDB<DataModel>,
    private readonly eosService: EOSService,
    @Inject(AbstractMarketplaceService)
    private readonly marketplaceService: MarketplaceService,
    private readonly splitPane: SplitPaneTracker,
  ) {}
}
