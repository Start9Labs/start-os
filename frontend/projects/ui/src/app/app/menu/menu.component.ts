import {
  ChangeDetectionStrategy,
  Component,
  inject,
  Inject,
} from '@angular/core'
import { EOSService } from '../../services/eos.service'
import { PatchDB } from 'patch-db-client'
import {
  combineLatest,
  filter,
  map,
  Observable,
  startWith,
  switchMap,
  withLatestFrom,
} from 'rxjs'
import { AbstractMarketplaceService } from '@start9labs/marketplace'
import { MarketplaceService } from 'src/app/services/marketplace.service'
import { DataModel } from 'src/app/services/patch-db/data-model'
import { SplitPaneTracker } from 'src/app/services/split-pane.service'
import { Emver, THEME } from '@start9labs/shared'
import { ConnectionService } from 'src/app/services/connection.service'

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

  private readonly local$ = this.connectionService.connected$.pipe(
    filter(Boolean),
    withLatestFrom(this.patch.watch$('package-data')),
    switchMap(([_, outer]) =>
      this.patch.watch$('package-data').pipe(
        filter(
          pkgs =>
            !!Object.values(pkgs).find(
              pkg => pkg['install-progress']?.['unpack-complete'],
            ),
        ),
        startWith(outer),
      ),
    ),
  )

  readonly updateCount$: Observable<number> = combineLatest([
    this.marketplaceService.getMarketplace$(true),
    this.local$,
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

  readonly theme$ = inject(THEME)

  constructor(
    private readonly patch: PatchDB<DataModel>,
    private readonly eosService: EOSService,
    @Inject(AbstractMarketplaceService)
    private readonly marketplaceService: MarketplaceService,
    private readonly splitPane: SplitPaneTracker,
    private readonly emver: Emver,
    private readonly connectionService: ConnectionService,
  ) {}
}
