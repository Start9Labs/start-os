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
  first,
  map,
  Observable,
  pairwise,
  startWith,
  switchMap,
} from 'rxjs'
import { AbstractMarketplaceService } from '@start9labs/marketplace'
import { MarketplaceService } from 'src/app/services/marketplace.service'
import { DataModel, PackageState } from 'src/app/services/patch-db/data-model'
import { SplitPaneTracker } from 'src/app/services/split-pane.service'
import { Emver, THEME } from '@start9labs/shared'
import { ConnectionService } from 'src/app/services/connection.service'
import { getManifest } from 'src/app/util/get-package-data'

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
    'serverInfo',
    'unreadNotificationCount',
  )

  readonly snekScore$ = this.patch.watch$('ui', 'gaming', 'snake', 'highScore')

  readonly showEOSUpdate$ = this.eosService.showUpdate$

  private readonly local$ = this.connectionService.connected$.pipe(
    filter(Boolean),
    switchMap(() => this.patch.watch$('packageData').pipe(first())),
    switchMap(outer =>
      this.patch.watch$('packageData').pipe(
        pairwise(),
        filter(([prev, curr]) =>
          Object.values(prev).some(
            p =>
              [
                PackageState.Installing,
                PackageState.Updating,
                PackageState.Restoring,
              ].includes(p.stateInfo.state) &&
              [PackageState.Installed, PackageState.Removing].includes(
                curr[getManifest(p).id].stateInfo.state,
              ),
          ),
        ),
        map(([_, curr]) => curr),
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
            local[id] &&
            this.emver.compare(
              version,
              getManifest(local[id]).version || '',
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
