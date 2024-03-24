import { CommonModule } from '@angular/common'
import { ChangeDetectionStrategy, Component, inject } from '@angular/core'
import {
  AbstractMarketplaceService,
  StoreIconComponentModule,
} from '@start9labs/marketplace'
import { TuiAvatarModule, TuiCellModule } from '@taiga-ui/experimental'
import { PatchDB } from 'patch-db-client'
import { combineLatest, map } from 'rxjs'
import { MarketplaceService } from 'src/app/services/marketplace.service'
import {
  DataModel,
  InstalledState,
  PackageDataEntry,
  UpdatingState,
} from 'src/app/services/patch-db/data-model'
import { ConfigService } from 'src/app/services/config.service'
import { FilterUpdatesPipe } from './pipes/filter-updates.pipe'
import { UpdatesItemComponent } from './components/item.component'
import { isInstalled, isUpdating } from 'src/app/util/get-package-data'

@Component({
  template: `
    @if (data$ | async; as data) {
      @for (host of data.hosts; track host) {
        <h3 class="g-title">
          <store-icon [url]="host.url" [marketplace]="mp" size="26px" />
          {{ host.name }}
        </h3>
        @if (data.errors.includes(host.url)) {
          <p class="g-error">Request Failed</p>
        }
        @if (data.mp[host.url]?.packages | filterUpdates: data.local; as pkgs) {
          @for (pkg of pkgs; track pkg) {
            <updates-item
              [marketplacePkg]="pkg"
              [localPkg]="data.local[pkg.manifest.id]"
              [url]="host.url"
            />
          } @empty {
            <p>All services are up to date!</p>
          }
        } @else {
          @for (i of [0, 1, 2]; track i) {
            <section tuiCell>
              <tui-avatar class="tui-skeleton" />
              <span class="tui-skeleton">Loading update item</span>
              <span class="tui-skeleton" [style.margin-left]="'auto'">
                Loading actions
              </span>
            </section>
          }
        }
      }
    }
  `,
  host: { class: 'g-page' },
  changeDetection: ChangeDetectionStrategy.OnPush,
  standalone: true,
  imports: [
    CommonModule,
    TuiCellModule,
    TuiAvatarModule,
    StoreIconComponentModule,
    FilterUpdatesPipe,
    UpdatesItemComponent,
  ],
})
export default class UpdatesComponent {
  private readonly service = inject(
    AbstractMarketplaceService,
  ) as MarketplaceService

  readonly mp = inject(ConfigService).marketplace
  readonly data$ = combineLatest({
    hosts: this.service.getKnownHosts$(true),
    mp: this.service.getMarketplace$(),
    local: inject(PatchDB<DataModel>)
      .watch$('packageData')
      .pipe(
        map(pkgs =>
          Object.values(pkgs).reduce(
            (acc, curr) => {
              if (isInstalled(curr) || isUpdating(curr)) return { ...acc, curr }
              return acc
            },
            {} as Record<
              string,
              PackageDataEntry<InstalledState | UpdatingState>
            >,
          ),
        ),
      ),
    errors: this.service.getRequestErrors$(),
  })
}
