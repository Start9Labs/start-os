import { CommonModule } from '@angular/common'
import { ChangeDetectionStrategy, Component, inject } from '@angular/core'
import {
  AbstractMarketplaceService,
  StoreIconComponentModule,
} from '@start9labs/marketplace'
import { TuiForModule } from '@taiga-ui/cdk'
import { PatchDB } from 'patch-db-client'
import { combineLatest } from 'rxjs'
import { MarketplaceService } from 'src/app/services/marketplace.service'
import { DataModel } from 'src/app/services/patch-db/data-model'
import { ConfigService } from 'src/app/services/config.service'
import { FilterUpdatesPipe } from './pipes/filter-updates.pipe'
import { UpdatesItemComponent } from './components/item.component'
import { SkeletonListComponent } from '../../../components/skeleton-list.component'

@Component({
  template: `
    <ng-container *ngIf="data$ | async as data">
      <section *ngFor="let host of data.hosts">
        <h3 class="g-title">
          <store-icon
            [url]="host.url"
            [marketplace]="config.marketplace"
            size="26px"
          ></store-icon>
          {{ host.name }}
        </h3>
        <p
          *ngIf="data.errors.includes(host.url)"
          [style.color]="'var(--tui-negative)'"
        >
          Request Failed
        </p>
        <updates-item
          *ngFor="
            let pkg of data.mp[host.url]?.packages | filterUpdates : data.local;
            else: loading;
            empty: blank
          "
          [pkg]="pkg"
          [local]="data.local[pkg.manifest.id]"
          [url]="host.url"
        ></updates-item>
      </section>
    </ng-container>
    <ng-template #blank><p>All services are up to date!</p></ng-template>
    <ng-template #loading>
      <skeleton-list [showAvatar]="true"></skeleton-list>
    </ng-template>
  `,
  host: { class: 'g-page' },
  changeDetection: ChangeDetectionStrategy.OnPush,
  standalone: true,
  imports: [
    CommonModule,
    TuiForModule,
    StoreIconComponentModule,
    FilterUpdatesPipe,
    UpdatesItemComponent,
    SkeletonListComponent,
  ],
})
export class UpdatesComponent {
  private readonly marketplace = inject(
    AbstractMarketplaceService,
  ) as MarketplaceService

  readonly config = inject(ConfigService)

  readonly data$ = combineLatest({
    hosts: this.marketplace.getKnownHosts$(true),
    mp: this.marketplace.getMarketplace$(),
    local: inject(PatchDB<DataModel>).watch$('package-data'),
    errors: this.marketplace.getRequestErrors$(),
  })
}
