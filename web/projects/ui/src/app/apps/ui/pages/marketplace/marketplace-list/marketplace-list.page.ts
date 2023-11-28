import { ChangeDetectionStrategy, Component, Inject } from '@angular/core'
import {
  AbstractCategoryService,
  AbstractMarketplaceService,
} from '@start9labs/marketplace'
import { TuiDialogService } from '@taiga-ui/core'
import { PatchDB } from 'patch-db-client'
import { map } from 'rxjs'
import { ConfigService } from 'src/app/services/config.service'
import { MarketplaceService } from 'src/app/services/marketplace.service'
import { DataModel } from 'src/app/services/patch-db/data-model'
import { CategoryService } from 'src/app/services/category.service'
import { SidebarService } from 'src/app/services/sidebar.service'
import { MarketplaceSettingsPage } from './marketplace-settings/marketplace-settings.page'
import { PolymorpheusComponent } from '@tinkoff/ng-polymorpheus'

@Component({
  selector: 'marketplace-list',
  templateUrl: 'marketplace-list.page.html',
  styleUrls: ['./marketplace-list.page.scss'],
  changeDetection: ChangeDetectionStrategy.OnPush,
})
export class MarketplaceListPage {
  constructor(
    private readonly patch: PatchDB<DataModel>,
    @Inject(AbstractMarketplaceService)
    private readonly marketplaceService: MarketplaceService,
    @Inject(AbstractCategoryService)
    private readonly categoryService: CategoryService,
    @Inject(TuiDialogService) private readonly dialogs: TuiDialogService,
    readonly config: ConfigService,
    readonly sidebarService: SidebarService,
  ) {}

  readonly packages$ = this.marketplaceService.getSelectedStore$().pipe(
    map(({ packages }) => {
      this.sidebarService.setMap(packages.map(p => p.manifest.id))
      return packages
    }),
  )
  readonly localPkgs$ = this.patch.watch$('package-data')
  readonly category$ = this.categoryService.getCategory$()
  readonly query$ = this.categoryService.getQuery$()
  readonly details$ = this.marketplaceService.getSelectedHost$()
  readonly marketplace = this.config.marketplace

  async presentModalMarketplaceSettings() {
    this.dialogs
      .open<MarketplaceSettingsPage>(
        new PolymorpheusComponent(MarketplaceSettingsPage),
        {
          label: 'Change Registry',
        },
      )
      .subscribe()
  }
}
