import {
  ChangeDetectionStrategy,
  Component,
  Inject,
  inject,
} from '@angular/core'
import { ActivatedRoute } from '@angular/router'
import {
  AbstractCategoryService,
  AbstractMarketplaceService,
} from '@start9labs/marketplace'
import { TuiDialogService, TuiNotificationT } from '@taiga-ui/core'
import { PolymorpheusComponent } from '@tinkoff/ng-polymorpheus'
import { PatchDB } from 'patch-db-client'
import { filter, map, shareReplay, take, tap } from 'rxjs'
import { MarketplaceSettingsPage } from './marketplace-settings/marketplace-settings.page'
import { ConfigService } from 'src/app/services/config.service'
import { MarketplaceService } from 'src/app/services/marketplace.service'
import { DataModel } from 'src/app/services/patch-db/data-model'
import {
  animate,
  query,
  stagger,
  style,
  transition,
  trigger,
} from '@angular/animations'
import { CategoryService } from 'src/app/services/category.service'

@Component({
  selector: 'marketplace-list',
  templateUrl: 'marketplace-list.page.html',
  styleUrls: ['./marketplace-list.page.scss'],
  changeDetection: ChangeDetectionStrategy.OnPush,
  animations: [
    trigger('itemAnimation', [
      transition('* => *', [
        query(
          ':enter',
          [
            style({ opacity: 0, transform: 'translateY(-20px)' }),
            stagger('100ms', [
              animate(
                '300ms ease-in-out',
                style({ opacity: 1, transform: 'none' }),
              ),
            ]),
          ],
          { optional: true },
        ),
      ]),
    ]),
  ],
})
export class MarketplaceListPage {
  constructor(
    private readonly patch: PatchDB<DataModel>,
    @Inject(AbstractMarketplaceService)
    private readonly marketplaceService: MarketplaceService,
    @Inject(AbstractCategoryService)
    private readonly categoryService: CategoryService,
    private readonly dialogs: TuiDialogService,
    readonly config: ConfigService,
    private readonly route: ActivatedRoute,
  ) {}

  readonly back = !!this.route.snapshot.queryParamMap.get('back')

  readonly store$ = this.marketplaceService.getSelectedStore$().pipe(
    map(({ info, packages }) => {
      const categories = new Set<string>()
      if (info.categories.includes('featured')) categories.add('featured')
      info.categories.forEach(c => categories.add(c))
      categories.add('all')

      return { categories: Array.from(categories), packages }
    }),
  )
  readonly localPkgs$ = this.patch.watch$('package-data')
  readonly category$ = this.categoryService.getCategory$()
  readonly query$ = this.categoryService.getQuery$()
  readonly details$ = this.marketplaceService.getSelectedHost$().pipe(
    map(({ url, name }) => {
      const { start9, community } = this.config.marketplace
      let color: TuiNotificationT
      let description: string

      if (url === start9) {
        color = 'success'
        description =
          'Services from this registry are packaged and maintained by the Start9 team. If you experience an issue or have questions related to a service from this registry, one of our dedicated support staff will be happy to assist you.'
      } else if (url === community) {
        color = 'info'
        description =
          'Services from this registry are packaged and maintained by members of the Start9 community. Install at your own risk. If you experience an issue or have a question related to a service in this marketplace, please reach out to the package developer for assistance.'
      } else if (url.includes('beta')) {
        color = 'warning'
        description =
          'Services from this registry are undergoing <b>beta</b> testing and may contain bugs. Install at your own risk.'
      } else if (url.includes('alpha')) {
        color = 'error'
        description =
          'Services from this registry are undergoing <b>alpha</b> testing. They are expected to contain bugs and could damage your system. Install at your own risk.'
      } else {
        // alt marketplace
        color = 'warning'
        description =
          'This is a Custom Registry. Start9 cannot verify the integrity or functionality of services from this registry, and they could damage your system. Install at your own risk.'
      }

      return {
        name,
        url,
        color,
        description,
      }
    }),
  )
}
