import { ChangeDetectionStrategy, Component, Inject } from '@angular/core'
import { ActivatedRoute } from '@angular/router'
import { AbstractMarketplaceService } from '@start9labs/marketplace'
import { TuiDialogService } from '@taiga-ui/core'
import { PolymorpheusComponent } from '@tinkoff/ng-polymorpheus'
import { PatchDB } from 'patch-db-client'
import { map } from 'rxjs'
import { MarketplaceSettingsPage } from './marketplace-settings/marketplace-settings.page'
import { ConfigService } from 'src/app/services/config.service'
import { MarketplaceService } from 'src/app/services/marketplace.service'
import { DataModel } from 'src/app/services/patch-db/data-model'

@Component({
  selector: 'marketplace-list',
  templateUrl: 'marketplace-list.page.html',
  styleUrls: ['./marketplace-list.page.scss'],
  changeDetection: ChangeDetectionStrategy.OnPush,
})
export class MarketplaceListPage {
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

  readonly details$ = this.marketplaceService.getSelectedHost$().pipe(
    map(({ url, name }) => {
      const { start9, community } = this.config.marketplace
      let color: string
      let description: string

      if (url === start9) {
        color = 'success'
        description =
          'Services from this registry are packaged and maintained by the Start9 team. If you experience an issue or have a question related to a service from this registry, one of our dedicated support staff will be happy to assist you.'
      } else if (url === community) {
        color = 'tertiary'
        description =
          'Services from this registry are packaged and maintained by members of the Start9 community. <b>Install at your own risk</b>. If you experience an issue or have a question related to a service in this marketplace, please reach out to the package developer for assistance.'
      } else if (url.includes('beta')) {
        color = 'warning'
        description =
          'Services from this registry are undergoing <b>beta</b> testing and may contain bugs. <b>Install at your own risk</b>.'
      } else if (url.includes('alpha')) {
        color = 'danger'
        description =
          'Services from this registry are undergoing <b>alpha</b> testing. They are expected to contain bugs and could damage your system. <b>Install at your own risk</b>.'
      } else {
        // alt marketplace
        color = 'warning'
        description =
          'This is a Custom Registry. Start9 cannot verify the integrity or functionality of services from this registry, and they could damage your system. <b>Install at your own risk</b>.'
      }

      return {
        name,
        url,
        color,
        description,
      }
    }),
  )

  constructor(
    private readonly patch: PatchDB<DataModel>,
    @Inject(AbstractMarketplaceService)
    private readonly marketplaceService: MarketplaceService,
    private readonly dialogs: TuiDialogService,
    private readonly config: ConfigService,
    private readonly route: ActivatedRoute,
  ) {}

  category = 'featured'
  query = ''

  presentModalMarketplaceSettings() {
    this.dialogs
      .open(new PolymorpheusComponent(MarketplaceSettingsPage), {
        label: 'Change Registry',
      })
      .subscribe()
  }

  onCategoryChange(category: string): void {
    this.category = category
    this.query = ''
  }
}
