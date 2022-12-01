import { ChangeDetectionStrategy, Component, Inject } from '@angular/core'
import { ActivatedRoute } from '@angular/router'
import { ModalController } from '@ionic/angular'
import { AbstractMarketplaceService } from '@start9labs/marketplace'
import { PatchDB } from 'patch-db-client'
import { filter, map } from 'rxjs'
import { MarketplaceSettingsPage } from 'src/app/modals/marketplace-settings/marketplace-settings.page'
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
    filter(Boolean),
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
      const { start9, community, beta } = this.config.marketplace
      let color: string
      let description: string
      switch (url) {
        case start9:
          color = 'success'
          description =
            'Services from this registry are packaged and maintained by the Start9 team. If you experience an issue or have a questions related to a service from this registry, one of our dedicated support staff will be happy to assist you.'
          break
        case community:
          color = 'tertiary'
          description =
            'Services from this registry are packaged and maintained by members of the Start9 community. <b>Install at your own risk</b>. If you experience an issue or have a question related to a service in this marketplace, please reach out to the package developer for assistance.'
          break
        case beta:
          color = 'primary'
          description =
            'Services from this registry are undergoing active testing and may contain bugs. <b>Install at your own risk</b>. If you discover a bug or have a suggestion for improvement, please report it to the Start9 team in our community testing channel on Matrix.'
          break
        default:
          // alt marketplace
          color = 'warning'
          description =
            'This is a Custom Registry. Start9 cannot verify the integrity or functionality of services from this registry, and they may cause harm to your system. Install at your own risk.'
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
    private readonly modalCtrl: ModalController,
    private readonly config: ConfigService,
    private readonly route: ActivatedRoute,
  ) {}

  category = 'featured'
  query = ''

  async presentModalMarketplaceSettings() {
    const modal = await this.modalCtrl.create({
      component: MarketplaceSettingsPage,
    })
    modal.onDidDismiss().then(res => {
      console.log(res)
    })
    await modal.present()
  }

  onCategoryChange(category: string): void {
    this.category = category
    this.query = ''
  }
}
