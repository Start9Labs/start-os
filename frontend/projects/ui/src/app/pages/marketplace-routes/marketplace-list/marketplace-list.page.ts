import { ChangeDetectionStrategy, Component, Inject } from '@angular/core'
import { AbstractMarketplaceService } from '@start9labs/marketplace'
import { PatchDB } from 'patch-db-client'
import { combineLatest, map } from 'rxjs'
import { MarketplaceService } from 'src/app/services/marketplace.service'
import { DataModel } from 'src/app/services/patch-db/data-model'

@Component({
  selector: 'marketplace-list',
  templateUrl: 'marketplace-list.page.html',
  styleUrls: ['./marketplace-list.page.scss'],
  changeDetection: ChangeDetectionStrategy.OnPush,
})
export class MarketplaceListPage {
  private readonly pkgs$ = this.marketplaceService.getPackages$()

  private readonly categories$ = this.marketplaceService
    .getMarketplaceInfo$()
    .pipe(
      map(({ categories }) => {
        const set = new Set<string>()
        if (categories.includes('featured')) set.add('featured')
        set.add('updates')
        categories.forEach(c => set.add(c))
        set.add('all')
        return set
      }),
    )

  readonly marketplace$ = combineLatest([this.pkgs$, this.categories$]).pipe(
    map(arr => {
      return { pkgs: arr[0], categories: arr[1] }
    }),
  )

  readonly localPkgs$ = this.patch.watch$('package-data')

  readonly details$ = this.marketplaceService.getUiMarketplace$().pipe(
    map(({ url, name }) => {
      let color: string
      let description: string
      switch (url) {
        case 'https://registry.start9.com/':
          color = 'success'
          description =
            'Services in this marketplace are packaged and maintained by the Start9 team. If you experience an issue or have a questions related to a service in this marketplace, one of our dedicated support staff will be happy to assist you.'
          break
        case 'https://beta-registry-0-3.start9labs.com/':
          color = 'primary'
          description =
            'Services in this marketplace are undergoing active testing and may contain bugs. <b>Install at your own risk</b>. If you discover a bug or have a suggestion for improvement, please report it to the Start9 team in our community testing channel on Matrix.'
          break
        case 'https://community.start9labs.com/':
          color = 'tertiary'
          description =
            'Services in this marketplace are packaged and maintained by members of the Start9 community. <b>Install at your own risk</b>. If you experience an issue or have a question related to a service in this marketplace, please reach out to the package developer for assistance.'
          break
        default:
          // alt marketplace
          color = 'warning'
          description =
            'Warning. This is an <b>Alternative</b> Marketplace. Start9 cannot verify the integrity or functionality of services in this marketplace, and they may cause harm to your system. <b>Install at your own risk</b>.'
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
  ) {}

  category = 'featured'
  query = ''

  onCategoryChange(category: string): void {
    this.category = category
    this.query = ''
  }
}
