import { ChangeDetectionStrategy, Component, Inject } from '@angular/core'
import { map } from 'rxjs/operators'
import { PatchDB } from 'patch-db-client'
import { ConnectionService } from 'src/app/services/connection.service'
import { DataModel } from 'src/app/services/patch-db/data-model'
import { MarketplaceService } from 'src/app/services/marketplace.service'
import { AbstractMarketplaceService } from '@start9labs/marketplace'

@Component({
  selector: 'marketplace-list',
  templateUrl: './marketplace-list.page.html',
  changeDetection: ChangeDetectionStrategy.OnPush,
})
export class MarketplaceListPage {
  readonly connected$ = this.connectionService.connected$

  readonly localPkgs$ = this.patch.watch$('package-data')

  readonly categories$ = this.marketplaceService.getCategories()

  readonly pkgs$ = this.marketplaceService.getPackages()

  readonly details$ = this.marketplaceService.getMarketplace()

  constructor(
    private readonly patch: PatchDB<DataModel>,
    @Inject(AbstractMarketplaceService)
    private readonly marketplaceService: MarketplaceService,
    private readonly connectionService: ConnectionService,
  ) {}
}
