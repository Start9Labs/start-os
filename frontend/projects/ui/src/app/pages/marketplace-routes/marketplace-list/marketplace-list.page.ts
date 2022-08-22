import { ChangeDetectionStrategy, Component } from '@angular/core'
import { map } from 'rxjs/operators'
import { AbstractMarketplaceService } from '@start9labs/marketplace'
import { PatchDbService } from 'src/app/services/patch-db/patch-db.service'
import { ConnectionService } from 'src/app/services/connection.service'

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

  readonly name$ = this.marketplaceService
    .getMarketplace()
    .pipe(map(({ name }) => name))

  constructor(
    private readonly patch: PatchDbService,
    private readonly marketplaceService: AbstractMarketplaceService,
    private readonly connectionService: ConnectionService,
  ) {}
}
