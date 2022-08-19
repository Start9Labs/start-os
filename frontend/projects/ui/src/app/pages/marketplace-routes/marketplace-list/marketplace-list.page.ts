import { Component } from '@angular/core'
import { Observable } from 'rxjs'
import { filter, first, map, switchMap } from 'rxjs/operators'
import { exists, isEmptyObject } from '@start9labs/shared'
import {
  AbstractMarketplaceService,
  MarketplacePkg,
} from '@start9labs/marketplace'
import { PatchDbService } from 'src/app/services/patch-db/patch-db.service'
import { PackageDataEntry } from 'src/app/services/patch-db/data-model'
import { ConnectionService } from 'src/app/services/connection.service'

@Component({
  selector: 'marketplace-list',
  templateUrl: './marketplace-list.page.html',
})
export class MarketplaceListPage {
  connected$ = this.connectionService.connected$

  readonly localPkgs$: Observable<Record<string, PackageDataEntry>> = this.patch
    .watch$('package-data')
    .pipe(filter(data => exists(data) && !isEmptyObject(data)))

  readonly categories$ = this.marketplaceService.getCategories()

  readonly pkgs$: Observable<MarketplacePkg[]> = this.patch
    .watch$('server-info')
    .pipe(
      filter(data => exists(data) && !isEmptyObject(data)),
      first(),
      switchMap(() => this.marketplaceService.getPackages()),
    )

  readonly name$: Observable<string> = this.marketplaceService
    .getMarketplace()
    .pipe(map(({ name }) => name))

  constructor(
    private readonly patch: PatchDbService,
    private readonly marketplaceService: AbstractMarketplaceService,
    private readonly connectionService: ConnectionService,
  ) {}
}
