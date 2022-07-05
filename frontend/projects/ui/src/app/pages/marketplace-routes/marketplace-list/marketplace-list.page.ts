import { Component } from '@angular/core'
import { Observable } from 'rxjs'
import { filter, first, map, startWith, switchMapTo } from 'rxjs/operators'
import { exists, isEmptyObject } from '@start9labs/shared'
import {
  AbstractMarketplaceService,
  MarketplacePkg,
} from '@start9labs/marketplace'

import { PatchDbService } from 'src/app/services/patch-db/patch-db.service'
import { PackageDataEntry } from 'src/app/services/patch-db/data-model'

@Component({
  selector: 'marketplace-list',
  templateUrl: './marketplace-list.page.html',
})
export class MarketplaceListPage {
  readonly connected$ = this.patch.connected$

  readonly localPkgs$: Observable<Record<string, PackageDataEntry>> = this.patch
    .watch$('package-data')
    .pipe(
      filter(data => exists(data) && !isEmptyObject(data)),
      startWith({}),
    )

  readonly categories$ = this.marketplaceService.getCategories()

  readonly pkgs$: Observable<MarketplacePkg[]> = this.patch
    .watch$('server-info')
    .pipe(
      filter(data => exists(data) && !isEmptyObject(data)),
      first(),
      switchMapTo(this.marketplaceService.getPackages()),
    )

  readonly name$: Observable<string> = this.marketplaceService
    .getMarketplace()
    .pipe(map(({ name }) => name))

  constructor(
    private readonly patch: PatchDbService,
    private readonly marketplaceService: AbstractMarketplaceService,
  ) {}
}
