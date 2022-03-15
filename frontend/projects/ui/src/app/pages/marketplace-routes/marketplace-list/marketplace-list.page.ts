import { Component } from '@angular/core'
import { defer, Observable } from 'rxjs'
import { filter, first, map, startWith, switchMapTo, tap } from 'rxjs/operators'
import { exists, isEmptyObject } from '@start9labs/shared'
import {
  AbstractMarketplaceService,
  LocalPkg,
  MarketplacePkg,
  spreadProgress,
} from '@start9labs/marketplace'

import { PatchDbService } from 'src/app/services/patch-db/patch-db.service'

@Component({
  selector: 'marketplace-list',
  templateUrl: './marketplace-list.page.html',
})
export class MarketplaceListPage {
  readonly localPkgs$: Observable<Record<string, LocalPkg>> = defer(() =>
    this.patch.watch$('package-data'),
  ).pipe(
    filter(data => exists(data) && !isEmptyObject(data)),
    tap(pkgs => Object.values(pkgs).forEach(spreadProgress)),
    startWith({}),
  )

  readonly categories$ = this.marketplaceService
    .getCategories()
    .pipe(
      map(categories => new Set(['featured', 'updates', ...categories, 'all'])),
    )

  readonly pkgs$: Observable<MarketplacePkg[]> = defer(() =>
    this.patch.watch$('server-info'),
  ).pipe(
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

  get loaded(): boolean {
    return this.patch.loaded
  }
}
