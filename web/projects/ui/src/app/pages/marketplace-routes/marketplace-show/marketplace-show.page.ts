import { ChangeDetectionStrategy, Component } from '@angular/core'
import { ActivatedRoute, Router } from '@angular/router'
import { Exver, getPkgId } from '@start9labs/shared'
import {
  AbstractMarketplaceService,
  MarketplacePkg,
} from '@start9labs/marketplace'
import { PatchDB } from 'patch-db-client'
import { combineLatest, Observable } from 'rxjs'
import { filter, map, shareReplay, startWith, switchMap } from 'rxjs/operators'
import { DataModel } from 'src/app/services/patch-db/data-model'
import { getManifest } from 'src/app/util/get-package-data'

@Component({
  selector: 'marketplace-show',
  templateUrl: './marketplace-show.page.html',
  styleUrls: ['./marketplace-show.page.scss'],
  changeDetection: ChangeDetectionStrategy.OnPush,
})
export class MarketplaceShowPage {
  readonly pkgId = getPkgId(this.route)
  readonly url = this.route.snapshot.queryParamMap.get('url') || undefined

  readonly localPkg$ = combineLatest([
    this.patch.watch$('packageData', this.pkgId).pipe(filter(Boolean)),
    this.route.queryParamMap,
  ]).pipe(
    map(([pkg, paramMap]) =>
      this.exver.getFlavor(getManifest(pkg).version) === paramMap.get('flavor')
        ? pkg
        : null,
    ),
    shareReplay({ bufferSize: 1, refCount: true }),
  )

  readonly localFlavor$ = this.localPkg$.pipe(
    map(pkg => !pkg),
    startWith(false),
  )

  readonly pkg$: Observable<MarketplacePkg> = this.route.queryParamMap.pipe(
    switchMap(paramMap =>
      this.marketplaceService.getPackage$(
        this.pkgId,
        paramMap.get('version'),
        paramMap.get('flavor'),
        this.url,
      ),
    ),
  )

  readonly flavors$ = this.route.queryParamMap.pipe(
    switchMap(paramMap =>
      this.marketplaceService.getSelectedStore$().pipe(
        map(s =>
          s.packages.filter(
            p => p.id === this.pkgId && p.flavor !== paramMap.get('flavor'),
          ),
        ),
        filter(p => p.length > 0),
      ),
    ),
  )

  constructor(
    private readonly route: ActivatedRoute,
    private readonly router: Router,
    private readonly patch: PatchDB<DataModel>,
    private readonly marketplaceService: AbstractMarketplaceService,
    private readonly exver: Exver,
  ) {}

  updateVersion(version: string) {
    this.router.navigate([], {
      relativeTo: this.route,
      queryParams: { version },
      queryParamsHandling: 'merge',
    })
  }
}
