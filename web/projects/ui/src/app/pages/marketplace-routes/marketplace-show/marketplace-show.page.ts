import { ChangeDetectionStrategy, Component } from '@angular/core'
import { ActivatedRoute } from '@angular/router'
import { getPkgId } from '@start9labs/shared'
import {
  AbstractMarketplaceService,
  MarketplacePkg,
} from '@start9labs/marketplace'
import { PatchDB } from 'patch-db-client'
import { BehaviorSubject, Observable } from 'rxjs'
import { filter, shareReplay, switchMap } from 'rxjs/operators'
import { DataModel } from 'src/app/services/patch-db/data-model'

@Component({
  selector: 'marketplace-show',
  templateUrl: './marketplace-show.page.html',
  styleUrls: ['./marketplace-show.page.scss'],
  changeDetection: ChangeDetectionStrategy.OnPush,
})
export class MarketplaceShowPage {
  readonly pkgId = getPkgId(this.route)
  readonly url = this.route.snapshot.queryParamMap.get('url') || undefined

  readonly loadVersion$ = new BehaviorSubject<string>('*')

  readonly localPkg$ = this.patch
    .watch$('packageData', this.pkgId)
    .pipe(filter(Boolean), shareReplay({ bufferSize: 1, refCount: true }))

  // TODO don't load new package, use otherVersion data
  readonly pkg$: Observable<MarketplacePkg> = this.loadVersion$.pipe(
    switchMap(version =>
      this.marketplaceService.getPackage$(
        {
          id: this.pkgId,
          version,
          otherVersions: 'full',
          sourceVersion: null,
        },
        this.url,
      ),
    ),
  )

  constructor(
    private readonly route: ActivatedRoute,
    private readonly patch: PatchDB<DataModel>,
    private readonly marketplaceService: AbstractMarketplaceService,
  ) {}
}
