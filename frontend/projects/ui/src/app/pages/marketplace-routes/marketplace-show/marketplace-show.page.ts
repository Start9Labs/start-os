import { ChangeDetectionStrategy, Component } from '@angular/core'
import { ActivatedRoute } from '@angular/router'
import { getPkgId } from '@start9labs/shared'
import { AbstractMarketplaceService } from '@start9labs/marketplace'
import { PatchDB } from 'patch-db-client'
import { BehaviorSubject } from 'rxjs'
import { filter, shareReplay, switchMap } from 'rxjs/operators'
import { DataModel } from 'src/app/services/patch-db/data-model'

@Component({
  selector: 'marketplace-show',
  templateUrl: './marketplace-show.page.html',
  styleUrls: ['./marketplace-show.page.scss'],
  changeDetection: ChangeDetectionStrategy.OnPush,
})
export class MarketplaceShowPage {
  private readonly pkgId = getPkgId(this.route)
  readonly url = this.route.snapshot.queryParamMap.get('url') || undefined

  readonly loadVersion$ = new BehaviorSubject<string>('*')

  readonly localPkg$ = this.patch
    .watch$('package-data', this.pkgId)
    .pipe(filter(Boolean), shareReplay({ bufferSize: 1, refCount: true }))

  readonly pkg$ = this.loadVersion$.pipe(
    switchMap(version =>
      this.marketplaceService.getPackage(this.pkgId, version, this.url),
    ),
  )

  constructor(
    private readonly route: ActivatedRoute,
    private readonly patch: PatchDB<DataModel>,
    private readonly marketplaceService: AbstractMarketplaceService,
  ) {}
}
